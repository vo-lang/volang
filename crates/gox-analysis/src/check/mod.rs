//! Phase 3: Body Checking
//!
//! This module type-checks function bodies, expressions, and statements.
//! It is the third and final phase of the type checking pipeline.
//!
//! # Module Structure
//!
//! The checker is split into submodules for maintainability:
//! - [`expr`]: Expression type checking
//! - [`stmt`]: Statement type checking  
//! - [`builtin`]: Built-in function checking
//!
//! # Responsibilities
//!
//! ## Expression Checking
//! - **Literals**: Infers types for integer, float, string, and boolean literals
//! - **Identifiers**: Looks up variables, functions, and constants in scope
//! - **Binary operations**: Validates operand types for arithmetic, comparison, and logical ops
//! - **Unary operations**: Validates operand types for negation, not, bitwise complement, etc.
//! - **Function calls**: Checks argument count and types against function signature
//! - **Selectors**: Resolves field access and method calls on structs and interfaces
//! - **Index expressions**: Validates indexable types (arrays, slices, maps, strings)
//! - **Type assertions**: Validates interface type assertions `x.(T)`
//! - **Composite literals**: Checks struct, array, slice, and map literal elements
//!
//! ## Statement Checking
//! - **Assignments**: Validates LHS/RHS type compatibility
//! - **Short variable declarations**: Handles `:=` with type inference
//! - **Return statements**: Validates return values against function signature
//! - **Control flow**: Checks conditions in `if`, `for`, `switch` statements
//! - **Channel operations**: Validates send/receive on channel types
//!
//! ## Scope Management
//! - **Function scope**: Parameters and named return values
//! - **Block scope**: Variables declared in `if`, `for`, `switch` blocks
//! - **Shadowing**: Allows inner scopes to shadow outer declarations

mod builtin;
mod expr;
mod stmt;
#[cfg(test)]
mod tests;

use gox_common::{DiagnosticSink, Span, Symbol, SymbolInterner};
use gox_syntax::ast::{self, Expr};

use crate::errors::TypeError;
use crate::resolve::ResolveResult;
use crate::scope::{Entity, Scope, ScopeKind, VarEntity};
use crate::types::{BasicType, FuncType, NamedTypeInfo, Type, TypeRegistry};

/// Offset for local type IDs to distinguish from package-level types.
const LOCAL_TYPE_ID_OFFSET: u32 = 0x1000_0000;

/// Type checker for Phase 3.
pub struct TypeChecker<'a> {
    /// Symbol interner for resolving names.
    pub(crate) interner: &'a SymbolInterner,
    /// Diagnostics sink.
    pub(crate) diagnostics: &'a mut DiagnosticSink,
    /// Package-level scope from Phase 2 (for fallback lookups).
    pub(crate) package_scope: &'a Scope,
    /// Current local scope stack (owned, for function bodies).
    pub(crate) local_scope: Option<Scope>,
    /// Named type information from Phase 2.
    pub(crate) named_types: &'a [NamedTypeInfo],
    /// Type registry for method set lookups.
    pub(crate) registry: TypeRegistry<'a>,
    /// Expected return types for the current function (empty if not in a function).
    pub(crate) return_types: Vec<Type>,
    /// Whether the current function has named return values.
    pub(crate) has_named_returns: bool,
    /// Imported package names (for cross-package calls).
    pub(crate) imported_packages: std::collections::HashSet<String>,
    /// Imported package exports (pkg_name -> symbol_name -> type).
    pub(crate) package_exports:
        std::collections::HashMap<String, std::collections::HashMap<String, Type>>,
    /// Imported package exported types with methods (pkg_name -> type_name -> method_name -> type).
    pub(crate) package_exported_types: std::collections::HashMap<
        String,
        std::collections::HashMap<String, std::collections::HashMap<String, Type>>,
    >,
    /// Local type declarations (function-scoped types).
    pub(crate) local_types: Vec<NamedTypeInfo>,
    /// Expression types: (span_start, span_end) -> Type (for codegen to look up any expression's type)
    /// Uses full span as key to distinguish nested expressions with same start position.
    pub expr_types: std::collections::HashMap<(u32, u32), Type>,
}

impl<'a> TypeChecker<'a> {
    /// Creates a new type checker from Phase 2 results.
    pub fn new(
        resolve_result: &'a ResolveResult,
        interner: &'a SymbolInterner,
        diagnostics: &'a mut DiagnosticSink,
    ) -> Self {
        Self {
            interner,
            diagnostics,
            package_scope: &resolve_result.scope,
            local_scope: None,
            named_types: &resolve_result.named_types,
            registry: TypeRegistry::new(&resolve_result.named_types),
            return_types: Vec::new(),
            has_named_returns: false,
            imported_packages: std::collections::HashSet::new(),
            package_exports: std::collections::HashMap::new(),
            package_exported_types: std::collections::HashMap::new(),
            local_types: Vec::new(),
            expr_types: std::collections::HashMap::new(),
        }
    }

    /// Records the type of an expression for later use by codegen.
    /// Uses (start, end) as key to distinguish nested expressions (e.g., `m` vs `m[1]`).
    pub(crate) fn record_expr_type(&mut self, expr: &Expr, ty: Type) {
        self.expr_types
            .insert((expr.span.start.0, expr.span.end.0), ty);
    }

    /// Sets imported packages for cross-package call resolution.
    pub fn set_imported_packages(
        &mut self,
        imports: &[crate::project::ResolvedImport],
        exports: &std::collections::HashMap<
            String,
            std::collections::HashMap<String, crate::project::ExportedSymbol>,
        >,
        exported_types: &std::collections::HashMap<
            String,
            std::collections::HashMap<String, crate::project::ExportedType>,
        >,
    ) {
        for import in imports {
            if !import.package_name.is_empty() {
                let local_name = import.alias.as_ref().unwrap_or(&import.package_name);
                self.imported_packages.insert(local_name.clone());

                // Copy the exports for this package
                if let Some(pkg_exports) = exports.get(&import.package_name) {
                    let type_map: std::collections::HashMap<String, Type> = pkg_exports
                        .iter()
                        .map(|(name, sym)| (name.clone(), sym.ty.clone()))
                        .collect();
                    self.package_exports.insert(local_name.clone(), type_map);
                }

                // Copy the exported types with methods for this package
                if let Some(pkg_types) = exported_types.get(&import.package_name) {
                    let mut types_map: std::collections::HashMap<
                        String,
                        std::collections::HashMap<String, Type>,
                    > = std::collections::HashMap::new();
                    for (type_name, exported_type) in pkg_types {
                        types_map.insert(type_name.clone(), exported_type.methods.clone());
                    }
                    self.package_exported_types
                        .insert(local_name.clone(), types_map);
                }
            }
        }
    }

    // ========== Error reporting ==========

    /// Emits a type error diagnostic.
    pub(crate) fn error(&mut self, err: TypeError, span: Span) {
        self.diagnostics.emit(err.at(span));
    }

    /// Emits a type error diagnostic with a custom message.
    pub(crate) fn error_msg(&mut self, err: TypeError, span: Span, message: impl Into<String>) {
        self.diagnostics.emit(err.at_with_message(span, message));
    }

    // ========== Scope management ==========

    /// Looks up a symbol, checking local scope first, then package scope.
    pub(crate) fn lookup(&self, symbol: Symbol) -> Option<&Entity> {
        // First check local scope
        if let Some(ref local) = self.local_scope {
            if let Some(entity) = local.lookup(symbol) {
                return Some(entity);
            }
        }
        // Fall back to package scope
        self.package_scope.lookup(symbol)
    }

    /// Pushes a new local scope.
    pub(crate) fn push_scope(&mut self, kind: ScopeKind) {
        let parent = self.local_scope.take();
        self.local_scope = Some(Scope::new(parent, kind));
    }

    /// Pops the current local scope.
    pub(crate) fn pop_scope(&mut self) {
        if let Some(mut scope) = self.local_scope.take() {
            self.local_scope = scope.take_parent();
        }
    }

    /// Defines a variable in the current local scope.
    pub(crate) fn define_var(&mut self, symbol: Symbol, ty: Type, span: Span) {
        if let Some(ref mut scope) = self.local_scope {
            scope.insert(
                symbol,
                Entity::Var(VarEntity {
                    ty,
                    constant: None,
                    span,
                }),
            );
        }
    }

    /// Checks a function body with its parameters in scope.
    pub fn check_func_body(&mut self, func: &ast::FuncDecl) {
        // Push function scope for parameters
        self.push_scope(ScopeKind::Function);

        // Add receiver to scope if this is a method
        if let Some(ref receiver) = func.receiver {
            // Look up the receiver type
            if let Some(Entity::Type(t)) = self.package_scope.lookup(receiver.ty.symbol) {
                let base_ty = Type::Named(t.id);
                // Wrap in pointer if this is a pointer receiver
                let receiver_ty = if receiver.is_pointer {
                    Type::Pointer(Box::new(base_ty))
                } else {
                    base_ty
                };
                self.define_var(receiver.name.symbol, receiver_ty, receiver.name.span);
            }
        }

        // Add parameters to scope
        for param in &func.sig.params {
            let param_ty = self.resolve_type_expr(&param.ty);
            for name in &param.names {
                self.define_var(name.symbol, param_ty.clone(), name.span);
            }
        }

        // Set expected return types for this function
        let old_return_types = std::mem::take(&mut self.return_types);
        self.return_types = func
            .sig
            .results
            .iter()
            .map(|r| self.resolve_type_expr(&r.ty))
            .collect();

        // Add named return values to scope
        let old_has_named_returns = self.has_named_returns;
        self.has_named_returns = func.sig.results.iter().any(|r| r.name.is_some());
        for result in &func.sig.results {
            if let Some(ref name) = result.name {
                let result_ty = self.resolve_type_expr(&result.ty);
                self.define_var(name.symbol, result_ty, name.span);
            }
        }

        // Check function body
        if let Some(ref body) = func.body {
            self.check_block(body);
        }

        // Restore previous state
        self.return_types = old_return_types;
        self.has_named_returns = old_has_named_returns;

        // Pop function scope
        self.pop_scope();
    }

    /// Checks a package-level variable declaration.
    pub fn check_pkg_var_decl(&mut self, decl: &ast::VarDecl) {
        for spec in &decl.specs {
            // Determine the declared type (if any)
            let declared_ty = spec.ty.as_ref().map(|t| self.resolve_type_expr(t));

            // Check initializer values
            let value_types: Vec<Type> = spec.values.iter()
                .map(|v| self.check_expr(v))
                .collect();

            // Validate type compatibility
            for (i, name) in spec.names.iter().enumerate() {
                if let Some(ref ty) = declared_ty {
                    // Type is declared - check initializer compatibility
                    if i < value_types.len() && !self.is_assignable(&value_types[i], ty) {
                        self.error(TypeError::VarInitTypeMismatch, name.span);
                    }
                }
                // Note: we don't define vars here since they're already in package scope
            }
        }
    }
}

impl<'a> TypeChecker<'a> {
    /// Resolves a type expression to a Type.
    pub(crate) fn resolve_type_expr(&self, ty_expr: &ast::TypeExpr) -> Type {
        match &ty_expr.kind {
            ast::TypeExprKind::Ident(name) => {
                // Look up the type name - check local scope first
                if let Some(Entity::Type(t)) = self.lookup(name.symbol) {
                    return Type::Named(t.id);
                }
                // Check for basic types
                let name_str = self.interner.resolve(name.symbol).unwrap_or("");
                match name_str {
                    "int" => Type::Basic(BasicType::Int),
                    "int8" => Type::Basic(BasicType::Int8),
                    "int16" => Type::Basic(BasicType::Int16),
                    "int32" => Type::Basic(BasicType::Int32),
                    "int64" => Type::Basic(BasicType::Int64),
                    "uint" => Type::Basic(BasicType::Uint),
                    "uint8" => Type::Basic(BasicType::Uint8),
                    "uint16" => Type::Basic(BasicType::Uint16),
                    "uint32" => Type::Basic(BasicType::Uint32),
                    "uint64" => Type::Basic(BasicType::Uint64),
                    "float32" => Type::Basic(BasicType::Float32),
                    "float64" => Type::Basic(BasicType::Float64),
                    "bool" => Type::Basic(BasicType::Bool),
                    "string" => Type::Basic(BasicType::String),
                    "byte" => Type::Basic(BasicType::Uint8),
                    "rune" => Type::Basic(BasicType::Int32),
                    _ => Type::Invalid,
                }
            }
            ast::TypeExprKind::Slice(elem) => {
                let elem_ty = self.resolve_type_expr(elem);
                Type::Slice(crate::types::SliceType {
                    elem: Box::new(elem_ty),
                })
            }
            ast::TypeExprKind::Array(arr) => {
                let elem_ty = self.resolve_type_expr(&arr.elem);
                Type::Array(crate::types::ArrayType {
                    elem: Box::new(elem_ty),
                    len: 0, // Would need to evaluate the length expression
                })
            }
            ast::TypeExprKind::Map(m) => {
                let key_ty = self.resolve_type_expr(&m.key);
                let value_ty = self.resolve_type_expr(&m.value);
                Type::Map(crate::types::MapType {
                    key: Box::new(key_ty),
                    value: Box::new(value_ty),
                })
            }
            ast::TypeExprKind::Chan(c) => {
                let elem_ty = self.resolve_type_expr(&c.elem);
                let dir = match c.dir {
                    ast::ChanDir::Both => crate::types::ChanDir::Both,
                    ast::ChanDir::Send => crate::types::ChanDir::SendOnly,
                    ast::ChanDir::Recv => crate::types::ChanDir::RecvOnly,
                };
                Type::Chan(crate::types::ChanType {
                    elem: Box::new(elem_ty),
                    dir,
                })
            }
            ast::TypeExprKind::Interface(iface) => {
                // Resolve interface methods
                let methods = iface
                    .elems
                    .iter()
                    .filter_map(|elem| {
                        if let ast::InterfaceElem::Method(m) = elem {
                            let sig = self.resolve_func_sig(&m.sig);
                            Some(crate::types::Method {
                                name: m.name.symbol,
                                sig,
                                // Interface methods have no receiver
                                is_pointer_receiver: false,
                            })
                        } else {
                            None
                        }
                    })
                    .collect();
                Type::Interface(crate::types::InterfaceType {
                    methods,
                    embeds: vec![],
                })
            }
            ast::TypeExprKind::Struct(s) => {
                let fields = self.resolve_struct_fields(&s.fields);
                Type::Struct(crate::types::StructType { fields })
            }
            ast::TypeExprKind::Pointer(inner) => {
                let elem = self.resolve_type_expr(inner);
                Type::Pointer(Box::new(elem))
            }
            _ => Type::Invalid,
        }
    }

    /// Resolves struct fields for local type declarations.
    fn resolve_struct_fields(&self, ast_fields: &[ast::Field]) -> Vec<crate::types::Field> {
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
                fields.push(crate::types::Field {
                    name: embedded_name,
                    ty,
                    embedded: true,
                    tag,
                });
            } else {
                // Named fields
                for name in &ast_field.names {
                    fields.push(crate::types::Field {
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
            ast::TypeExprKind::Ident(name) => Some(name.symbol),
            _ => None,
        }
    }

    /// Resolves a function signature to a FuncType.
    fn resolve_func_sig(&self, sig: &ast::FuncSig) -> FuncType {
        let params: Vec<Type> = sig
            .params
            .iter()
            .flat_map(|p| {
                let ty = self.resolve_type_expr(&p.ty);
                // Repeat the type for each name (or once if no names)
                let count = if p.names.is_empty() { 1 } else { p.names.len() };
                std::iter::repeat_n(ty, count)
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
}

/// Type-checks all declarations in a resolve result.
/// This is the main entry point for Phase 3.
pub fn check_types<'a>(
    resolve_result: &'a ResolveResult,
    interner: &'a SymbolInterner,
    diagnostics: &'a mut DiagnosticSink,
) -> TypeChecker<'a> {
    TypeChecker::new(resolve_result, interner, diagnostics)
}
