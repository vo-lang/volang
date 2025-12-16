//! Phase 1: Type Collection
//!
//! This module collects all top-level declarations and builds the initial symbol table.
//! It is the first phase of the three-phase type checking pipeline.
//!
//! # Responsibilities
//!
//! - **Declaration registration**: Registers all top-level `var`, `const`, `type`, `func`,
//!   and `interface` declarations in the package scope
//! - **Duplicate detection**: Reports errors for redeclared identifiers
//! - **Constant evaluation**: Evaluates `iota` in const blocks and simple constant expressions
//! - **Placeholder creation**: Creates [`NamedTypePlaceholder`], [`VarTypePlaceholder`],
//!   [`FuncSigPlaceholder`], and [`MethodPlaceholder`] for Phase 2 resolution
//! - **Built-in population**: Populates the universe scope with built-in types and functions
//!
//! # Output
//!
//! Returns a [`CollectResult`] containing:
//! - The package-level [`Scope`] with all declarations
//! - Placeholders for types, variables, functions, and methods that need resolution
//!
//! # Example
//!
//! ```ignore
//! use gox_analysis::collect::TypeCollector;
//!
//! let mut diag = DiagnosticSink::new();
//! let collector = TypeCollector::new(&interner, &mut diag);
//! let result = collector.collect(&file);
//! // result.scope contains all declarations
//! // result.named_types contains type placeholders for Phase 2
//! ```

use gox_common::{DiagnosticSink, Label, Span, Symbol, SymbolInterner};
use gox_syntax::ast::{self, BinaryOp, Decl, File, UnaryOp};

use crate::constant::Constant;
use crate::errors::TypeError;
use crate::scope::{BuiltinKind, Entity, FuncEntity, Scope, ScopeKind, TypeEntity, VarEntity};
use crate::types::{BasicType, FuncType, NamedTypeId, Type, UntypedKind};

/// The result of Phase 1 type collection.
pub struct CollectResult {
    /// The package-level scope with all declarations.
    pub scope: Scope,
    /// Named type registry (id -> info placeholder).
    pub named_types: Vec<NamedTypePlaceholder>,
    /// Methods collected (receiver type name -> method declarations).
    pub methods: Vec<MethodPlaceholder>,
    /// Variables that need type resolution.
    pub var_types: Vec<VarTypePlaceholder>,
    /// Functions that need signature resolution.
    pub func_sigs: Vec<FuncSigPlaceholder>,
}

/// A placeholder for a variable's type (resolved in Phase 2).
#[derive(Debug)]
pub struct VarTypePlaceholder {
    /// The variable name.
    pub name: Symbol,
    /// The type expression (if specified).
    pub ty: Option<ast::TypeExpr>,
    /// The initializer expression (for type inference).
    pub init_expr: Option<ast::Expr>,
}

/// A placeholder for a function's signature (resolved in Phase 2).
#[derive(Debug)]
pub struct FuncSigPlaceholder {
    /// The function name.
    pub name: Symbol,
    /// The function signature AST.
    pub sig: ast::FuncSig,
}

/// A placeholder for a method declaration (resolved in Phase 2).
#[derive(Debug)]
pub struct MethodPlaceholder {
    /// The receiver type name.
    pub receiver_type: Symbol,
    /// The method name.
    pub name: Symbol,
    /// The function declaration AST.
    pub decl: ast::FuncDecl,
}

/// A placeholder for a named type (resolved in Phase 2).
#[derive(Debug)]
pub struct NamedTypePlaceholder {
    /// The type's name.
    pub name: Symbol,
    /// The AST type expression (to be resolved in Phase 2).
    pub ast_type: ast::TypeExpr,
    /// The declaration span.
    pub span: Span,
}

/// Type collector for Phase 1.
pub struct TypeCollector<'a> {
    /// Symbol interner for resolving names.
    interner: &'a SymbolInterner,
    /// Diagnostics sink.
    diagnostics: &'a mut DiagnosticSink,
    /// Package-level scope.
    scope: Scope,
    /// Named type registry.
    named_types: Vec<NamedTypePlaceholder>,
    /// Methods collected.
    methods: Vec<MethodPlaceholder>,
    /// Variables needing type resolution.
    var_types: Vec<VarTypePlaceholder>,
    /// Functions needing signature resolution.
    func_sigs: Vec<FuncSigPlaceholder>,
    /// Next named type ID.
    next_type_id: u32,
    /// Current iota value (for const blocks).
    iota: u64,
    /// Pre-interned symbols for built-in names.
    builtin_symbols: BuiltinSymbols,
}

/// Pre-interned symbols for built-in names.
struct BuiltinSymbols {
    // Types
    bool_sym: Symbol,
    int_sym: Symbol,
    int8_sym: Symbol,
    int16_sym: Symbol,
    int32_sym: Symbol,
    int64_sym: Symbol,
    uint_sym: Symbol,
    uint8_sym: Symbol,
    uint16_sym: Symbol,
    uint32_sym: Symbol,
    uint64_sym: Symbol,
    float32_sym: Symbol,
    float64_sym: Symbol,
    string_sym: Symbol,
    byte_sym: Symbol,
    rune_sym: Symbol,
    // Constants
    true_sym: Symbol,
    false_sym: Symbol,
    iota_sym: Symbol,
    nil_sym: Symbol,
    // Built-in functions
    len_sym: Symbol,
    cap_sym: Symbol,
    append_sym: Symbol,
    copy_sym: Symbol,
    delete_sym: Symbol,
    make_sym: Symbol,
    close_sym: Symbol,
    panic_sym: Symbol,
    recover_sym: Symbol,
    print_sym: Symbol,
    println_sym: Symbol,
    assert_sym: Symbol,
}

impl BuiltinSymbols {
    fn new(interner: &SymbolInterner) -> Self {
        Self {
            bool_sym: interner.get("bool").unwrap_or(Symbol::DUMMY),
            int_sym: interner.get("int").unwrap_or(Symbol::DUMMY),
            int8_sym: interner.get("int8").unwrap_or(Symbol::DUMMY),
            int16_sym: interner.get("int16").unwrap_or(Symbol::DUMMY),
            int32_sym: interner.get("int32").unwrap_or(Symbol::DUMMY),
            int64_sym: interner.get("int64").unwrap_or(Symbol::DUMMY),
            uint_sym: interner.get("uint").unwrap_or(Symbol::DUMMY),
            uint8_sym: interner.get("uint8").unwrap_or(Symbol::DUMMY),
            uint16_sym: interner.get("uint16").unwrap_or(Symbol::DUMMY),
            uint32_sym: interner.get("uint32").unwrap_or(Symbol::DUMMY),
            uint64_sym: interner.get("uint64").unwrap_or(Symbol::DUMMY),
            float32_sym: interner.get("float32").unwrap_or(Symbol::DUMMY),
            float64_sym: interner.get("float64").unwrap_or(Symbol::DUMMY),
            string_sym: interner.get("string").unwrap_or(Symbol::DUMMY),
            byte_sym: interner.get("byte").unwrap_or(Symbol::DUMMY),
            rune_sym: interner.get("rune").unwrap_or(Symbol::DUMMY),
            true_sym: interner.get("true").unwrap_or(Symbol::DUMMY),
            false_sym: interner.get("false").unwrap_or(Symbol::DUMMY),
            iota_sym: interner.get("iota").unwrap_or(Symbol::DUMMY),
            nil_sym: interner.get("nil").unwrap_or(Symbol::DUMMY),
            len_sym: interner.get("len").unwrap_or(Symbol::DUMMY),
            cap_sym: interner.get("cap").unwrap_or(Symbol::DUMMY),
            append_sym: interner.get("append").unwrap_or(Symbol::DUMMY),
            copy_sym: interner.get("copy").unwrap_or(Symbol::DUMMY),
            delete_sym: interner.get("delete").unwrap_or(Symbol::DUMMY),
            make_sym: interner.get("make").unwrap_or(Symbol::DUMMY),
            close_sym: interner.get("close").unwrap_or(Symbol::DUMMY),
            panic_sym: interner.get("panic").unwrap_or(Symbol::DUMMY),
            recover_sym: interner.get("recover").unwrap_or(Symbol::DUMMY),
            print_sym: interner.get("print").unwrap_or(Symbol::DUMMY),
            println_sym: interner.get("println").unwrap_or(Symbol::DUMMY),
            assert_sym: interner.get("assert").unwrap_or(Symbol::DUMMY),
        }
    }
}

impl<'a> TypeCollector<'a> {
    /// Creates a new type collector.
    pub fn new(interner: &'a SymbolInterner, diagnostics: &'a mut DiagnosticSink) -> Self {
        let builtin_symbols = BuiltinSymbols::new(interner);
        let mut universe = Scope::universe();

        // Populate universe scope with built-in types
        Self::populate_builtin_types(&mut universe, &builtin_symbols);
        Self::populate_builtin_consts(&mut universe, &builtin_symbols);
        Self::populate_builtin_funcs(&mut universe, &builtin_symbols);

        // Create package scope with universe as parent
        let scope = Scope::new(Some(universe), ScopeKind::Package);

        Self {
            interner,
            diagnostics,
            scope,
            named_types: Vec::new(),
            methods: Vec::new(),
            var_types: Vec::new(),
            func_sigs: Vec::new(),
            next_type_id: 0,
            iota: 0,
            builtin_symbols,
        }
    }

    fn populate_builtin_types(scope: &mut Scope, syms: &BuiltinSymbols) {
        let types = [
            (syms.bool_sym, BasicType::Bool),
            (syms.int_sym, BasicType::Int),
            (syms.int8_sym, BasicType::Int8),
            (syms.int16_sym, BasicType::Int16),
            (syms.int32_sym, BasicType::Int32),
            (syms.int64_sym, BasicType::Int64),
            (syms.uint_sym, BasicType::Uint),
            (syms.uint8_sym, BasicType::Uint8),
            (syms.uint16_sym, BasicType::Uint16),
            (syms.uint32_sym, BasicType::Uint32),
            (syms.uint64_sym, BasicType::Uint64),
            (syms.float32_sym, BasicType::Float32),
            (syms.float64_sym, BasicType::Float64),
            (syms.string_sym, BasicType::String),
            (syms.byte_sym, BasicType::Uint8), // byte = uint8
            (syms.rune_sym, BasicType::Int32), // rune = int32
        ];

        for (sym, basic) in types {
            if !sym.is_dummy() {
                scope.insert(
                    sym,
                    Entity::Var(VarEntity {
                        ty: Type::Basic(basic),
                        constant: None,
                        span: Span::dummy(),
                    }),
                );
            }
        }
    }

    fn populate_builtin_consts(scope: &mut Scope, syms: &BuiltinSymbols) {
        if !syms.true_sym.is_dummy() {
            scope.insert(
                syms.true_sym,
                Entity::Var(VarEntity {
                    ty: Type::Untyped(UntypedKind::Bool),
                    constant: Some(Constant::Bool(true)),
                    span: Span::dummy(),
                }),
            );
        }
        if !syms.false_sym.is_dummy() {
            scope.insert(
                syms.false_sym,
                Entity::Var(VarEntity {
                    ty: Type::Untyped(UntypedKind::Bool),
                    constant: Some(Constant::Bool(false)),
                    span: Span::dummy(),
                }),
            );
        }
        if !syms.nil_sym.is_dummy() {
            scope.insert(
                syms.nil_sym,
                Entity::Var(VarEntity {
                    ty: Type::Nil,
                    constant: Some(Constant::Nil),
                    span: Span::dummy(),
                }),
            );
        }
        // iota is handled specially during const evaluation
    }

    fn populate_builtin_funcs(scope: &mut Scope, syms: &BuiltinSymbols) {
        let funcs = [
            (syms.len_sym, BuiltinKind::Len),
            (syms.cap_sym, BuiltinKind::Cap),
            (syms.append_sym, BuiltinKind::Append),
            (syms.copy_sym, BuiltinKind::Copy),
            (syms.delete_sym, BuiltinKind::Delete),
            (syms.make_sym, BuiltinKind::Make),
            (syms.close_sym, BuiltinKind::Close),
            (syms.panic_sym, BuiltinKind::Panic),
            (syms.recover_sym, BuiltinKind::Recover),
            (syms.print_sym, BuiltinKind::Print),
            (syms.println_sym, BuiltinKind::Println),
            (syms.assert_sym, BuiltinKind::Assert),
        ];

        for (sym, kind) in funcs {
            if !sym.is_dummy() {
                scope.insert(sym, Entity::Builtin(kind));
            }
        }
    }

    /// Collects all declarations from a file.
    pub fn collect(mut self, file: &File) -> CollectResult {
        self.collect_decls(file);
        self.finish()
    }

    /// Collects declarations from a file (can be called multiple times for multi-file packages).
    pub fn collect_decls(&mut self, file: &File) {
        for decl in &file.decls {
            self.collect_decl(decl);
        }
    }

    /// Finishes collection and returns the result.
    pub fn finish(mut self) -> CollectResult {
        // Second pass: infer types for variables with initializers
        // (now all variable names are in scope)
        self.infer_var_types();

        CollectResult {
            scope: self.scope,
            named_types: self.named_types,
            methods: self.methods,
            var_types: self.var_types,
            func_sigs: self.func_sigs,
        }
    }

    fn collect_decl(&mut self, decl: &Decl) {
        match decl {
            Decl::Var(v) => self.collect_var(v),
            Decl::Const(c) => self.collect_const(c),
            Decl::Type(t) => self.collect_type(t),
            Decl::Func(f) => self.collect_func(f),
            Decl::Interface(i) => self.collect_interface(i),
        }
    }

    fn collect_var(&mut self, decl: &ast::VarDecl) {
        for spec in &decl.specs {
            for (i, name) in spec.names.iter().enumerate() {
                let sym = name.symbol;
                if self.check_redeclaration(sym, name.span) {
                    // First pass: just register the variable name
                    // Type will be resolved later (explicit type) or inferred in second pass
                    if let Some(ref ty_expr) = spec.ty {
                        // Track for type resolution in Phase 2
                        self.var_types.push(VarTypePlaceholder {
                            name: sym,
                            ty: Some(ty_expr.clone()),
                            init_expr: None,
                        });
                    } else if i < spec.values.len() {
                        // Track for type inference in second pass
                        self.var_types.push(VarTypePlaceholder {
                            name: sym,
                            ty: None,
                            init_expr: Some(spec.values[i].clone()),
                        });
                    }

                    // Insert with Invalid type - will be resolved later
                    self.scope.insert(
                        sym,
                        Entity::Var(VarEntity {
                            ty: Type::Invalid,
                            constant: None,
                            span: name.span,
                        }),
                    );
                }
            }
        }
    }

    /// Second pass: infer types for variables with initializers (after all names are collected)
    /// Uses iterative approach to handle forward references
    fn infer_var_types(&mut self) {
        // Iterate until no more types can be inferred (handles forward references)
        let max_iterations = self.var_types.len() + 1;
        for _ in 0..max_iterations {
            let mut made_progress = false;

            for placeholder in &self.var_types {
                if placeholder.ty.is_none() {
                    if let Some(ref init_expr) = placeholder.init_expr {
                        // Check if current type is still Invalid
                        let current_ty =
                            if let Some(Entity::Var(v)) = self.scope.lookup(placeholder.name) {
                                v.ty.clone()
                            } else {
                                continue;
                            };

                        if current_ty == Type::Invalid {
                            let inferred_ty = self.infer_type_from_expr(init_expr);
                            if inferred_ty != Type::Invalid {
                                if let Some(Entity::Var(v)) =
                                    self.scope.lookup_local_mut(placeholder.name)
                                {
                                    v.ty = inferred_ty;
                                    made_progress = true;
                                }
                            }
                        }
                    }
                }
            }

            if !made_progress {
                break;
            }
        }
    }

    /// Infer type from a simple expression (for var initialization).
    fn infer_type_from_expr(&self, expr: &ast::Expr) -> Type {
        use ast::ExprKind;
        match &expr.kind {
            ExprKind::IntLit(_) => Type::Basic(BasicType::Int),
            ExprKind::FloatLit(_) => Type::Basic(BasicType::Float64),
            ExprKind::StringLit(_) => Type::Basic(BasicType::String),
            ExprKind::Ident(ident) => {
                let name = self.interner.resolve(ident.symbol).unwrap_or("");
                match name {
                    "true" | "false" => Type::Basic(BasicType::Bool),
                    "nil" => Type::Nil,
                    _ => {
                        // Look up variable in scope
                        if let Some(Entity::Var(var)) = self.scope.lookup(ident.symbol) {
                            return var.ty.clone();
                        }
                        Type::Invalid
                    }
                }
            }
            // Handle binary expressions - infer from operands
            ExprKind::Binary(bin) => {
                let left_ty = self.infer_type_from_expr(&bin.left);
                let right_ty = self.infer_type_from_expr(&bin.right);
                // For arithmetic/comparison, return left type (or right if left is invalid)
                if left_ty != Type::Invalid {
                    left_ty
                } else {
                    right_ty
                }
            }
            // Handle unary expressions
            ExprKind::Unary(un) => self.infer_type_from_expr(&un.operand),
            // Handle parenthesized expressions
            ExprKind::Paren(inner) => self.infer_type_from_expr(inner),
            // Handle make/new calls - infer type from first argument
            ExprKind::Call(call) => {
                if let ExprKind::Ident(func_ident) = &call.func.kind {
                    let func_name = self.interner.resolve(func_ident.symbol).unwrap_or("");
                    if (func_name == "make" || func_name == "new") && !call.args.is_empty() {
                        return self.infer_type_from_type_arg(&call.args[0]);
                    }
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

    /// Infer type from a type argument expression (for make/new).
    fn infer_type_from_type_arg(&self, expr: &ast::Expr) -> Type {
        use ast::ExprKind;
        match &expr.kind {
            ExprKind::Ident(ident) => {
                // Named type - look up in scope
                if let Some(Entity::Type(type_entity)) = self.scope.lookup(ident.symbol) {
                    return Type::Named(type_entity.id);
                }
                Type::Invalid
            }
            ExprKind::TypeAsExpr(ty) => self.infer_type_from_type_expr(ty),
            _ => Type::Invalid,
        }
    }

    fn collect_const(&mut self, decl: &ast::ConstDecl) {
        self.iota = 0;

        for spec in &decl.specs {
            for (i, name) in spec.names.iter().enumerate() {
                let sym = name.symbol;
                if self.check_redeclaration(sym, name.span) {
                    // Evaluate constant value if possible
                    let (mut ty, constant) = if i < spec.values.len() {
                        self.eval_const_expr(&spec.values[i])
                    } else {
                        // Use previous spec's expression pattern (iota continues)
                        (Type::Invalid, None)
                    };

                    // If there's an explicit type annotation, use it
                    // (e.g., `const FB float64 = 1 << 100`)
                    if spec.ty.is_some() {
                        // For typed constants, infer the basic type from the type expression
                        ty = self.infer_type_from_type_expr(spec.ty.as_ref().unwrap());
                    }

                    self.scope.insert(
                        sym,
                        Entity::Var(VarEntity {
                            ty,
                            constant,
                            span: name.span,
                        }),
                    );
                }
            }
            self.iota += 1;
        }
    }

    /// Infer type from a type expression.
    fn infer_type_from_type_expr(&self, ty_expr: &ast::TypeExpr) -> Type {
        use ast::TypeExprKind;
        match &ty_expr.kind {
            TypeExprKind::Ident(ident) => {
                let name = self.interner.resolve(ident.symbol).unwrap_or("");
                match name {
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
                    _ => {
                        // Check for named types
                        if let Some(Entity::Type(type_entity)) = self.scope.lookup(ident.symbol) {
                            return Type::Named(type_entity.id);
                        }
                        Type::Invalid
                    }
                }
            }
            TypeExprKind::Chan(c) => {
                let elem_ty = self.infer_type_from_type_expr(&c.elem);
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
            TypeExprKind::Map(m) => {
                let key_ty = self.infer_type_from_type_expr(&m.key);
                let value_ty = self.infer_type_from_type_expr(&m.value);
                Type::Map(crate::types::MapType {
                    key: Box::new(key_ty),
                    value: Box::new(value_ty),
                })
            }
            TypeExprKind::Slice(elem) => {
                let elem_ty = self.infer_type_from_type_expr(elem);
                Type::Slice(crate::types::SliceType {
                    elem: Box::new(elem_ty),
                })
            }
            _ => Type::Invalid,
        }
    }

    fn collect_type(&mut self, decl: &ast::TypeDecl) {
        let sym = decl.name.symbol;
        if !self.check_redeclaration(sym, decl.name.span) {
            return;
        }

        // Allocate a new named type ID
        let id = NamedTypeId(self.next_type_id);
        self.next_type_id += 1;

        // Register placeholder
        self.named_types.push(NamedTypePlaceholder {
            name: sym,
            ast_type: decl.ty.clone(),
            span: decl.name.span,
        });

        self.scope.insert(
            sym,
            Entity::Type(TypeEntity {
                id,
                span: decl.name.span,
            }),
        );
    }

    fn collect_func(&mut self, decl: &ast::FuncDecl) {
        // If there's a receiver, this is a method - collect it separately
        if let Some(ref receiver) = decl.receiver {
            self.methods.push(MethodPlaceholder {
                receiver_type: receiver.ty.symbol,
                name: decl.name.symbol,
                decl: decl.clone(),
            });
            return;
        }

        let sym = decl.name.symbol;
        let name = self.interner.resolve(sym).unwrap_or("");

        // Go allows multiple init() functions - skip redeclaration check for init
        if name != "init" && !self.check_redeclaration(sym, decl.name.span) {
            return;
        }

        // Create a placeholder function type (resolved in Phase 2)
        let sig = FuncType {
            params: Vec::new(),  // Will be resolved
            results: Vec::new(), // Will be resolved
            variadic: decl.sig.variadic,
        };

        self.scope.insert(
            sym,
            Entity::Func(FuncEntity {
                sig,
                is_native: decl.is_native,
                span: decl.name.span,
            }),
        );

        // Track for signature resolution in Phase 2
        self.func_sigs.push(FuncSigPlaceholder {
            name: sym,
            sig: decl.sig.clone(),
        });
    }

    fn collect_interface(&mut self, decl: &ast::InterfaceDecl) {
        let sym = decl.name.symbol;
        if !self.check_redeclaration(sym, decl.name.span) {
            return;
        }

        // Allocate a new named type ID for the interface
        let id = NamedTypeId(self.next_type_id);
        self.next_type_id += 1;

        // Create a placeholder AST type for the interface
        let ast_type = ast::TypeExpr {
            kind: ast::TypeExprKind::Interface(Box::new(ast::InterfaceType {
                elems: decl.elems.clone(),
            })),
            span: decl.span,
        };

        self.named_types.push(NamedTypePlaceholder {
            name: sym,
            ast_type,
            span: decl.name.span,
        });

        self.scope.insert(
            sym,
            Entity::Type(TypeEntity {
                id,
                span: decl.name.span,
            }),
        );
    }

    /// Checks for redeclaration and reports an error if found.
    /// Returns true if the name can be declared.
    fn check_redeclaration(&mut self, sym: Symbol, span: Span) -> bool {
        if let Some(existing) = self.scope.lookup_local(sym) {
            let existing_span = match existing {
                Entity::Var(v) => v.span,
                Entity::Type(t) => t.span,
                Entity::Func(f) => f.span,
                Entity::Package(p) => p.span,
                Entity::Label(l) => l.span,
                Entity::Builtin(_) => Span::dummy(),
            };

            let name = self.interner.resolve(sym).unwrap_or("<unknown>");
            let mut diag = TypeError::Redeclared
                .at_with_message(span, TypeError::Redeclared.with_name(name));

            if !existing_span.is_dummy() {
                diag = diag.with_label(
                    Label::secondary(existing_span).with_message("previous declaration here"),
                );
            }

            self.diagnostics.emit(diag);
            false
        } else {
            true
        }
    }

    /// Evaluates a constant expression (simplified for Phase 1).
    /// Full constant evaluation happens in Phase 3.
    fn eval_const_expr(&mut self, expr: &ast::Expr) -> (Type, Option<Constant>) {
        match &expr.kind {
            ast::ExprKind::IntLit(lit) => {
                let s = self.interner.resolve(lit.raw).unwrap_or("");
                if let Ok(v) = parse_int_literal(s) {
                    (Type::Untyped(UntypedKind::Int), Some(Constant::int(v)))
                } else {
                    (Type::Invalid, None)
                }
            }
            ast::ExprKind::FloatLit(lit) => {
                let s = self.interner.resolve(lit.raw).unwrap_or("");
                if let Ok(v) = s.parse::<f64>() {
                    (Type::Untyped(UntypedKind::Float), Some(Constant::float(v)))
                } else {
                    (Type::Invalid, None)
                }
            }
            ast::ExprKind::StringLit(lit) => {
                let s = self.interner.resolve(lit.raw).unwrap_or("");
                // Remove quotes and unescape
                let value = parse_string_literal(s);
                (
                    Type::Untyped(UntypedKind::String),
                    Some(Constant::String(value)),
                )
            }
            ast::ExprKind::RuneLit(lit) => {
                let s = self.interner.resolve(lit.raw).unwrap_or("");
                if let Some(c) = parse_rune_literal(s) {
                    (Type::Untyped(UntypedKind::Rune), Some(Constant::Rune(c)))
                } else {
                    (Type::Invalid, None)
                }
            }
            ast::ExprKind::Ident(id) => {
                // Check for iota
                if id.symbol == self.builtin_symbols.iota_sym {
                    return (
                        Type::Untyped(UntypedKind::Int),
                        Some(Constant::int(self.iota as i64)),
                    );
                }
                // Check for true/false
                if id.symbol == self.builtin_symbols.true_sym {
                    return (Type::Untyped(UntypedKind::Bool), Some(Constant::Bool(true)));
                }
                if id.symbol == self.builtin_symbols.false_sym {
                    return (
                        Type::Untyped(UntypedKind::Bool),
                        Some(Constant::Bool(false)),
                    );
                }
                // Look up in scope
                if let Some(Entity::Var(v)) = self.scope.lookup(id.symbol) {
                    if let Some(c) = &v.constant {
                        return (v.ty.clone(), Some(c.clone()));
                    }
                }
                (Type::Invalid, None)
            }
            ast::ExprKind::Binary(bin) => {
                let (_, left) = self.eval_const_expr(&bin.left);
                let (_, right) = self.eval_const_expr(&bin.right);

                if let (Some(l), Some(r)) = (left, right) {
                    let result = match bin.op {
                        BinaryOp::Add => l.add(&r),
                        BinaryOp::Sub => l.sub(&r),
                        BinaryOp::Mul => l.mul(&r),
                        BinaryOp::Div => l.div(&r),
                        BinaryOp::Rem => l.rem(&r),
                        BinaryOp::Shl => l.shl(&r),
                        BinaryOp::Shr => l.shr(&r),
                        BinaryOp::And => l.bit_and(&r),
                        BinaryOp::Or => l.bit_or(&r),
                        BinaryOp::Xor => l.bit_xor(&r),
                        BinaryOp::AndNot => l.bit_clear(&r),
                        BinaryOp::LogAnd => l.and(&r),
                        BinaryOp::LogOr => l.or(&r),
                        BinaryOp::Eq => l.const_eq(&r),
                        BinaryOp::NotEq => l.const_eq(&r).and_then(|c| c.not()),
                        BinaryOp::Lt => l.lt(&r),
                        BinaryOp::LtEq => l.le(&r),
                        BinaryOp::Gt => r.lt(&l),
                        BinaryOp::GtEq => r.le(&l),
                    };

                    if let Some(c) = result {
                        let kind = c.kind().unwrap_or(UntypedKind::Int);
                        return (Type::Untyped(kind), Some(c));
                    }
                }
                (Type::Invalid, None)
            }
            ast::ExprKind::Unary(un) => {
                let (_, val) = self.eval_const_expr(&un.operand);
                if let Some(v) = val {
                    let result = match un.op {
                        UnaryOp::Neg => v.neg(),
                        UnaryOp::Not => v.not(),
                        UnaryOp::BitNot => v.bit_not(),
                        UnaryOp::Pos => Some(v),
                    };
                    if let Some(c) = result {
                        let kind = c.kind().unwrap_or(UntypedKind::Int);
                        return (Type::Untyped(kind), Some(c));
                    }
                }
                (Type::Invalid, None)
            }
            ast::ExprKind::Paren(inner) => self.eval_const_expr(inner),
            _ => (Type::Invalid, None),
        }
    }
}

/// Parses a string literal, removing quotes and handling escapes.
fn parse_string_literal(s: &str) -> String {
    // Remove surrounding quotes
    let s = if s.starts_with('"') && s.ends_with('"') {
        &s[1..s.len() - 1]
    } else if s.starts_with('`') && s.ends_with('`') {
        // Raw string - no escape processing
        return s[1..s.len() - 1].to_string();
    } else {
        s
    };

    let mut result = String::new();
    let mut chars = s.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => result.push('\n'),
                Some('r') => result.push('\r'),
                Some('t') => result.push('\t'),
                Some('\\') => result.push('\\'),
                Some('"') => result.push('"'),
                Some('0') => result.push('\0'),
                Some('x') => {
                    let hex: String = chars.by_ref().take(2).collect();
                    if let Ok(b) = u8::from_str_radix(&hex, 16) {
                        result.push(b as char);
                    }
                }
                Some('u') => {
                    let hex: String = chars.by_ref().take(4).collect();
                    if let Ok(v) = u32::from_str_radix(&hex, 16) {
                        if let Some(c) = char::from_u32(v) {
                            result.push(c);
                        }
                    }
                }
                Some(other) => {
                    result.push('\\');
                    result.push(other);
                }
                None => result.push('\\'),
            }
        } else {
            result.push(c);
        }
    }

    result
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
        // Legacy octal
        i64::from_str_radix(&s[1..], 8).map_err(|_| ())
    } else {
        s.parse().map_err(|_| ())
    }
}

/// Parses a rune literal string to char.
/// Supports all Go escape sequences:
/// - Simple escapes: \a \b \f \n \r \t \v \\ \' \"
/// - Hex escape: \xhh (2 hex digits)
/// - Unicode escapes: \uhhhh (4 hex digits), \Uhhhhhhhh (8 hex digits)
/// - Octal escape: \ooo (3 octal digits)
pub fn parse_rune_literal(s: &str) -> Option<char> {
    // Remove surrounding quotes if present (only first and last)
    let s = if s.starts_with('\'') && s.ends_with('\'') && s.len() >= 2 {
        &s[1..s.len() - 1]
    } else {
        s
    };
    if s.starts_with('\\') {
        // Escape sequence
        let escape_char = s.chars().nth(1)?;
        match escape_char {
            // Simple escapes
            'a' => Some('\x07'), // bell
            'b' => Some('\x08'), // backspace
            'f' => Some('\x0C'), // form feed
            'n' => Some('\n'),   // newline
            'r' => Some('\r'),   // carriage return
            't' => Some('\t'),   // horizontal tab
            'v' => Some('\x0B'), // vertical tab
            '\\' => Some('\\'),
            '\'' => Some('\''),
            '"' => Some('"'),
            // Hex escape: \xhh
            'x' => {
                let hex = &s[2..];
                if hex.len() >= 2 {
                    u8::from_str_radix(&hex[..2], 16).ok().map(|b| b as char)
                } else {
                    None
                }
            }
            // Unicode escape: \uhhhh (4 hex digits)
            'u' => {
                let hex = &s[2..];
                if hex.len() >= 4 {
                    u32::from_str_radix(&hex[..4], 16)
                        .ok()
                        .and_then(char::from_u32)
                } else {
                    None
                }
            }
            // Unicode escape: \Uhhhhhhhh (8 hex digits)
            'U' => {
                let hex = &s[2..];
                if hex.len() >= 8 {
                    u32::from_str_radix(&hex[..8], 16)
                        .ok()
                        .and_then(char::from_u32)
                } else {
                    None
                }
            }
            // Octal escape: \ooo (3 octal digits, 0-7)
            c if matches!(c, '0'..='7') => {
                let octal = &s[1..];
                if octal.len() >= 3 && octal[..3].chars().all(|c| matches!(c, '0'..='7')) {
                    u8::from_str_radix(&octal[..3], 8).ok().map(|b| b as char)
                } else {
                    None
                }
            }
            _ => None,
        }
    } else {
        s.chars().next()
    }
}

/// Convenience function to run Phase 1 collection.
pub fn collect_types(
    file: &File,
    interner: &SymbolInterner,
    diagnostics: &mut DiagnosticSink,
) -> CollectResult {
    let collector = TypeCollector::new(interner, diagnostics);
    collector.collect(file)
}

/// Collect types from multiple files in the same package.
pub fn collect_types_multi(
    files: &[&File],
    interner: &SymbolInterner,
    diagnostics: &mut DiagnosticSink,
) -> CollectResult {
    let mut collector = TypeCollector::new(interner, diagnostics);

    // Collect declarations from all files
    for file in files {
        collector.collect_decls(file);
    }

    collector.finish()
}

#[cfg(test)]
mod tests {
    use super::*;
    use gox_syntax::parse;

    #[test]
    fn test_parse_int_literal() {
        assert_eq!(parse_int_literal("42"), Ok(42));
        assert_eq!(parse_int_literal("0x2A"), Ok(42));
        assert_eq!(parse_int_literal("0o52"), Ok(42));
        assert_eq!(parse_int_literal("0b101010"), Ok(42));
        assert_eq!(parse_int_literal("1_000_000"), Ok(1_000_000));
    }

    #[test]
    fn test_parse_rune_literal() {
        assert_eq!(parse_rune_literal("a"), Some('a'));
        assert_eq!(parse_rune_literal("'a'"), Some('a'));
        assert_eq!(parse_rune_literal("\\n"), Some('\n'));
        assert_eq!(parse_rune_literal("\\t"), Some('\t'));
        assert_eq!(parse_rune_literal("\\x41"), Some('A'));
    }

    #[test]
    fn test_parse_string_literal() {
        assert_eq!(parse_string_literal("\"hello\""), "hello");
        assert_eq!(parse_string_literal("\"hello\\nworld\""), "hello\nworld");
        assert_eq!(parse_string_literal("`raw string`"), "raw string");
    }

    fn collect_source(source: &str) -> (CollectResult, DiagnosticSink, SymbolInterner) {
        let (file, _parse_diag, interner) = parse(source, 0);

        let mut collect_diag = DiagnosticSink::new();
        let result = collect_types(&file, &interner, &mut collect_diag);

        (result, collect_diag, interner)
    }

    #[test]
    fn test_collect_var_decl() {
        let (result, diag, interner) = collect_source("package main\nvar x int");

        assert!(!diag.has_errors());

        let x_sym = interner.get("x").unwrap();
        assert!(result.scope.lookup(x_sym).is_some());
    }

    #[test]
    fn test_collect_const_decl() {
        let (result, diag, interner) = collect_source("package main\nconst Pi = 3.14");

        assert!(!diag.has_errors());

        let pi_sym = interner.get("Pi").unwrap();
        if let Some(Entity::Var(v)) = result.scope.lookup(pi_sym) {
            assert!(v.constant.is_some());
            if let Some(c) = &v.constant {
                if let Some(f) = c.to_f64() {
                    assert!((f - 3.14).abs() < 0.001);
                } else {
                    panic!("expected float constant");
                }
            }
        } else {
            panic!("expected var entity for Pi");
        }
    }

    #[test]
    fn test_collect_const_iota() {
        let (result, diag, interner) =
            collect_source("package main\nconst (\n  A = iota\n  B\n  C\n)");

        assert!(!diag.has_errors());

        let a_sym = interner.get("A").unwrap();
        let b_sym = interner.get("B").unwrap();
        let c_sym = interner.get("C").unwrap();

        if let Some(Entity::Var(v)) = result.scope.lookup(a_sym) {
            assert_eq!(v.constant, Some(Constant::int(0)));
        } else {
            panic!("expected var entity for A");
        }

        // B and C don't have values in this simple test since we don't
        // carry forward the expression pattern yet
    }

    #[test]
    fn test_collect_type_decl() {
        let (result, diag, interner) = collect_source("package main\ntype MyInt int");

        assert!(!diag.has_errors());

        let myint_sym = interner.get("MyInt").unwrap();
        assert!(result.scope.lookup(myint_sym).is_some());
        assert_eq!(result.named_types.len(), 1);
        assert_eq!(result.named_types[0].name, myint_sym);
    }

    #[test]
    fn test_collect_func_decl() {
        let (result, diag, interner) =
            collect_source("package main\nfunc add(a, b int) int { return a + b }");

        assert!(!diag.has_errors());

        let add_sym = interner.get("add").unwrap();
        assert!(matches!(
            result.scope.lookup(add_sym),
            Some(Entity::Func(_))
        ));
    }

    #[test]
    fn test_collect_interface_decl() {
        let (result, diag, interner) =
            collect_source("package main\ninterface Reader { Read(buf []byte) int }");

        assert!(!diag.has_errors());

        let reader_sym = interner.get("Reader").unwrap();
        assert!(matches!(
            result.scope.lookup(reader_sym),
            Some(Entity::Type(_))
        ));
    }

    #[test]
    fn test_collect_duplicate_error() {
        let (_, diag, _) = collect_source("package main\nvar x int\nvar x string");

        assert!(diag.has_errors());
    }

    #[test]
    fn test_collect_builtin_types() {
        // Use source that references built-in types so they get interned
        let (result, _, interner) = collect_source("package main\nvar x int\nvar y string");

        // Check that built-in types are in the universe scope
        let int_sym = interner.get("int").unwrap();
        assert!(result.scope.lookup(int_sym).is_some());

        let string_sym = interner.get("string").unwrap();
        assert!(result.scope.lookup(string_sym).is_some());
    }

    #[test]
    fn test_collect_builtin_funcs() {
        // Use source that references built-in functions so they get interned
        let (result, _, interner) =
            collect_source("package main\nfunc f() { len(nil); make([]int, 0) }");

        let len_sym = interner.get("len").unwrap();
        assert!(matches!(
            result.scope.lookup(len_sym),
            Some(Entity::Builtin(BuiltinKind::Len))
        ));

        let make_sym = interner.get("make").unwrap();
        assert!(matches!(
            result.scope.lookup(make_sym),
            Some(Entity::Builtin(BuiltinKind::Make))
        ));
    }

    #[test]
    fn test_collect_builtin_consts() {
        // Use source that references built-in constants so they get interned
        let (result, _, interner) = collect_source("package main\nvar x = true\nvar y = nil");

        let true_sym = interner.get("true").unwrap();
        if let Some(Entity::Var(v)) = result.scope.lookup(true_sym) {
            assert_eq!(v.constant, Some(Constant::Bool(true)));
        } else {
            panic!("expected var entity for true");
        }

        let nil_sym = interner.get("nil").unwrap();
        if let Some(Entity::Var(v)) = result.scope.lookup(nil_sym) {
            assert_eq!(v.constant, Some(Constant::Nil));
        } else {
            panic!("expected var entity for nil");
        }
    }

    #[test]
    fn test_collect_const_expr_binary() {
        let (result, diag, interner) = collect_source("package main\nconst X = 1 + 2 * 3");

        assert!(!diag.has_errors());

        let x_sym = interner.get("X").unwrap();
        if let Some(Entity::Var(v)) = result.scope.lookup(x_sym) {
            // Note: without proper precedence in eval, this might be wrong
            // but we're testing that binary expressions work at all
            assert!(v.constant.is_some());
        } else {
            panic!("expected var entity for X");
        }
    }

    #[test]
    fn test_collect_const_expr_shift() {
        let (result, diag, interner) = collect_source("package main\nconst KB = 1 << 10");

        assert!(!diag.has_errors());

        let kb_sym = interner.get("KB").unwrap();
        if let Some(Entity::Var(v)) = result.scope.lookup(kb_sym) {
            assert_eq!(v.constant, Some(Constant::int(1024)));
        } else {
            panic!("expected var entity for KB");
        }
    }

    #[test]
    fn test_collect_const_unary_neg() {
        let (result, diag, interner) = collect_source("package main\nconst X = -42");

        assert!(!diag.has_errors());

        let x_sym = interner.get("X").unwrap();
        if let Some(Entity::Var(v)) = result.scope.lookup(x_sym) {
            assert_eq!(v.constant, Some(Constant::int(-42)));
        } else {
            panic!("expected var entity for X");
        }
    }

    #[test]
    fn test_collect_const_unary_not() {
        let (result, diag, interner) = collect_source("package main\nconst X = !true");

        assert!(!diag.has_errors());

        let x_sym = interner.get("X").unwrap();
        if let Some(Entity::Var(v)) = result.scope.lookup(x_sym) {
            assert_eq!(v.constant, Some(Constant::Bool(false)));
        } else {
            panic!("expected var entity for X");
        }
    }

    #[test]
    fn test_collect_const_bitwise() {
        let (result, diag, interner) = collect_source("package main\nconst X = 0xFF & 0x0F");

        assert!(!diag.has_errors());

        let x_sym = interner.get("X").unwrap();
        if let Some(Entity::Var(v)) = result.scope.lookup(x_sym) {
            assert_eq!(v.constant, Some(Constant::int(0x0F)));
        } else {
            panic!("expected var entity for X");
        }
    }

    #[test]
    fn test_collect_const_string_concat() {
        let (result, diag, interner) = collect_source(
            r#"package main
const X = "hello" + " world""#,
        );

        assert!(!diag.has_errors());

        let x_sym = interner.get("X").unwrap();
        if let Some(Entity::Var(v)) = result.scope.lookup(x_sym) {
            assert_eq!(
                v.constant,
                Some(Constant::String("hello world".to_string()))
            );
        } else {
            panic!("expected var entity for X");
        }
    }

    #[test]
    fn test_collect_const_comparison() {
        let (result, diag, interner) = collect_source("package main\nconst X = 1 < 2");

        assert!(!diag.has_errors());

        let x_sym = interner.get("X").unwrap();
        if let Some(Entity::Var(v)) = result.scope.lookup(x_sym) {
            assert_eq!(v.constant, Some(Constant::Bool(true)));
        } else {
            panic!("expected var entity for X");
        }
    }

    #[test]
    fn test_collect_const_logical() {
        let (result, diag, interner) = collect_source("package main\nconst X = true && false");

        assert!(!diag.has_errors());

        let x_sym = interner.get("X").unwrap();
        if let Some(Entity::Var(v)) = result.scope.lookup(x_sym) {
            assert_eq!(v.constant, Some(Constant::Bool(false)));
        } else {
            panic!("expected var entity for X");
        }
    }

    #[test]
    fn test_collect_const_reference_other() {
        let (result, diag, interner) =
            collect_source("package main\nconst A = 10\nconst B = A + 5");

        assert!(!diag.has_errors());

        let b_sym = interner.get("B").unwrap();
        if let Some(Entity::Var(v)) = result.scope.lookup(b_sym) {
            assert_eq!(v.constant, Some(Constant::int(15)));
        } else {
            panic!("expected var entity for B");
        }
    }

    #[test]
    fn test_collect_multiple_vars() {
        let (result, diag, interner) = collect_source("package main\nvar a, b, c int");

        assert!(!diag.has_errors());

        for name in ["a", "b", "c"] {
            let sym = interner.get(name).unwrap();
            assert!(
                result.scope.lookup(sym).is_some(),
                "expected {} to be declared",
                name
            );
        }
    }

    #[test]
    fn test_collect_multiple_consts() {
        let (result, diag, interner) = collect_source("package main\nconst x, y = 1, 2");

        assert!(!diag.has_errors());

        let x_sym = interner.get("x").unwrap();
        let y_sym = interner.get("y").unwrap();

        if let Some(Entity::Var(v)) = result.scope.lookup(x_sym) {
            assert_eq!(v.constant, Some(Constant::int(1)));
        }
        if let Some(Entity::Var(v)) = result.scope.lookup(y_sym) {
            assert_eq!(v.constant, Some(Constant::int(2)));
        }
    }

    #[test]
    fn test_collect_struct_type() {
        let (result, diag, interner) =
            collect_source("package main\ntype Point struct { x, y int }");

        assert!(!diag.has_errors());

        let point_sym = interner.get("Point").unwrap();
        assert!(matches!(
            result.scope.lookup(point_sym),
            Some(Entity::Type(_))
        ));
        assert_eq!(result.named_types.len(), 1);
    }

    #[test]
    fn test_collect_object_type() {
        let (result, diag, interner) =
            collect_source("package main\ntype Counter object { value int }");

        assert!(!diag.has_errors());

        let counter_sym = interner.get("Counter").unwrap();
        assert!(matches!(
            result.scope.lookup(counter_sym),
            Some(Entity::Type(_))
        ));
    }

    #[test]
    fn test_collect_func_with_params() {
        let (result, diag, interner) =
            collect_source("package main\nfunc add(a int, b int) int { return a + b }");

        assert!(!diag.has_errors());

        let add_sym = interner.get("add").unwrap();
        assert!(matches!(
            result.scope.lookup(add_sym),
            Some(Entity::Func(_))
        ));
    }

    #[test]
    fn test_collect_func_variadic() {
        let (result, diag, interner) =
            collect_source("package main\nfunc sum(nums ...int) int { return 0 }");

        assert!(!diag.has_errors());

        let sum_sym = interner.get("sum").unwrap();
        if let Some(Entity::Func(f)) = result.scope.lookup(sum_sym) {
            assert!(f.sig.variadic);
        } else {
            panic!("expected func entity for sum");
        }
    }

    #[test]
    fn test_collect_interface_with_methods() {
        let (result, diag, interner) = collect_source(
            "package main\ninterface ReadWriter { Read(buf []byte) int; Write(buf []byte) int }",
        );

        assert!(!diag.has_errors());

        let rw_sym = interner.get("ReadWriter").unwrap();
        assert!(matches!(result.scope.lookup(rw_sym), Some(Entity::Type(_))));
    }

    #[test]
    fn test_collect_const_paren_expr() {
        let (result, diag, interner) = collect_source("package main\nconst X = (1 + 2) * 3");

        assert!(!diag.has_errors());

        let x_sym = interner.get("X").unwrap();
        if let Some(Entity::Var(v)) = result.scope.lookup(x_sym) {
            assert_eq!(v.constant, Some(Constant::int(9)));
        } else {
            panic!("expected var entity for X");
        }
    }

    #[test]
    fn test_collect_const_rune() {
        let (result, diag, interner) = collect_source("package main\nconst X = 'A'");

        assert!(!diag.has_errors());

        let x_sym = interner.get("X").unwrap();
        if let Some(Entity::Var(v)) = result.scope.lookup(x_sym) {
            assert_eq!(v.constant, Some(Constant::Rune('A')));
        } else {
            panic!("expected var entity for X");
        }
    }

    #[test]
    fn test_collect_const_hex() {
        let (result, diag, interner) = collect_source("package main\nconst X = 0xDEADBEEF");

        assert!(!diag.has_errors());

        let x_sym = interner.get("X").unwrap();
        if let Some(Entity::Var(v)) = result.scope.lookup(x_sym) {
            assert_eq!(v.constant, Some(Constant::int(0xDEADBEEF)));
        } else {
            panic!("expected var entity for X");
        }
    }

    #[test]
    fn test_collect_const_octal() {
        let (result, diag, interner) = collect_source("package main\nconst X = 0o755");

        assert!(!diag.has_errors());

        let x_sym = interner.get("X").unwrap();
        if let Some(Entity::Var(v)) = result.scope.lookup(x_sym) {
            assert_eq!(v.constant, Some(Constant::int(0o755)));
        } else {
            panic!("expected var entity for X");
        }
    }

    #[test]
    fn test_collect_const_binary() {
        let (result, diag, interner) = collect_source("package main\nconst X = 0b1010");

        assert!(!diag.has_errors());

        let x_sym = interner.get("X").unwrap();
        if let Some(Entity::Var(v)) = result.scope.lookup(x_sym) {
            assert_eq!(v.constant, Some(Constant::int(0b1010)));
        } else {
            panic!("expected var entity for X");
        }
    }

    #[test]
    fn test_collect_duplicate_type_error() {
        let (_, diag, _) = collect_source("package main\ntype X int\ntype X string");

        assert!(diag.has_errors());
    }

    #[test]
    fn test_collect_duplicate_func_error() {
        let (_, diag, _) = collect_source("package main\nfunc f() {}\nfunc f() {}");

        assert!(diag.has_errors());
    }

    #[test]
    fn test_collect_duplicate_const_error() {
        let (_, diag, _) = collect_source("package main\nconst X = 1\nconst X = 2");

        assert!(diag.has_errors());
    }

    #[test]
    fn test_collect_iota_expression() {
        let (result, diag, interner) =
            collect_source("package main\nconst (\n  KB = 1 << (10 * iota)\n  MB\n  GB\n)");

        assert!(!diag.has_errors());

        let kb_sym = interner.get("KB").unwrap();
        if let Some(Entity::Var(v)) = result.scope.lookup(kb_sym) {
            // 1 << (10 * 0) = 1
            assert_eq!(v.constant, Some(Constant::int(1)));
        } else {
            panic!("expected var entity for KB");
        }
    }

    #[test]
    fn test_collect_const_division() {
        let (result, diag, interner) = collect_source("package main\nconst X = 10 / 3");

        assert!(!diag.has_errors());

        let x_sym = interner.get("X").unwrap();
        if let Some(Entity::Var(v)) = result.scope.lookup(x_sym) {
            assert_eq!(v.constant, Some(Constant::int(3)));
        } else {
            panic!("expected var entity for X");
        }
    }

    #[test]
    fn test_collect_const_modulo() {
        let (result, diag, interner) = collect_source("package main\nconst X = 10 % 3");

        assert!(!diag.has_errors());

        let x_sym = interner.get("X").unwrap();
        if let Some(Entity::Var(v)) = result.scope.lookup(x_sym) {
            assert_eq!(v.constant, Some(Constant::int(1)));
        } else {
            panic!("expected var entity for X");
        }
    }

    // Tests for parse_rune_literal
    #[test]
    fn test_parse_rune_simple() {
        assert_eq!(parse_rune_literal("'a'"), Some('a'));
        assert_eq!(parse_rune_literal("'Z'"), Some('Z'));
        assert_eq!(parse_rune_literal("'0'"), Some('0'));
        assert_eq!(parse_rune_literal("' '"), Some(' '));
        assert_eq!(parse_rune_literal("'中'"), Some('中'));
        assert_eq!(parse_rune_literal("'🎉'"), Some('🎉'));
    }

    #[test]
    fn test_parse_rune_simple_escapes() {
        assert_eq!(parse_rune_literal("'\\n'"), Some('\n'));
        assert_eq!(parse_rune_literal("'\\r'"), Some('\r'));
        assert_eq!(parse_rune_literal("'\\t'"), Some('\t'));
        assert_eq!(parse_rune_literal("'\\\\'"), Some('\\'));
        assert_eq!(parse_rune_literal("'\\''"), Some('\''));
        assert_eq!(parse_rune_literal("'\\\"'"), Some('"'));
    }

    #[test]
    fn test_parse_rune_bell_escapes() {
        // Bell, backspace, form feed, vertical tab
        assert_eq!(parse_rune_literal("'\\a'"), Some('\x07'));
        assert_eq!(parse_rune_literal("'\\b'"), Some('\x08'));
        assert_eq!(parse_rune_literal("'\\f'"), Some('\x0C'));
        assert_eq!(parse_rune_literal("'\\v'"), Some('\x0B'));
    }

    #[test]
    fn test_parse_rune_hex_escape() {
        assert_eq!(parse_rune_literal("'\\x41'"), Some('A'));
        assert_eq!(parse_rune_literal("'\\x00'"), Some('\0'));
        assert_eq!(parse_rune_literal("'\\xff'"), Some('\u{00ff}'));
        assert_eq!(parse_rune_literal("'\\x7F'"), Some('\x7F'));
    }

    #[test]
    fn test_parse_rune_unicode_escape() {
        // \u with 4 hex digits
        assert_eq!(parse_rune_literal("'\\u0041'"), Some('A'));
        assert_eq!(parse_rune_literal("'\\u4e2d'"), Some('中'));
        assert_eq!(parse_rune_literal("'\\u0000'"), Some('\0'));

        // \U with 8 hex digits
        assert_eq!(parse_rune_literal("'\\U00000041'"), Some('A'));
        assert_eq!(parse_rune_literal("'\\U0001F389'"), Some('🎉'));
        assert_eq!(parse_rune_literal("'\\U00004E2D'"), Some('中'));
    }

    #[test]
    fn test_parse_rune_octal_escape() {
        assert_eq!(parse_rune_literal("'\\000'"), Some('\0'));
        assert_eq!(parse_rune_literal("'\\101'"), Some('A')); // 65 in octal
        assert_eq!(parse_rune_literal("'\\012'"), Some('\n')); // 10 in octal
        assert_eq!(parse_rune_literal("'\\177'"), Some('\x7F')); // 127 in octal
    }

    #[test]
    fn test_parse_rune_invalid() {
        // Invalid escape sequence
        assert_eq!(parse_rune_literal("'\\z'"), None);
        // Empty string
        assert_eq!(parse_rune_literal(""), None);
        // Invalid Unicode code point (surrogate)
        assert_eq!(parse_rune_literal("'\\uD800'"), None);
    }
}
