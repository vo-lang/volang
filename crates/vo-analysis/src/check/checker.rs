//! The type checker.
//!
//! This module contains the main Checker struct and related context types
//! for type checking Vo source code.


use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use vo_common::diagnostics::{Diagnostic, DiagnosticSink};
use vo_common::span::Span;
use vo_common::symbol::SymbolInterner;
use vo_syntax::ast::ExprId;
use vo_syntax::ast::{Expr, File};

use super::errors::TypeError;
use super::type_info::TypeInfo;
use crate::obj::{ConstValue, Pos};
use crate::objects::{DeclInfoKey, ObjKey, PackageKey, ScopeKey, TCObjects, TypeKey};
use crate::typ;
use crate::operand::OperandMode;
use crate::universe::Universe;
use crate::importer::{Importer};

// =============================================================================
// ExprInfo - information about untyped expressions
// =============================================================================

/// Stores information about an untyped expression.
#[derive(Debug, Clone)]
pub struct ExprInfo {
    pub is_lhs: bool,
    pub mode: OperandMode,
    pub typ: Option<TypeKey>,
    pub expr: Expr,
}

// =============================================================================
// ObjContext - context for type-checking a specific object
// =============================================================================

/// Context within which the current object is type-checked.
/// Valid only for the duration of type-checking a specific object.
#[derive(Clone, Default)]
pub struct ObjContext {
    /// Package-level declaration whose init expression/function body is checked.
    pub decl: Option<DeclInfoKey>,
    /// Top-most scope for lookups.
    pub scope: Option<ScopeKey>,
    /// If valid, identifiers are looked up as if at position pos.
    pub pos: Option<Pos>,
    /// Value of iota in a constant declaration; None otherwise.
    pub iota: Option<ConstValue>,
    /// Function signature if inside a function; None otherwise.
    pub sig: Option<TypeKey>,
    /// Set of panic call ids (used for termination check).
    pub panics: Option<HashSet<ExprId>>,
    /// Set if a function makes use of labels.
    pub has_label: bool,
    /// Set if an expression contains a function call or channel receive.
    pub has_call_or_recv: bool,
}

impl ObjContext {
    pub(crate) fn new() -> ObjContext {
        ObjContext::default()
    }
}

/// Delayed action to be executed later during type checking.
pub type DelayedAction = Box<dyn FnOnce(&mut Checker)>;

// =============================================================================
// Checker - the main type checker
// =============================================================================

/// Import key for caching imported packages.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ImportKey {
    pub path: String,
    pub dir: String,
}

impl ImportKey {
    pub(crate) fn new(path: &str, dir: &str) -> ImportKey {
        ImportKey {
            path: path.to_string(),
            dir: dir.to_string(),
        }
    }
}

/// The main type checker.
pub struct Checker {
    /// Type checking objects container.
    pub tc_objs: TCObjects,
    /// Symbol interner for resolving identifiers.
    pub interner: SymbolInterner,
    /// Diagnostics collector (interior mutability for &self error reporting).
    pub diagnostics: RefCell<DiagnosticSink>,
    /// Current package being checked.
    pub pkg: PackageKey,
    /// Maps package-level objects and methods to declaration info.
    pub obj_map: HashMap<ObjKey, DeclInfoKey>,
    /// Import cache: maps (path, dir) to imported package.
    pub imp_map: HashMap<ImportKey, PackageKey>,
    /// Object context.
    pub octx: ObjContext,
    /// Type checking results.
    pub result: TypeInfo,
    /// Enable debug tracing (set via VO_TRACE env var).
    pub trace_enabled: bool,
    /// Current trace indentation level.
    pub trace_indent: Rc<RefCell<usize>>,
    
    // --- Per-check state ---
    /// Positions of unused dot-imported packages for each file scope.
    pub unused_dot_imports: HashMap<ScopeKey, HashMap<PackageKey, Span>>,
    /// Maps package scope type names to associated non-blank, non-interface methods.
    pub methods: HashMap<ObjKey, Vec<ObjKey>>,
    /// Maps interface type names to corresponding interface infos.
    pub ifaces: HashMap<ObjKey, Option<super::interface::RcIfaceInfo>>,
    /// Map of expressions without final type.
    pub untyped: HashMap<ExprId, ExprInfo>,
    /// Stack of delayed actions.
    pub delayed: Vec<DelayedAction>,
    /// Path of object dependencies during type inference (for cycle reporting).
    pub obj_path: Vec<ObjKey>,
}

impl Checker {
    /// Creates a new type checker for the given package.
    pub(crate) fn new(pkg: PackageKey, interner: SymbolInterner) -> Checker {
        Self::new_with_trace(pkg, interner, false)
    }

    /// Creates a new type checker with trace option.
    pub(crate) fn new_with_trace(pkg: PackageKey, interner: SymbolInterner, trace_enabled: bool) -> Checker {
        let tc_objs = TCObjects::new();
        Checker {
            tc_objs,
            interner,
            diagnostics: RefCell::new(DiagnosticSink::new()),
            pkg,
            obj_map: HashMap::new(),
            imp_map: HashMap::new(),
            octx: ObjContext::new(),
            result: TypeInfo::new(),
            trace_enabled,
            trace_indent: Rc::new(RefCell::new(0)),
            unused_dot_imports: HashMap::new(),
            methods: HashMap::new(),
            ifaces: HashMap::new(),
            untyped: HashMap::new(),
            delayed: Vec::new(),
            obj_path: Vec::new(),
        }
    }
    
    /// Creates a new type checker with pre-existing TCObjects.
    pub(crate) fn with_objs(
        pkg: PackageKey,
        interner: SymbolInterner,
        tc_objs: TCObjects,
        trace_enabled: bool,
    ) -> Checker {
        Checker {
            tc_objs,
            interner,
            diagnostics: RefCell::new(DiagnosticSink::new()),
            pkg,
            obj_map: HashMap::new(),
            imp_map: HashMap::new(),
            octx: ObjContext::new(),
            result: TypeInfo::new(),
            trace_enabled,
            trace_indent: Rc::new(RefCell::new(0)),
            unused_dot_imports: HashMap::new(),
            methods: HashMap::new(),
            ifaces: HashMap::new(),
            untyped: HashMap::new(),
            delayed: Vec::new(),
            obj_path: Vec::new(),
        }
    }
    
    /// Take ownership of tc_objs (for sharing between checkers).
    pub(crate) fn take_objs(&mut self) -> TCObjects {
        std::mem::take(&mut self.tc_objs)
    }
    
    /// Put tc_objs back.
    pub(crate) fn put_objs(&mut self, objs: TCObjects) {
        self.tc_objs = objs;
    }

    /// Resolves a symbol to its string.
    pub(crate) fn resolve_symbol(&self, symbol: vo_common::symbol::Symbol) -> &str {
        self.interner.resolve(symbol).unwrap_or("<unknown>")
    }

    /// Resolves an identifier to its string.
    pub(crate) fn resolve_ident(&self, ident: &vo_syntax::ast::Ident) -> &str {
        self.resolve_symbol(ident.symbol)
    }

    /// Returns the universe.
    pub(crate) fn universe(&self) -> &Universe {
        self.tc_objs.universe()
    }

    /// Returns a reference to the TCObjects.
    pub(crate) fn objects(&self) -> &TCObjects {
        &self.tc_objs
    }

    /// Returns a mutable reference to the TCObjects.
    pub(crate) fn objects_mut(&mut self) -> &mut TCObjects {
        &mut self.tc_objs
    }

    /// Returns the type checking results.
    pub(crate) fn type_info(&self) -> &TypeInfo {
        &self.result
    }

    /// Returns a mutable reference to the type checking results.
    pub(crate) fn type_info_mut(&mut self) -> &mut TypeInfo {
        &mut self.result
    }

    /// Emit a diagnostic.
    pub(crate) fn emit(&self, diagnostic: Diagnostic) {
        self.diagnostics.borrow_mut().emit(diagnostic);
    }

    /// Report an error with a TypeError code.
    pub(crate) fn error_code(&self, code: TypeError, span: Span) {
        self.emit(code.at(span));
    }

    /// Report an error with a TypeError code and custom message.
    pub(crate) fn error_code_msg(&self, code: TypeError, span: Span, msg: impl Into<String>) {
        self.emit(code.at_with_message(span, msg));
    }

    /// Returns true if any errors were emitted.
    pub(crate) fn has_errors(&self) -> bool {
        self.diagnostics.borrow().has_errors()
    }

    /// Returns the error count.
    pub(crate) fn error_count(&self) -> usize {
        self.diagnostics.borrow().error_count()
    }

    /// Format a position for error messages.
    pub(crate) fn position(&self, pos: usize) -> String {
        format!("pos:{}", pos)
    }

    /// Returns whether tracing is enabled.
    pub(crate) fn trace(&self) -> bool {
        self.trace_enabled
    }

    /// Print a trace message with current indentation.
    fn trace_print(&self, msg: &str) {
        let indent = *self.trace_indent.borrow();
        let prefix = ".  ".repeat(indent);
        eprintln!("{}{}", prefix, msg);
    }

    /// Begin a trace block for an expression.
    pub(crate) fn trace_expr(&self, expr: &vo_syntax::ast::Expr) {
        if self.trace_enabled {
            let s = super::format::format_expr(expr, &self.interner);
            self.trace_print(&format!("expr[ {} ]", s));
            *self.trace_indent.borrow_mut() += 1;
        }
    }

    /// End a trace block for an expression.
    pub(crate) fn trace_expr_end(&self) {
        if self.trace_enabled {
            let indent = &mut *self.trace_indent.borrow_mut();
            *indent = indent.saturating_sub(1);
        }
    }

    /// Begin a trace block for a statement.
    pub(crate) fn trace_stmt(&self, stmt: &vo_syntax::ast::Stmt) {
        if self.trace_enabled {
            let s = super::format::format_stmt(stmt, &self.interner);
            self.trace_print(&format!("stmt[ {} ]", s));
            *self.trace_indent.borrow_mut() += 1;
        }
    }

    /// End a trace block for a statement.
    pub(crate) fn trace_stmt_end(&self) {
        if self.trace_enabled {
            let indent = &mut *self.trace_indent.borrow_mut();
            *indent = indent.saturating_sub(1);
        }
    }

    // =========================================================================
    // Object/Scope accessors (used by resolver and other modules)
    // =========================================================================

    /// Returns a reference to a package.
    pub(crate) fn package(&self, key: PackageKey) -> &crate::package::Package {
        &self.tc_objs.pkgs[key]
    }

    /// Returns a mutable reference to a package.
    pub(crate) fn package_mut(&mut self, key: PackageKey) -> &mut crate::package::Package {
        &mut self.tc_objs.pkgs[key]
    }

    /// Returns a reference to a scope.
    pub(crate) fn scope(&self, key: ScopeKey) -> &crate::scope::Scope {
        &self.tc_objs.scopes[key]
    }

    /// Returns a mutable reference to a scope.
    pub(crate) fn scope_mut(&mut self, key: ScopeKey) -> &mut crate::scope::Scope {
        &mut self.tc_objs.scopes[key]
    }


    /// Returns a reference to a declaration info.
    pub(crate) fn decl_info(&self, key: DeclInfoKey) -> &super::resolver::DeclInfo {
        &self.tc_objs.decls[key]
    }

    /// Returns a mutable reference to a declaration info.
    pub(crate) fn decl_info_mut(&mut self, key: DeclInfoKey) -> &mut super::resolver::DeclInfo {
        &mut self.tc_objs.decls[key]
    }

    // Note: report_alt_decl and add_method_decls are in decl.rs

    // =========================================================================
    // Per-check state management
    // =========================================================================

    /// Add an unused dot import.
    pub(crate) fn add_unused_dot_import(&mut self, scope: ScopeKey, pkg: PackageKey, span: Span) {
        self.unused_dot_imports
            .entry(scope)
            .or_default()
            .insert(pkg, span);
    }

    /// Remember an untyped expression.
    pub(crate) fn remember_untyped(&mut self, expr_id: ExprId, info: ExprInfo) {
        self.untyped.insert(expr_id, info);
    }

    /// Push a delayed action onto the stack.
    pub(crate) fn later(&mut self, action: DelayedAction) {
        self.delayed.push(action);
    }

    /// Returns the count of delayed actions.
    pub(crate) fn delayed_count(&self) -> usize {
        self.delayed.len()
    }

    /// Process delayed actions starting from index `top`.
    pub(crate) fn process_delayed(&mut self, top: usize) {
        let actions: Vec<DelayedAction> = self.delayed.drain(top..).collect();
        for action in actions {
            action(self);
        }
    }

    /// Push an object onto the dependency path.
    pub(crate) fn push_obj_path(&mut self, obj: ObjKey) -> usize {
        self.obj_path.push(obj);
        self.obj_path.len() - 1
    }

    /// Pop an object from the dependency path.
    pub(crate) fn pop_obj_path(&mut self) -> ObjKey {
        self.obj_path.pop().unwrap()
    }

    // =========================================================================
    // Expression checking - delegated to expr.rs
    // =========================================================================

    /// Raw expression type-checking - delegates to raw_expr_impl in expr.rs.
    pub(crate) fn raw_expr(
        &mut self,
        x: &mut crate::operand::Operand,
        expr: &Expr,
        hint: Option<TypeKey>,
    ) {
        self.raw_expr_impl(x, expr, hint);
    }

    // =========================================================================
    // Main entry point
    // =========================================================================

    /// Main entry point for type checking a set of files.
    pub(crate) fn check(&mut self, files: &[File]) -> Result<PackageKey, ()> {
        self.check_files_pkg_name(files)?;
        self.collect_objects(files, None);
        self.package_objects();
        self.process_delayed(0);
        self.init_order();
        self.unused_imports();
        self.record_untyped();
        
        // Escape analysis pass
        let escape_result = super::escape::analyze(files, &self.result, &self.tc_objs);
        self.result.escaped_vars = escape_result.escaped;
        self.result.closure_captures = escape_result.closure_captures;
        
        if self.has_errors() {
            Err(())
        } else {
            Ok(self.pkg)
        }
    }
    
    /// Type check files with an importer for handling imports.
    pub(crate) fn check_with_importer(
        &mut self,
        files: &[File],
        importer: &mut dyn Importer,
    ) -> Result<PackageKey, ()> {
        self.check_files_pkg_name(files)?;
        self.collect_objects(files, Some(importer));
        self.package_objects();
        self.process_delayed(0);
        self.init_order();
        self.unused_imports();
        self.record_untyped();
        
        // Escape analysis pass
        let escape_result = super::escape::analyze(files, &self.result, &self.tc_objs);
        self.result.escaped_vars = escape_result.escaped;
        self.result.closure_captures = escape_result.closure_captures;
        
        if self.has_errors() {
            Err(())
        } else {
            Ok(self.pkg)
        }
    }

    /// Check that all files have the same package name.
    fn check_files_pkg_name(&mut self, files: &[File]) -> Result<(), ()> {
        let mut pkg_name: Option<String> = None;
        for f in files.iter() {
            if let Some(ident) = &f.package {
                let name = self.resolve_ident(ident);
                if pkg_name.is_none() {
                    if name == "_" {
                        self.error_code(TypeError::InvalidPackageName, ident.span);
                        return Err(());
                    } else {
                        pkg_name = Some(name.to_string());
                    }
                } else if name != pkg_name.as_ref().unwrap() {
                    self.error_code_msg(
                        TypeError::PackageNameMismatch,
                        ident.span,
                        format!(
                            "package {}; expected {}",
                            name,
                            pkg_name.as_ref().unwrap()
                        ),
                    );
                    return Err(());
                }
            }
        }
        if let Some(name) = pkg_name {
            self.package_mut(self.pkg).set_name(name);
        }
        Ok(())
    }

    /// Record all untyped expressions in the result.
    /// Converts remaining untyped types to their default typed versions.
    fn record_untyped(&mut self) {
        let untyped: Vec<_> = self.untyped.drain().collect();
        for (id, info) in untyped {
            if info.mode != OperandMode::Invalid {
                if let Some(typ) = info.typ {
                    // Convert untyped to default type before recording
                    let final_typ = typ::untyped_default_type(typ, self.objs());
                    self.result.record_type_and_value(id, info.mode.clone(), final_typ);
                }
            }
        }
    }
}
