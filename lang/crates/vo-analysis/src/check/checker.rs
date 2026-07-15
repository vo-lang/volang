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
use vo_syntax::ast::{BinaryOp, Expr, ExprKind, File};

use super::errors::TypeError;
use super::type_info::TypeInfo;
use crate::constant::{constant_fold_work_bytes, Value, MAX_CONSTANT_FOLD_WORK_BYTES};
use crate::importer::Importer;
use crate::obj::{ConstValue, Pos};
use crate::objects::{DeclInfoKey, ObjKey, PackageKey, ScopeKey, TCObjects, TypeKey};
use crate::operand::OperandMode;
use crate::universe::Universe;

// =============================================================================
// ExprInfo - information about untyped expressions
// =============================================================================

/// Stores information about an untyped expression.
#[derive(Debug, Clone)]
pub(crate) struct ExprInfo {
    pub(crate) is_lhs: bool,
    pub(crate) mode: OperandMode,
    pub(crate) typ: Option<TypeKey>,
    pub(crate) expr: crate::operand::ExprRef,
    pub(crate) shape: UntypedExprShape,
}

/// Compact child information needed when an untyped expression receives its
/// final type. Keeping the full AST here recursively cloned every prefix of a
/// left-deep constant expression.
#[derive(Debug, Clone, Copy)]
pub(crate) enum UntypedExprShape {
    Atom,
    NoUpdate,
    Paren(ExprId),
    Unary(ExprId),
    Binary {
        left: ExprId,
        right: ExprId,
        op: BinaryOp,
    },
}

impl UntypedExprShape {
    pub(crate) fn from_expr(expr: &Expr) -> Self {
        match &expr.kind {
            ExprKind::FuncLit(_)
            | ExprKind::CompositeLit(_)
            | ExprKind::Index(_)
            | ExprKind::Slice(_)
            | ExprKind::TypeAssert(_) => Self::NoUpdate,
            ExprKind::Paren(inner) => Self::Paren(inner.id),
            ExprKind::Unary(unary) => Self::Unary(unary.operand.id),
            ExprKind::Binary(binary) => Self::Binary {
                left: binary.left.id,
                right: binary.right.id,
                op: binary.op,
            },
            _ => Self::Atom,
        }
    }
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
}

impl ImportKey {
    pub(crate) fn new(path: &str) -> ImportKey {
        ImportKey {
            path: path.to_string(),
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
    /// Import cache: maps canonical import path to imported package.
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
    /// Current recursive interface method-set resolution depth.
    pub(crate) interface_resolution_depth: usize,
    /// Map of expressions without final type.
    pub(crate) untyped: HashMap<ExprId, ExprInfo>,
    /// Stack of delayed actions.
    pub delayed: Vec<DelayedAction>,
    /// Path of object dependencies during type inference (for cycle reporting).
    pub obj_path: Vec<ObjKey>,
    /// Aggregate payload processed by constant folding in this package.
    pub(crate) constant_fold_work_bytes: u64,
}

impl Checker {
    /// Creates a new type checker for the given package.
    pub(crate) fn new(pkg: PackageKey, interner: SymbolInterner) -> Checker {
        Self::new_with_trace(pkg, interner, false)
    }

    /// Creates a new type checker with trace option.
    pub fn new_with_trace(
        pkg: PackageKey,
        interner: SymbolInterner,
        trace_enabled: bool,
    ) -> Checker {
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
            interface_resolution_depth: 0,
            untyped: HashMap::new(),
            delayed: Vec::new(),
            obj_path: Vec::new(),
            constant_fold_work_bytes: 0,
        }
    }

    /// Charges deterministic package-level constant-fold work before retaining
    /// the result. Every individual value is already bounded separately.
    pub(crate) fn charge_constant_fold_work(&mut self, span: Span, values: &[&Value]) -> bool {
        let additional = match constant_fold_work_bytes(values) {
            Ok(additional) => additional,
            Err(error) => {
                self.error_code_msg(TypeError::ConstantResourceLimit, span, error.to_string());
                return false;
            }
        };
        let Some(total) = self.constant_fold_work_bytes.checked_add(additional) else {
            self.error_code_msg(
                TypeError::ConstantResourceLimit,
                span,
                format!(
                    "constant-folding work exceeds the per-package limit of {} bytes",
                    MAX_CONSTANT_FOLD_WORK_BYTES
                ),
            );
            return false;
        };
        if total > MAX_CONSTANT_FOLD_WORK_BYTES {
            self.error_code_msg(
                TypeError::ConstantResourceLimit,
                span,
                format!(
                    "constant-folding work exceeds the per-package limit of {} bytes",
                    MAX_CONSTANT_FOLD_WORK_BYTES
                ),
            );
            return false;
        }
        self.constant_fold_work_bytes = total;
        true
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

    /// Emit a diagnostic.
    pub(crate) fn emit(&self, diagnostic: Diagnostic) {
        let mut diagnostics = self.diagnostics.borrow_mut();
        if diagnostics.len() < super::MAX_TYPE_CHECK_DIAGNOSTICS {
            diagnostics.emit(diagnostic);
            return;
        }
        if diagnostics.len() > super::MAX_TYPE_CHECK_DIAGNOSTICS {
            return;
        }
        let span = diagnostic
            .labels
            .first()
            .map(|label| label.span)
            .unwrap_or_else(Span::dummy);
        diagnostics.emit(TypeError::DiagnosticLimitExceeded.at_with_message(
            span,
            format!(
                "type-check diagnostic limit of {} reached; further diagnostics suppressed",
                super::MAX_TYPE_CHECK_DIAGNOSTICS
            ),
        ));
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
            .entry(pkg)
            .or_insert(span);
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
        self.constant_fold_work_bytes = 0;
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
        self.result.loop_defined_vars = escape_result.loop_defined_vars;

        // go @(island) sendability post-pass (needs closure_captures from escape analysis)
        let go_island_diags =
            super::go_island::check_go_island_sendability(files, &self.result, &self.tc_objs);
        for diag in go_island_diags {
            self.error_code_msg(diag.code, diag.span, diag.message);
        }

        if self.has_errors() {
            Err(())
        } else {
            Ok(self.pkg)
        }
    }

    /// Type check files with an importer for handling imports.
    #[allow(clippy::result_unit_err)]
    pub fn check_with_importer(
        &mut self,
        files: &[File],
        importer: &mut dyn Importer,
    ) -> Result<PackageKey, ()> {
        self.constant_fold_work_bytes = 0;
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
        self.result.loop_defined_vars = escape_result.loop_defined_vars;

        // go @(island) sendability post-pass (needs closure_captures from escape analysis)
        let go_island_diags =
            super::go_island::check_go_island_sendability(files, &self.result, &self.tc_objs);
        for diag in go_island_diags {
            self.error_code_msg(diag.code, diag.span, diag.message);
        }

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
                } else if Some(name) != pkg_name.as_deref() {
                    self.error_code_msg(
                        TypeError::PackageNameMismatch,
                        ident.span,
                        format!(
                            "package {}; expected {}",
                            name,
                            pkg_name.as_deref().unwrap_or("")
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
    /// Uses the type already set in info.typ (which may have been updated by convert_untyped).
    fn record_untyped(&mut self) {
        let untyped: Vec<_> = self.untyped.drain().collect();
        for (id, info) in untyped {
            if info.mode != OperandMode::Invalid {
                if let Some(typ) = info.typ {
                    // Use the type as-is - it may have been updated by convert_untyped
                    self.result
                        .record_type_and_value(id, info.mode.clone(), typ);
                }
            }
        }
    }
}
