//! The type checker.
//!
//! This module contains the main Checker struct and related context types
//! for type checking GoX source code.

#![allow(dead_code)]

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::marker::PhantomData;
use std::rc::Rc;

use gox_common::span::Span;
use gox_common::symbol::SymbolInterner;
use gox_common::vfs::FileSystem;
use gox_common_core::ExprId;
use gox_syntax::ast::{Expr, File};

use super::type_info::TypeInfo;
use crate::obj::{ConstValue, LangObj, Pos};
use crate::objects::{DeclInfoKey, ObjKey, PackageKey, ScopeKey, TCObjects, TypeKey};
use crate::operand::OperandMode;
use crate::universe::Universe;

// =============================================================================
// ExprInfo - information about untyped expressions
// =============================================================================

/// Stores information about an untyped expression.
#[derive(Debug, Clone)]
pub struct ExprInfo {
    pub is_lhs: bool,
    pub mode: OperandMode,
    pub typ: Option<TypeKey>,
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
    pub fn new() -> ObjContext {
        ObjContext::default()
    }
}

// =============================================================================
// FilesContext - context for type-checking a set of files
// =============================================================================

/// Delayed action to be executed later during type checking.
pub type DelayedAction<F> = Box<dyn FnOnce(&mut Checker<F>, &mut FilesContext<F>)>;

/// Contains information collected during type-checking of a set of package files.
pub struct FilesContext<'a, F: FileSystem> {
    /// Package files.
    pub files: &'a [File],
    /// Positions of unused dot-imported packages for each file scope.
    pub unused_dot_imports: HashMap<ScopeKey, HashMap<PackageKey, Span>>,
    /// Maps package scope type names to associated non-blank, non-interface methods.
    pub methods: HashMap<ObjKey, Vec<ObjKey>>,
    /// Maps interface type names to corresponding interface infos.
    pub ifaces: HashMap<ObjKey, Option<super::interface::RcIfaceInfo>>,
    /// Map of expressions without final type.
    pub untyped: HashMap<ExprId, ExprInfo>,
    /// Stack of delayed actions.
    pub delayed: Vec<DelayedAction<F>>,
    /// Path of object dependencies during type inference (for cycle reporting).
    pub obj_path: Vec<ObjKey>,
    /// Phantom data for the file system type.
    _phantom: PhantomData<F>,
}

impl<'a, F: FileSystem> FilesContext<'a, F> {
    pub fn new(files: &'a [File]) -> FilesContext<'a, F> {
        FilesContext {
            files,
            unused_dot_imports: HashMap::new(),
            methods: HashMap::new(),
            ifaces: HashMap::new(),
            untyped: HashMap::new(),
            delayed: Vec::new(),
            obj_path: Vec::new(),
            _phantom: PhantomData,
        }
    }

    /// Add an unused dot import.
    pub fn add_unused_dot_import(&mut self, scope: ScopeKey, pkg: PackageKey, span: Span) {
        self.unused_dot_imports
            .entry(scope)
            .or_default()
            .insert(pkg, span);
    }

    /// Remember an untyped expression.
    pub fn remember_untyped(&mut self, expr_id: ExprId, info: ExprInfo) {
        self.untyped.insert(expr_id, info);
    }

    /// Push a delayed action onto the stack.
    pub fn later(&mut self, action: DelayedAction<F>) {
        self.delayed.push(action);
    }

    /// Returns the count of delayed actions.
    pub fn delayed_count(&self) -> usize {
        self.delayed.len()
    }

    /// Process delayed actions starting from index `top`.
    pub fn process_delayed(&mut self, top: usize, checker: &mut Checker<F>) {
        let actions: Vec<DelayedAction<F>> = self.delayed.drain(top..).collect();
        for action in actions {
            action(checker, self);
        }
    }

    /// Push an object onto the dependency path.
    pub fn push(&mut self, obj: ObjKey) -> usize {
        self.obj_path.push(obj);
        self.obj_path.len() - 1
    }

    /// Pop an object from the dependency path.
    pub fn pop(&mut self) -> ObjKey {
        self.obj_path.pop().unwrap()
    }
}

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
    pub fn new(path: &str, dir: &str) -> ImportKey {
        ImportKey {
            path: path.to_string(),
            dir: dir.to_string(),
        }
    }
}

/// The main type checker.
pub struct Checker<F: FileSystem> {
    /// Type checking objects container.
    pub tc_objs: TCObjects,
    /// Universe (predefined types and functions).
    universe: Universe,
    /// Symbol interner for resolving identifiers.
    pub interner: SymbolInterner,
    /// Error messages.
    errors: Vec<(Span, String)>,
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
    /// For debug tracing.
    pub indent: Rc<RefCell<usize>>,
    /// Phantom data for the file system type.
    _phantom: PhantomData<F>,
}

impl<F: FileSystem> Checker<F> {
    /// Creates a new type checker for the given package.
    pub fn new(pkg: PackageKey, interner: SymbolInterner) -> Checker<F> {
        let mut tc_objs = TCObjects::new();
        let universe = Universe::new(&mut tc_objs);
        Checker {
            tc_objs,
            universe,
            interner,
            errors: Vec::new(),
            pkg,
            obj_map: HashMap::new(),
            imp_map: HashMap::new(),
            octx: ObjContext::new(),
            result: TypeInfo::new(),
            indent: Rc::new(RefCell::new(0)),
            _phantom: PhantomData,
        }
    }

    /// Resolves a symbol to its string.
    pub fn resolve_symbol(&self, symbol: gox_common::symbol::Symbol) -> &str {
        self.interner.resolve(symbol).unwrap_or("<unknown>")
    }

    /// Resolves an identifier to its string.
    pub fn resolve_ident(&self, ident: &gox_common::symbol::Ident) -> &str {
        self.resolve_symbol(ident.symbol)
    }

    /// Returns the universe.
    pub fn universe(&self) -> &Universe {
        &self.universe
    }

    /// Returns a reference to the TCObjects.
    pub fn objects(&self) -> &TCObjects {
        &self.tc_objs
    }

    /// Returns a mutable reference to the TCObjects.
    pub fn objects_mut(&mut self) -> &mut TCObjects {
        &mut self.tc_objs
    }

    /// Returns the type checking results.
    pub fn type_info(&self) -> &TypeInfo {
        &self.result
    }

    /// Returns a mutable reference to the type checking results.
    pub fn type_info_mut(&mut self) -> &mut TypeInfo {
        &mut self.result
    }

    /// Report an error.
    pub fn error(&self, span: Span, msg: String) {
        // TODO: Proper error handling
        eprintln!("error at {:?}: {}", span, msg);
    }

    /// Report an error with a string message (convenience method).
    pub fn error_str(&self, pos: usize, msg: &str) {
        eprintln!("error at pos {}: {}", pos, msg);
    }

    /// Report a soft error (warning-like).
    pub fn soft_error(&self, span: Span, msg: String) {
        eprintln!("warning at {:?}: {}", span, msg);
    }

    /// Format a position for error messages.
    pub fn position(&self, pos: usize) -> String {
        format!("pos:{}", pos)
    }

    /// Returns whether tracing is enabled.
    pub fn trace(&self) -> bool {
        false
    }

    /// Begin a trace block.
    pub fn trace_begin(&self, _pos: usize, _msg: &str) {
        // TODO: Implement tracing
    }

    /// End a trace block.
    pub fn trace_end(&self, _pos: usize, _msg: &str) {
        // TODO: Implement tracing
    }

    // =========================================================================
    // Object/Scope accessors (used by resolver and other modules)
    // =========================================================================

    /// Returns a reference to a package.
    pub fn package(&self, key: PackageKey) -> &crate::package::Package {
        &self.tc_objs.pkgs[key]
    }

    /// Returns a mutable reference to a package.
    pub fn package_mut(&mut self, key: PackageKey) -> &mut crate::package::Package {
        &mut self.tc_objs.pkgs[key]
    }

    /// Returns a reference to a scope.
    pub fn scope(&self, key: ScopeKey) -> &crate::scope::Scope {
        &self.tc_objs.scopes[key]
    }

    /// Returns a mutable reference to a scope.
    pub fn scope_mut(&mut self, key: ScopeKey) -> &mut crate::scope::Scope {
        &mut self.tc_objs.scopes[key]
    }


    /// Returns a reference to a declaration info.
    pub fn decl_info(&self, key: DeclInfoKey) -> &super::resolver::DeclInfo {
        &self.tc_objs.decls[key]
    }

    /// Returns a mutable reference to a declaration info.
    pub fn decl_info_mut(&mut self, key: DeclInfoKey) -> &mut super::resolver::DeclInfo {
        &mut self.tc_objs.decls[key]
    }

    // Note: report_alt_decl and add_method_decls are in decl.rs

    // =========================================================================
    // Expression checking stubs (to be implemented in expr.rs)
    // =========================================================================

    /// Type-check an expression that may have multiple values (tuple).
    pub fn multi_expr(&mut self, x: &mut crate::operand::Operand, expr: &Expr) {
        self.raw_expr(x, expr, None);
    }

    /// Raw expression type-checking.
    pub fn raw_expr(
        &mut self,
        x: &mut crate::operand::Operand,
        expr: &Expr,
        hint: Option<TypeKey>,
    ) {
        // TODO: Implement in expr.rs
        x.mode = crate::operand::OperandMode::Invalid;
        x.expr_id = Some(expr.id);
        x.typ = None;
        let _ = hint;
    }

    /// Updates the type of a previously type-checked expression.
    /// Used to update the type of untyped expressions once the context provides
    /// enough information to determine their final type.
    pub fn update_expr_type(
        &mut self,
        _expr_id: ExprId,
        _typ: TypeKey,
        _final_: bool,
        _fctx: &mut FilesContext<F>,
    ) {
        // TODO: Implement - update untyped expression to final type
    }
}
