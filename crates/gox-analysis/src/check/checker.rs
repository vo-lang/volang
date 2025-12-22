//! The type checker.
//!
//! This module contains the main Checker struct and related context types
//! for type checking GoX source code.

#![allow(dead_code)]

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use gox_common::span::Span;
use gox_common::symbol::SymbolInterner;
use gox_common_core::ExprId;
use gox_syntax::ast::{Expr, File};

use super::type_info::TypeInfo;
use crate::obj::{ConstValue, Pos};
use crate::objects::{DeclInfoKey, ObjKey, PackageKey, ScopeKey, TCObjects, TypeKey};
use crate::operand::OperandMode;
use crate::universe::Universe;
use crate::importer::{Importer, ImportResult};

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
    pub fn new() -> ObjContext {
        ObjContext::default()
    }
}

// =============================================================================
// FilesContext - context for type-checking a set of files
// =============================================================================

/// Delayed action to be executed later during type checking.
pub type DelayedAction = Box<dyn FnOnce(&mut Checker)>;

/// Simplified context for type-checking - only holds borrowed data.
pub struct FilesContext<'a> {
    /// Package files.
    pub files: &'a [File],
    /// Optional importer for resolving package imports.
    pub importer: Option<&'a mut dyn Importer>,
}

impl<'a> FilesContext<'a> {
    pub fn new(files: &'a [File]) -> FilesContext<'a> {
        FilesContext {
            files,
            importer: None,
        }
    }
    
    pub fn with_importer(files: &'a [File], importer: &'a mut dyn Importer) -> FilesContext<'a> {
        FilesContext {
            files,
            importer: Some(importer),
        }
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
pub struct Checker {
    /// Type checking objects container.
    pub tc_objs: TCObjects,
    /// Symbol interner for resolving identifiers.
    pub interner: SymbolInterner,
    /// Error messages.
    pub errors: Vec<(Span, String)>,
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
    
    // --- Fields moved from FilesContext ---
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
    pub fn new(pkg: PackageKey, interner: SymbolInterner) -> Checker {
        let tc_objs = TCObjects::new();
        Checker {
            tc_objs,
            interner,
            errors: Vec::new(),
            pkg,
            obj_map: HashMap::new(),
            imp_map: HashMap::new(),
            octx: ObjContext::new(),
            result: TypeInfo::new(),
            indent: Rc::new(RefCell::new(0)),
            unused_dot_imports: HashMap::new(),
            methods: HashMap::new(),
            ifaces: HashMap::new(),
            untyped: HashMap::new(),
            delayed: Vec::new(),
            obj_path: Vec::new(),
        }
    }
    
    /// Creates a new type checker with pre-existing TCObjects.
    pub fn with_objs(
        pkg: PackageKey,
        interner: SymbolInterner,
        tc_objs: TCObjects,
    ) -> Checker {
        Checker {
            tc_objs,
            interner,
            errors: Vec::new(),
            pkg,
            obj_map: HashMap::new(),
            imp_map: HashMap::new(),
            octx: ObjContext::new(),
            result: TypeInfo::new(),
            indent: Rc::new(RefCell::new(0)),
            unused_dot_imports: HashMap::new(),
            methods: HashMap::new(),
            ifaces: HashMap::new(),
            untyped: HashMap::new(),
            delayed: Vec::new(),
            obj_path: Vec::new(),
        }
    }
    
    /// Take ownership of tc_objs (for sharing between checkers).
    pub fn take_objs(&mut self) -> TCObjects {
        std::mem::take(&mut self.tc_objs)
    }
    
    /// Put tc_objs back.
    pub fn put_objs(&mut self, objs: TCObjects) {
        self.tc_objs = objs;
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
        self.tc_objs.universe()
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
    // Methods moved from FilesContext
    // =========================================================================

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
    pub fn later(&mut self, action: DelayedAction) {
        self.delayed.push(action);
    }

    /// Returns the count of delayed actions.
    pub fn delayed_count(&self) -> usize {
        self.delayed.len()
    }

    /// Process delayed actions starting from index `top`.
    pub fn process_delayed(&mut self, top: usize) {
        let actions: Vec<DelayedAction> = self.delayed.drain(top..).collect();
        for action in actions {
            action(self);
        }
    }

    /// Push an object onto the dependency path.
    pub fn push_obj_path(&mut self, obj: ObjKey) -> usize {
        self.obj_path.push(obj);
        self.obj_path.len() - 1
    }

    /// Pop an object from the dependency path.
    pub fn pop_obj_path(&mut self) -> ObjKey {
        self.obj_path.pop().unwrap()
    }

    // =========================================================================
    // Expression checking - delegated to expr.rs
    // =========================================================================

    /// Raw expression type-checking - delegates to raw_expr_impl in expr.rs.
    pub fn raw_expr(
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
    pub fn check(&mut self, files: &[File]) -> Result<PackageKey, ()> {
        self.check_files_pkg_name(files)?;
        let fctx = &mut FilesContext::new(files);
        self.collect_objects(fctx);
        self.package_objects(fctx);
        self.process_delayed(0);
        self.init_order();
        self.unused_imports(fctx);
        self.record_untyped();
        Ok(self.pkg)
    }
    
    /// Type check files with an importer for handling imports.
    pub fn check_with_importer(
        &mut self,
        files: &[File],
        importer: &mut dyn Importer,
    ) -> Result<PackageKey, ()> {
        self.check_files_pkg_name(files)?;
        let fctx = &mut FilesContext::with_importer(files, importer);
        self.collect_objects(fctx);
        self.package_objects(fctx);
        self.process_delayed(0);
        self.init_order();
        self.unused_imports(fctx);
        self.record_untyped();
        Ok(self.pkg)
    }

    /// Check that all files have the same package name.
    fn check_files_pkg_name(&mut self, files: &[File]) -> Result<(), ()> {
        let mut pkg_name: Option<String> = None;
        for f in files.iter() {
            if let Some(ident) = &f.package {
                let name = self.resolve_ident(ident);
                if pkg_name.is_none() {
                    if name == "_" {
                        self.error(ident.span, "invalid package name _".to_string());
                        return Err(());
                    } else {
                        pkg_name = Some(name.to_string());
                    }
                } else if name != pkg_name.as_ref().unwrap() {
                    self.error(
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
    fn record_untyped(&mut self) {
        let untyped: Vec<_> = self.untyped.drain().collect();
        for (id, info) in untyped {
            if info.mode != OperandMode::Invalid {
                if let Some(typ) = info.typ {
                    self.result.record_type_and_value(id, info.mode.clone(), typ);
                }
            }
        }
    }
}
