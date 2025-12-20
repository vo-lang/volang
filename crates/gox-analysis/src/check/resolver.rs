//! Declaration resolver - collects and organizes package-level declarations.
//!
//! This module handles the first pass of type checking: collecting all
//! package-level declarations and organizing them for later type checking.

#![allow(dead_code)]

use std::collections::HashSet;

use gox_common::span::Span;
use gox_common::vfs::FileSystem;
use gox_common_core::ExprId;
use gox_syntax::ast::{Decl, File};

use crate::constant::Value;
use crate::obj::LangObj;
use crate::objects::{ObjKey, PackageKey, ScopeKey};

use super::checker::{Checker, FilesContext};

/// NodeId for referencing AST nodes - uses u32 to be compatible with both ExprId and TypeExprId.
pub type NodeId = u32;

/// DeclInfo for const declarations.
#[derive(Debug)]
pub struct DeclInfoConst {
    pub file_scope: ScopeKey,
    pub typ_node: Option<NodeId>,
    pub init_node: Option<NodeId>,
    pub deps: HashSet<ObjKey>,
}

/// DeclInfo for var declarations.
#[derive(Debug)]
pub struct DeclInfoVar {
    pub file_scope: ScopeKey,
    pub lhs: Option<Vec<ObjKey>>,
    pub typ_node: Option<NodeId>,
    pub init_node: Option<NodeId>,
    pub deps: HashSet<ObjKey>,
}

/// DeclInfo for type declarations.
#[derive(Debug)]
pub struct DeclInfoType {
    pub file_scope: ScopeKey,
    pub typ_node: NodeId,
    pub alias: bool,
}

/// DeclInfo for func declarations.
#[derive(Debug)]
pub struct DeclInfoFunc {
    pub file_scope: ScopeKey,
    pub func_node: NodeId,
    pub deps: HashSet<ObjKey>,
}

/// DeclInfo describes a package-level const, type, var, or func declaration.
#[derive(Debug)]
pub enum DeclInfo {
    Const(DeclInfoConst),
    Var(DeclInfoVar),
    Type(DeclInfoType),
    Func(DeclInfoFunc),
}

impl DeclInfo {
    pub fn new_const(
        file_scope: ScopeKey,
        typ_node: Option<NodeId>,
        init_node: Option<NodeId>,
    ) -> DeclInfo {
        DeclInfo::Const(DeclInfoConst {
            file_scope,
            typ_node,
            init_node,
            deps: HashSet::new(),
        })
    }

    pub fn new_var(
        file_scope: ScopeKey,
        lhs: Option<Vec<ObjKey>>,
        typ_node: Option<NodeId>,
        init_node: Option<NodeId>,
    ) -> DeclInfo {
        DeclInfo::Var(DeclInfoVar {
            file_scope,
            lhs,
            typ_node,
            init_node,
            deps: HashSet::new(),
        })
    }

    pub fn new_type(file_scope: ScopeKey, typ_node: NodeId, alias: bool) -> DeclInfo {
        DeclInfo::Type(DeclInfoType {
            file_scope,
            typ_node,
            alias,
        })
    }

    pub fn new_func(file_scope: ScopeKey, func_node: NodeId) -> DeclInfo {
        DeclInfo::Func(DeclInfoFunc {
            file_scope,
            func_node,
            deps: HashSet::new(),
        })
    }

    pub fn as_const(&self) -> Option<&DeclInfoConst> {
        match self {
            DeclInfo::Const(c) => Some(c),
            _ => None,
        }
    }

    pub fn as_var(&self) -> Option<&DeclInfoVar> {
        match self {
            DeclInfo::Var(v) => Some(v),
            _ => None,
        }
    }

    pub fn as_type(&self) -> Option<&DeclInfoType> {
        match self {
            DeclInfo::Type(t) => Some(t),
            _ => None,
        }
    }

    pub fn as_func(&self) -> Option<&DeclInfoFunc> {
        match self {
            DeclInfo::Func(f) => Some(f),
            _ => None,
        }
    }

    pub fn file_scope(&self) -> ScopeKey {
        match self {
            DeclInfo::Const(c) => c.file_scope,
            DeclInfo::Var(v) => v.file_scope,
            DeclInfo::Type(t) => t.file_scope,
            DeclInfo::Func(f) => f.file_scope,
        }
    }

    pub fn deps(&self) -> Option<&HashSet<ObjKey>> {
        match self {
            DeclInfo::Const(c) => Some(&c.deps),
            DeclInfo::Var(v) => Some(&v.deps),
            DeclInfo::Func(f) => Some(&f.deps),
            DeclInfo::Type(_) => None,
        }
    }

    pub fn deps_mut(&mut self) -> Option<&mut HashSet<ObjKey>> {
        match self {
            DeclInfo::Const(c) => Some(&mut c.deps),
            DeclInfo::Var(v) => Some(&mut v.deps),
            DeclInfo::Func(f) => Some(&mut f.deps),
            DeclInfo::Type(_) => None,
        }
    }

    /// Add a dependency to this declaration.
    pub fn add_dep(&mut self, okey: ObjKey) {
        if let Some(deps) = self.deps_mut() {
            deps.insert(okey);
        }
    }

    /// Check if this declaration has dependencies.
    pub fn has_deps(&self) -> bool {
        self.deps().map_or(false, |d| !d.is_empty())
    }
}

// =============================================================================
// Checker resolver methods
// =============================================================================

impl<F: FileSystem> Checker<F> {
    /// Collects all package-level declarations from the files.
    /// This is the first pass of type checking.
    pub fn collect_objects(&mut self, fctx: &mut FilesContext<F>) {
        // Track all imported packages
        let mut all_imported: HashSet<PackageKey> = self
            .package(self.pkg)
            .imports()
            .iter()
            .copied()
            .collect();

        // List of methods with non-blank names
        let mut methods: Vec<ObjKey> = Vec::new();

        for (file_num, file) in fctx.files.iter().enumerate() {
            // Create file scope
            let parent_scope = Some(*self.package(self.pkg).scope());
            let file_scope = self.tc_objs.new_scope(
                parent_scope,
                0, // pos
                0, // end
                &format!("file{}", file_num),
                false,
            );
            // TODO: record_scope needs ExprId, skipping for now

            // Process imports
            for import in &file.imports {
                self.collect_import(import, file_scope, &mut all_imported, fctx);
            }

            // Process declarations
            for decl in &file.decls {
                self.collect_decl_objects(decl, file_scope, &mut methods);
            }
        }

        // Associate methods with receiver base types
        for method_key in methods {
            self.associate_method(method_key, fctx);
        }
    }

    /// Collects import declaration.
    fn collect_import(
        &mut self,
        import: &gox_syntax::ast::ImportDecl,
        file_scope: ScopeKey,
        all_imported: &mut HashSet<PackageKey>,
        fctx: &mut FilesContext<F>,
    ) {
        let path = &import.path.value;
        
        // Import the package
        let imp = self.import_package(path, import.span);

        // Add to list of explicit imports
        if !all_imported.contains(&imp) {
            all_imported.insert(imp);
            self.package_mut(self.pkg).add_import(imp);
        }

        // Determine the local name
        let name = if let Some(alias) = &import.alias {
            let alias_name = self.resolve_ident(alias);
            if alias_name == "init" {
                self.error(import.span, "cannot declare init - must be func".to_string());
            }
            alias_name.to_string()
        } else {
            // Use package name
            self.package(imp).name().clone().unwrap_or_else(|| path.clone())
        };

        // Create package name object
        let pkg_name_obj = self.tc_objs.new_pkg_name(
            0, // pos
            Some(self.pkg),
            name.clone(),
            imp,
        );

        // Record definition
        if let Some(alias) = &import.alias {
            self.result.record_def(alias.clone(), Some(pkg_name_obj));
        } else {
            // TODO: record_implicit needs ExprId, skipping for now
            let _ = pkg_name_obj;
        }

        // Add import to file scope
        if name == "." {
            // Dot import: merge imported scope with file scope
            let pkg_scope = *self.package(imp).scope();
            let elems: Vec<ObjKey> = self
                .scope(pkg_scope)
                .elems()
                .iter()
                .filter_map(|(_, &v)| {
                    if self.lobj(v).exported() {
                        Some(v)
                    } else {
                        None
                    }
                })
                .collect();
            for elem in elems {
                self.declare(file_scope, elem);
            }
            fctx.add_unused_dot_import(file_scope, imp, import.span);
        } else {
            // Regular import
            self.declare(file_scope, pkg_name_obj);
        }
    }

    /// Collects declaration objects (const, var, type, func).
    fn collect_decl_objects(
        &mut self,
        decl: &Decl,
        file_scope: ScopeKey,
        methods: &mut Vec<ObjKey>,
    ) {
        match decl {
            Decl::Const(const_decl) => {
                let mut last_typ: Option<NodeId> = None;
                let mut last_values: &[gox_syntax::ast::Expr] = &[];

                for (iota, spec) in const_decl.specs.iter().enumerate() {
                    // Update last type and values if this spec has them
                    if spec.ty.is_some() || !spec.values.is_empty() {
                        last_typ = spec.ty.as_ref().map(|t| t.id.0 as NodeId);
                        last_values = &spec.values;
                    }

                    for (i, name) in spec.names.iter().enumerate() {
                        let name_str = self.resolve_ident(name);
                        let okey = self.tc_objs.new_const(
                            0, // pos
                            Some(self.pkg),
                            name_str.to_string(),
                            None,
                            Value::with_i64(iota as i64),
                        );

                        let init_node = last_values.get(i).map(|e| e.id.0 as NodeId);
                        let di = self.tc_objs.decls.insert(DeclInfo::new_const(
                            file_scope,
                            last_typ,
                            init_node,
                        ));

                        self.declare_pkg_obj(name, okey, di);
                    }
                }
            }
            Decl::Var(var_decl) => {
                for spec in &var_decl.specs {
                    let lhs: Vec<ObjKey> = spec
                        .names
                        .iter()
                        .map(|name| {
                            let name_str = self.resolve_ident(name).to_string();
                            self.tc_objs.new_var(
                                0, // pos
                                Some(self.pkg),
                                name_str,
                                None,
                            )
                        })
                        .collect();

                    let n_to_1 = spec.values.len() == 1 && spec.names.len() > 1;
                    let typ_node = spec.ty.as_ref().map(|t| t.id.0 as NodeId);

                    if n_to_1 {
                        // n:1 assignment
                        let di = self.tc_objs.decls.insert(DeclInfo::new_var(
                            file_scope,
                            Some(lhs.clone()),
                            typ_node,
                            Some(spec.values[0].id.0 as NodeId),
                        ));
                        for (name, &okey) in spec.names.iter().zip(&lhs) {
                            self.declare_pkg_obj(name, okey, di);
                        }
                    } else {
                        // 1:1 or n:n assignment
                        for (i, (name, &okey)) in spec.names.iter().zip(&lhs).enumerate() {
                            let init_node = spec.values.get(i).map(|e| e.id.0 as NodeId);
                            let di = self.tc_objs.decls.insert(DeclInfo::new_var(
                                file_scope,
                                None,
                                typ_node,
                                init_node,
                            ));
                            self.declare_pkg_obj(name, okey, di);
                        }
                    }
                }
            }
            Decl::Type(type_decl) => {
                let name_str = self.resolve_ident(&type_decl.name);
                let okey = self.tc_objs.new_type_name(
                    0, // pos
                    Some(self.pkg),
                    name_str.to_string(),
                    None,
                );

                let di = self.tc_objs.decls.insert(DeclInfo::new_type(
                    file_scope,
                    type_decl.ty.id.0 as NodeId,
                    false, // GoX doesn't have type aliases with =
                ));

                self.declare_pkg_obj(&type_decl.name, okey, di);
            }
            Decl::Func(func_decl) => {
                let name_str = self.resolve_ident(&func_decl.name).to_string();
                let okey = self.tc_objs.new_func(
                    0, // pos
                    Some(self.pkg),
                    name_str.clone(),
                    None,
                );

                if func_decl.receiver.is_none() {
                    // Regular function
                    let scope = *self.package(self.pkg).scope();
                    if name_str == "init" {
                        self.lobj_mut(okey).set_parent(Some(scope));
                        self.result.record_def(func_decl.name.clone(), Some(okey));
                        if func_decl.body.is_none() {
                            self.error(func_decl.span, "missing function body".to_string());
                        }
                    } else {
                        self.declare(scope, okey);
                        self.result.record_def(func_decl.name.clone(), Some(okey));
                    }
                } else {
                    // Method
                    if name_str != "_" {
                        methods.push(okey);
                    }
                    self.result.record_def(func_decl.name.clone(), Some(okey));
                }

                let di = self.tc_objs.decls.insert(DeclInfo::new_func(
                    file_scope,
                    func_decl.span.start.0, // Use span as node id
                ));
                self.obj_map.insert(okey, di);
                let order = self.obj_map.len() as u32;
                self.lobj_mut(okey).set_order(order);
            }
            Decl::Interface(iface_decl) => {
                let name_str = self.resolve_ident(&iface_decl.name);
                let okey = self.tc_objs.new_type_name(
                    0, // pos
                    Some(self.pkg),
                    name_str.to_string(),
                    None,
                );

                let di = self.tc_objs.decls.insert(DeclInfo::new_type(
                    file_scope,
                    iface_decl.span.start.0,
                    false,
                ));

                self.declare_pkg_obj(&iface_decl.name, okey, di);
            }
        }
    }

    /// Declares a package-level object.
    fn declare_pkg_obj(
        &mut self,
        ident: &gox_common::symbol::Ident,
        okey: ObjKey,
        dkey: crate::objects::DeclInfoKey,
    ) {
        let name = self.resolve_ident(ident);

        // Check for special names
        if name == "init" {
            self.error(ident.span, "cannot declare init - must be func".to_string());
            return;
        }

        let pkg_name = self.package(self.pkg).name();
        if name == "main" && pkg_name.as_deref() == Some("main") {
            self.error(ident.span, "cannot declare main - must be func".to_string());
            return;
        }

        let scope = *self.package(self.pkg).scope();
        self.declare(scope, okey);
        self.result.record_def(ident.clone(), Some(okey));
        self.obj_map.insert(okey, dkey);
        let order = self.obj_map.len() as u32;
        self.lobj_mut(okey).set_order(order);
    }

    /// Associates a method with its receiver base type.
    fn associate_method(&mut self, _method_key: ObjKey, _fctx: &mut FilesContext<F>) {
        // TODO: Implement method association with receiver type
    }

    /// Imports a package.
    fn import_package(&mut self, _path: &str, _span: Span) -> PackageKey {
        // TODO: Implement actual import
        // For now, create a fake package
        let pkg = self.tc_objs.new_package(_path.to_string());
        pkg
    }

    /// Type-checks all package objects (but not function bodies).
    pub fn package_objects(&mut self, fctx: &mut FilesContext<F>) {
        // Process package objects in source order
        let mut obj_list: Vec<ObjKey> = self.obj_map.keys().copied().collect();
        obj_list.sort_by_key(|&o| self.lobj(o).order());

        // First pass: add method declarations to types
        for &o in &obj_list {
            let lobj = self.lobj(o);
            if lobj.entity_type().is_type_name() && lobj.typ().is_some() {
                self.add_method_decls(o, fctx);
            }
        }

        // Second pass: type-check non-alias declarations
        let alias_list: Vec<ObjKey> = obj_list
            .into_iter()
            .filter(|&o| {
                if self.lobj(o).entity_type().is_type_name() {
                    if let Some(di) = self.obj_map.get(&o) {
                        if let Some(t) = self.decl_info(*di).as_type() {
                            if t.alias {
                                return true; // Keep for phase 2
                            }
                        }
                    }
                }
                // Phase 1: type-check non-alias
                self.obj_decl(o);
                false
            })
            .collect();

        // Third pass: type-check alias declarations
        for o in alias_list {
            self.obj_decl(o);
        }

        // Clear methods map
        fctx.methods.clear();
    }

    /// Checks for unused imports.
    pub fn unused_imports(&mut self, fctx: &mut FilesContext<F>) {
        // Check regular imported packages
        let pkg_scope = *self.package(self.pkg).scope();
        for &child_scope in self.scope(pkg_scope).children() {
            for (_, &okey) in self.scope(child_scope).elems() {
                let obj = self.lobj(okey);
                if let crate::obj::EntityType::PkgName { imported, used } = obj.entity_type() {
                    if !used {
                        let pkg = self.package(*imported);
                        self.soft_error(
                            Span::default(),
                            format!("{} imported but not used", pkg.path()),
                        );
                    }
                }
            }
        }

        // Check dot-imported packages
        for (_, imports) in &fctx.unused_dot_imports {
            for (&pkey, &span) in imports {
                self.soft_error(
                    span,
                    format!("{} imported but not used", self.package(pkey).path()),
                );
            }
        }
    }
}
