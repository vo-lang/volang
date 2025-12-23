//! Declaration resolver - collects and organizes package-level declarations.
//!
//! This module handles the first pass of type checking: collecting all
//! package-level declarations and organizing them for later type checking.


use std::collections::HashSet;

use vo_common::span::Span;
use vo_syntax::ast::{Decl, Expr, FuncDecl, TypeExpr};

use crate::objects::{ObjKey, PackageKey, ScopeKey};

use super::checker::Checker;
use super::errors::TypeError;
use crate::importer::Importer;

/// DeclInfo for const declarations.
#[derive(Debug, Clone)]
pub struct DeclInfoConst {
    pub file_scope: ScopeKey,
    pub typ: Option<TypeExpr>,
    pub init: Option<Expr>,
    pub deps: HashSet<ObjKey>,
}

/// DeclInfo for var declarations.
#[derive(Debug, Clone)]
pub struct DeclInfoVar {
    pub file_scope: ScopeKey,
    pub lhs: Option<Vec<ObjKey>>,
    pub typ: Option<TypeExpr>,
    pub init: Option<Expr>,
    pub deps: HashSet<ObjKey>,
}

/// DeclInfo for type declarations.
#[derive(Debug, Clone)]
pub struct DeclInfoType {
    pub file_scope: ScopeKey,
    pub typ: TypeExpr,
    pub alias: bool,
}

/// DeclInfo for func declarations.
#[derive(Debug, Clone)]
pub struct DeclInfoFunc {
    pub file_scope: ScopeKey,
    pub fdecl: FuncDecl,
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
    pub(crate) fn new_const(
        file_scope: ScopeKey,
        typ: Option<TypeExpr>,
        init: Option<Expr>,
    ) -> DeclInfo {
        DeclInfo::Const(DeclInfoConst {
            file_scope,
            typ,
            init,
            deps: HashSet::new(),
        })
    }

    pub(crate) fn new_var(
        file_scope: ScopeKey,
        lhs: Option<Vec<ObjKey>>,
        typ: Option<TypeExpr>,
        init: Option<Expr>,
    ) -> DeclInfo {
        DeclInfo::Var(DeclInfoVar {
            file_scope,
            lhs,
            typ,
            init,
            deps: HashSet::new(),
        })
    }

    pub(crate) fn new_type(file_scope: ScopeKey, typ: TypeExpr, alias: bool) -> DeclInfo {
        DeclInfo::Type(DeclInfoType {
            file_scope,
            typ,
            alias,
        })
    }

    pub(crate) fn new_func(file_scope: ScopeKey, fdecl: FuncDecl) -> DeclInfo {
        DeclInfo::Func(DeclInfoFunc {
            file_scope,
            fdecl,
            deps: HashSet::new(),
        })
    }

    pub(crate) fn as_const(&self) -> &DeclInfoConst {
        match self {
            DeclInfo::Const(c) => c,
            _ => unreachable!(),
        }
    }

    pub(crate) fn as_var(&self) -> &DeclInfoVar {
        match self {
            DeclInfo::Var(v) => v,
            _ => unreachable!(),
        }
    }

    pub(crate) fn as_type(&self) -> &DeclInfoType {
        match self {
            DeclInfo::Type(t) => t,
            _ => unreachable!(),
        }
    }

    pub(crate) fn as_func(&self) -> &DeclInfoFunc {
        match self {
            DeclInfo::Func(f) => f,
            _ => unreachable!(),
        }
    }

    pub(crate) fn file_scope(&self) -> ScopeKey {
        match self {
            DeclInfo::Const(c) => c.file_scope,
            DeclInfo::Var(v) => v.file_scope,
            DeclInfo::Type(t) => t.file_scope,
            DeclInfo::Func(f) => f.file_scope,
        }
    }

    pub(crate) fn deps(&self) -> &HashSet<ObjKey> {
        match self {
            DeclInfo::Const(c) => &c.deps,
            DeclInfo::Var(v) => &v.deps,
            DeclInfo::Func(f) => &f.deps,
            _ => unreachable!(),
        }
    }

    pub(crate) fn deps_mut(&mut self) -> &mut HashSet<ObjKey> {
        match self {
            DeclInfo::Const(c) => &mut c.deps,
            DeclInfo::Var(v) => &mut v.deps,
            DeclInfo::Func(f) => &mut f.deps,
            _ => unreachable!(),
        }
    }

    /// Add a dependency to this declaration.
    pub(crate) fn add_dep(&mut self, okey: ObjKey) {
        self.deps_mut().insert(okey);
    }
}

// =============================================================================
// Checker resolver methods
// =============================================================================

impl Checker {
    /// Collects all package-level declarations from the files.
    /// This is the first pass of type checking.
    pub(crate) fn collect_objects(&mut self, files: &[vo_syntax::ast::File], importer: Option<&mut dyn Importer>) {
        // Track all imported packages
        let mut all_imported: HashSet<PackageKey> = self
            .package(self.pkg)
            .imports()
            .iter()
            .copied()
            .collect();

        // List of methods with non-blank names: (method_key, receiver_type_name, is_pointer)
        let mut methods: Vec<(ObjKey, String, bool)> = Vec::new();
        
        // We need to reborrow importer for each import, so collect import info first
        let mut import_infos: Vec<(usize, ScopeKey, vo_syntax::ast::ImportDecl)> = Vec::new();

        for (file_num, file) in files.iter().enumerate() {
            // Create file scope
            let parent_scope = Some(*self.package(self.pkg).scope());
            let file_scope = self.tc_objs.new_scope(
                parent_scope,
                0, // pos
                0, // end
                &format!("file{}", file_num),
                false,
            );
            self.result.record_scope(file.span, file_scope);

            // Collect imports for later processing
            for import in &file.imports {
                import_infos.push((file_num, file_scope, import.clone()));
            }

            // Process declarations
            for decl in &file.decls {
                self.collect_decl_objects(decl, file_scope, &mut methods);
            }
        }

        // Verify that objects in package and file scopes have different names
        let pkg_scope_key = *self.package(self.pkg).scope();
        let pkg_scope = self.scope(pkg_scope_key);
        let children: Vec<ScopeKey> = pkg_scope.children().to_vec();
        for s in children {
            let elems: Vec<(String, ObjKey)> = self
                .scope(s)
                .elems()
                .iter()
                .map(|(name, &okey)| (name.clone(), okey))
                .collect();
            for (_, okey) in elems {
                let obj_val = self.lobj(okey);
                let pkg_scope = self.scope(pkg_scope_key);
                if let Some(alt) = pkg_scope.lookup(obj_val.name()) {
                    let alt_val = self.lobj(alt);
                    if let crate::obj::EntityType::PkgName { imported, .. } = obj_val.entity_type() {
                        let pkg_val = self.package(*imported);
                        self.error_code_msg(
                            TypeError::Redeclared,
                            Span::default(),
                            format!(
                                "{} already declared through import of {}",
                                alt_val.name(),
                                pkg_val.path()
                            ),
                        );
                    } else {
                        if let Some(pkg_key) = obj_val.pkg() {
                            let pkg_val = self.package(pkg_key);
                            self.error_code_msg(
                                TypeError::Redeclared,
                                Span::default(),
                                format!(
                                    "{} already declared through dot-import of {}",
                                    alt_val.name(),
                                    pkg_val.path()
                                ),
                            );
                        }
                    }
                    self.report_alt_decl(okey);
                }
            }
        }

        // Process imports with importer
        let mut importer = importer;
        for (_file_num, file_scope, import) in import_infos {
            self.collect_import(&import, file_scope, &mut all_imported, &mut importer);
        }

        // Associate methods with receiver base types
        for (method_key, recv_type_name, is_pointer) in methods {
            self.associate_method_with_receiver(method_key, &recv_type_name, is_pointer);
        }
    }

    /// Collects import declaration.
    fn collect_import(
        &mut self,
        import: &vo_syntax::ast::ImportDecl,
        file_scope: ScopeKey,
        all_imported: &mut HashSet<PackageKey>,
        importer: &mut Option<&mut dyn Importer>,
    ) {
        let path = &import.path.value;
        
        // Import the package using importer if available
        let imp = if let Some(importer) = importer.as_mut() {
            use crate::importer::{ImportKey as ImpKey, ImportResult};
            let dir = importer.working_dir().to_string_lossy().to_string();
            let key = ImpKey::new(path, &dir);
            match importer.import(&key) {
                ImportResult::Ok(pkg) => pkg,
                ImportResult::Err(e) => {
                    self.error_code_msg(TypeError::InvalidImportPath, import.span, e);
                    return;
                }
                ImportResult::Cycle => {
                    self.error_code_msg(TypeError::ImportCycle, import.span, format!("import cycle not allowed: {}", path));
                    return;
                }
            }
        } else {
            // Fallback: create empty package (for testing without importer)
            self.import_package(path, import.span)
        };

        // Add to list of explicit imports
        if !all_imported.contains(&imp) {
            all_imported.insert(imp);
            self.package_mut(self.pkg).add_import(imp);
        }

        // Determine the local name
        let name = if let Some(alias) = &import.alias {
            let alias_name = self.resolve_ident(alias);
            if alias_name == "init" {
                self.error_code(TypeError::CannotDeclareInit, import.span);
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
            self.result.record_implicit(import.span, pkg_name_obj);
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
            self.add_unused_dot_import(file_scope, imp, import.span);
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
        methods: &mut Vec<(ObjKey, String, bool)>,
    ) {
        match decl {
            Decl::Const(const_decl) => {
                let mut last_typ_expr: Option<TypeExpr> = None;
                let mut last_values: &[Expr] = &[];

                for (iota, spec) in const_decl.specs.iter().enumerate() {
                    // Update last type and values if this spec has them
                    if spec.ty.is_some() || !spec.values.is_empty() {
                        last_typ_expr = spec.ty.clone();
                        last_values = &spec.values;
                    }

                    for (i, name) in spec.names.iter().enumerate() {
                        let name_str = self.resolve_ident(name);
                        let okey = self.tc_objs.new_const(
                            0, // pos
                            Some(self.pkg),
                            name_str.to_string(),
                            None,
                            crate::constant::make_int64(iota as i64),
                        );

                        let init_expr = last_values.get(i).cloned();
                        let di = self.tc_objs.decls.insert(DeclInfo::new_const(
                            file_scope,
                            last_typ_expr.clone(),
                            init_expr,
                        ));

                        self.declare_pkg_obj(name, okey, di);
                    }

                    // Check arity: names vs values count
                    self.arity_match(
                        spec.names.len(),
                        last_values.len(),
                        last_typ_expr.is_some(),
                        true, // is_const
                        spec.span,
                    );
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
                    let typ_expr = spec.ty.clone();

                    if n_to_1 {
                        // n:1 assignment
                        let di = self.tc_objs.decls.insert(DeclInfo::new_var(
                            file_scope,
                            Some(lhs.clone()),
                            typ_expr.clone(),
                            Some(spec.values[0].clone()),
                        ));
                        for (name, &okey) in spec.names.iter().zip(&lhs) {
                            self.declare_pkg_obj(name, okey, di);
                        }
                    } else {
                        // 1:1 or n:n assignment
                        for (i, (name, &okey)) in spec.names.iter().zip(&lhs).enumerate() {
                            let init_expr = spec.values.get(i).cloned();
                            let di = self.tc_objs.decls.insert(DeclInfo::new_var(
                                file_scope,
                                None,
                                typ_expr.clone(),
                                init_expr,
                            ));
                            self.declare_pkg_obj(name, okey, di);
                        }
                    }

                    // Check arity: names vs values count
                    self.arity_match(
                        spec.names.len(),
                        spec.values.len(),
                        typ_expr.is_some(),
                        false, // is_const
                        spec.span,
                    );
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
                    type_decl.ty.clone(),
                    false, // Vo doesn't have type aliases with =
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
                            self.error_code(TypeError::MissingFuncBody, func_decl.span);
                        }
                    } else {
                        self.declare(scope, okey);
                        self.result.record_def(func_decl.name.clone(), Some(okey));
                    }
                } else {
                    // Method - get receiver info from AST
                    let recv = func_decl.receiver.as_ref().unwrap();
                    let recv_type_name = self.resolve_ident(&recv.ty).to_string();
                    let is_pointer = recv.is_pointer;
                    
                    if name_str != "_" {
                        methods.push((okey, recv_type_name, is_pointer));
                    }
                    self.result.record_def(func_decl.name.clone(), Some(okey));
                }

                let di = self.tc_objs.decls.insert(DeclInfo::new_func(
                    file_scope,
                    func_decl.clone(),
                ));
                self.obj_map.insert(okey, di);
                let order = self.obj_map.len() as u32;
                self.lobj_mut(okey).set_order(order);
            }
        }
    }

    /// Declares a package-level object.
    fn declare_pkg_obj(
        &mut self,
        ident: &vo_common::symbol::Ident,
        okey: ObjKey,
        dkey: crate::objects::DeclInfoKey,
    ) {
        let name = self.resolve_ident(ident);

        // Check for special names
        if name == "init" {
            self.error_code(TypeError::CannotDeclareInit, ident.span);
            return;
        }

        let pkg_name = self.package(self.pkg).name();
        if name == "main" && pkg_name.as_deref() == Some("main") {
            self.error_code(TypeError::CannotDeclareMain, ident.span);
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
    /// In Vo, Receiver has `ty: Ident` and `is_pointer: bool` directly.
    fn associate_method_with_receiver(
        &mut self,
        method_key: ObjKey,
        receiver_type_name: &str,
        is_pointer: bool,
    ) {
        // Use resolve_base_type_name to properly resolve the base type
        if let Some((ptr, base_obj)) = self.resolve_base_type_name(receiver_type_name, is_pointer) {
            // Set pointer receiver flag on method
            self.lobj_mut(method_key)
                .entity_type_mut()
                .func_set_has_ptr_recv(ptr);

            // Associate method with base type
            self.methods.entry(base_obj).or_default().push(method_key);
        }
    }

    /// Resolves the base type name for a method receiver.
    /// Returns (has_pointer, base_type_obj) or None if not found.
    fn resolve_base_type_name(&self, type_name: &str, is_pointer: bool) -> Option<(bool, ObjKey)> {
        let pkg_scope = *self.package(self.pkg).scope();
        let scope = self.scope(pkg_scope);

        // Look up in package scope
        let okey = scope.lookup(type_name)?;
        let lobj = self.lobj(okey);

        // Must be a type name
        if !lobj.entity_type().is_type_name() {
            return None;
        }

        // Check for cycles and resolve aliases
        let mut path: Vec<ObjKey> = Vec::new();
        let current = okey;

        loop {
            // Check for cycle
            if path.contains(&current) {
                return None;
            }
            path.push(current);

            // Check if it's an alias
            if let Some(&decl_key) = self.obj_map.get(&current) {
                let decl = self.decl_info(decl_key);
                if let DeclInfo::Type(t) = decl {
                    if !t.alias {
                        // Found non-alias type
                        return Some((is_pointer, current));
                    }
                    // For alias, would need to resolve the underlying type
                    // This requires AST access to get the alias target type name
                    // For now, just return the alias itself
                    return Some((is_pointer, current));
                }
            }

            // Not in obj_map means it's predeclared or imported
            // Just return it
            return Some((is_pointer, current));
        }
    }

    /// Validates an import path.
    fn valid_import_path<'a>(&self, path: &'a str) -> Result<&'a str, String> {
        if path.is_empty() {
            return Err("empty string".to_owned());
        }
        let illegal_chars: &[char] = &[
            '!', '"', '#', '$', '%', '&', '\'', '(', ')', '*', ',', ':', ';', '<', '=', '>', '?',
            '[', '\\', ']', '^', '{', '|', '}', '`', '\u{FFFD}',
        ];
        if let Some(c) = path
            .chars()
            .find(|&x| !x.is_ascii_graphic() || x.is_whitespace() || illegal_chars.contains(&x))
        {
            return Err(format!("invalid character: {}", c));
        }
        Ok(path)
    }

    /// Imports a package (fallback when no importer is available).
    /// First tries to find an already loaded package, then creates empty package.
    fn import_package(&mut self, path: &str, span: Span) -> PackageKey {
        // Validate import path
        if let Err(e) = self.valid_import_path(path) {
            self.error_code_msg(TypeError::InvalidImportPath, span, format!("invalid import path ({})", e));
        }

        let dir = ".".to_string();
        let key = super::checker::ImportKey::new(path, &dir);

        // Check if already imported in this checker
        if let Some(&imp) = self.imp_map.get(&key) {
            return imp;
        }

        // Check if package was pre-loaded (search by path in tc_objs)
        if let Some(pkg) = self.tc_objs.find_package_by_path(path) {
            self.imp_map.insert(key, pkg);
            return pkg;
        }

        // Create a new empty package as last resort
        let pkg = self.new_package(path.to_string());

        // Extract package name from path
        let name = if let Some(i) = path.rfind('/') {
            &path[i + 1..]
        } else {
            path
        };
        self.package_mut(pkg).set_name(name.to_string());

        self.imp_map.insert(key, pkg);
        pkg
    }

    /// Checks arity (number of names vs values) for const/var declarations.
    fn arity_match(
        &self,
        names_count: usize,
        values_count: usize,
        has_type: bool,
        is_const: bool,
        span: Span,
    ) {
        let l = names_count;
        let r = values_count;

        if l < r {
            self.error_code(TypeError::ExtraInitExpr, span);
        } else if l > r {
            if is_const {
                if r == 0 && !has_type {
                    self.error_code(TypeError::MissingTypeOrInit, span);
                }
            } else {
                // var declaration
                if r != 1 {
                    self.error_code(TypeError::MissingInitExpr, span);
                }
            }
        }
    }

    /// Type-checks all package objects (but not function bodies).
    pub(crate) fn package_objects(&mut self) {
        // Process package objects in source order
        let mut obj_list: Vec<ObjKey> = self.obj_map.keys().copied().collect();
        obj_list.sort_by_key(|&o| self.lobj(o).order());

        // First pass: add method declarations to types
        for &o in &obj_list {
            let lobj = self.lobj(o);
            if lobj.entity_type().is_type_name() && lobj.typ().is_some() {
                self.add_method_decls(o);
            }
        }

        // Second pass: type-check non-alias declarations
        let alias_list: Vec<ObjKey> = obj_list
            .into_iter()
            .filter(|&o| {
                if self.lobj(o).entity_type().is_type_name() {
                    if let Some(di) = self.obj_map.get(&o) {
                        if let DeclInfo::Type(t) = self.decl_info(*di) {
                            if t.alias {
                                return true; // Keep for phase 2
                            }
                        }
                    }
                }
                // Phase 1: type-check non-alias
                self.obj_decl(o, None);
                false
            })
            .collect();

        // Third pass: type-check alias declarations
        for o in alias_list {
            self.obj_decl(o, None);
        }

        // Clear methods map
        self.methods.clear();
    }

    /// Checks for unused imports.
    pub(crate) fn unused_imports(&mut self) {
        // Check regular imported packages
        let pkg_scope = *self.package(self.pkg).scope();
        for &child_scope in self.scope(pkg_scope).children() {
            for (_, &okey) in self.scope(child_scope).elems() {
                let obj = self.lobj(okey);
                if let crate::obj::EntityType::PkgName { imported, used } = obj.entity_type() {
                    if !used {
                        let pkg = self.package(*imported);
                        self.emit(TypeError::UnusedImport.at_with_message(
                            Span::default(),
                            format!("{} imported but not used", pkg.path()),
                        ));
                    }
                }
            }
        }

        // Check dot-imported packages
        for (_, imports) in &self.unused_dot_imports {
            for (&pkey, &span) in imports {
                self.emit(TypeError::UnusedImport.at_with_message(
                    span,
                    format!("{} imported but not used", self.package(pkey).path()),
                ));
            }
        }
    }
}
