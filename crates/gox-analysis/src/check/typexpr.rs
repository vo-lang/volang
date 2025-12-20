//! Type expression resolution.
//!
//! This module converts AST type expressions (TypeExpr) to internal types (TypeKey).
//! It handles type-checking of type expressions and resolves them to TypeKey values.

#![allow(dead_code)]

use gox_common::symbol::{Ident, Symbol};

use crate::obj::EntityType;
use crate::objects::{ObjKey, ScopeKey, TypeKey};
use crate::operand::{Operand, OperandMode};
use crate::lookup;
use crate::scope;
use crate::typ::{self, ChanDir, Type};
use gox_syntax::ast::{self, Expr, FuncSig, Param, TypeExpr, TypeExprKind, InterfaceElem};
use gox_common_core::ExprId;

use super::checker::{Checker, FilesContext};

impl Checker {
    // =========================================================================
    // Main type expression entry points
    // =========================================================================

    /// Type-checks the type expression and returns its type, or Invalid Type.
    pub fn type_expr(&mut self, ty: &TypeExpr, fctx: &mut FilesContext) -> TypeKey {
        self.defined_type(ty, None, fctx)
    }

    /// Like type_expr but also accepts a type name def.
    /// If def is Some, ty is the type specification for the defined type def.
    pub fn defined_type(&mut self, ty: &TypeExpr, def: Option<TypeKey>, fctx: &mut FilesContext) -> TypeKey {
        let t = self.type_internal(ty, def, fctx);
        debug_assert!(typ::is_typed(t, &self.tc_objs));
        self.result.record_type_and_value(ExprId(ty.id.0), OperandMode::TypeExpr, t);
        t
    }

    /// Like type_expr but breaks infinite size of recursive types.
    /// Used for pointer base types, slice/map element types, function params, etc.
    pub fn indirect_type(&mut self, ty: &TypeExpr, fctx: &mut FilesContext) -> TypeKey {
        fctx.push(self.tc_objs.universe().indir());
        let t = self.defined_type(ty, None, fctx);
        fctx.pop();
        t
    }

    // =========================================================================
    // Internal type checking driver
    // =========================================================================

    /// Drives type checking of types. Must only be called by defined_type.
    fn type_internal(&mut self, ty: &TypeExpr, def: Option<TypeKey>, fctx: &mut FilesContext) -> TypeKey {
        let set_underlying = |typ: Option<TypeKey>, tc_objs: &mut crate::objects::TCObjects| {
            if let Some(d) = def {
                if let Some(named) = tc_objs.types[d].try_as_named_mut() {
                    if let Some(t) = typ {
                        named.set_underlying(t);
                    }
                }
            }
        };

        let result_t: Option<TypeKey> = match &ty.kind {
            TypeExprKind::Ident(ident) => {
                let mut x = Operand::new();
                self.ident(&mut x, ident, def, true, fctx);
                match x.mode {
                    OperandMode::TypeExpr => {
                        set_underlying(x.typ, &mut self.tc_objs);
                        x.typ
                    }
                    OperandMode::Invalid => None,
                    _ => {
                        self.error(ident.span, format!("{} is not a type", self.resolve_ident(ident)));
                        None
                    }
                }
            }
            TypeExprKind::Selector(sel) => {
                // Handle qualified type: pkg.Type
                let pkg_name = self.resolve_ident(&sel.pkg).to_string();
                let type_name = self.resolve_ident(&sel.sel).to_string();
                let sel_sel = sel.sel.clone();
                
                if let Some(scope_key) = self.octx.scope {
                    if let Some((_, pkg_obj)) = scope::lookup_parent(scope_key, &pkg_name, &self.tc_objs) {
                        let entity = self.tc_objs.lobjs[pkg_obj].entity_type().clone();
                        if let EntityType::PkgName { imported, .. } = entity {
                            let pkg_scope = *self.package(imported).scope();
                            if let Some(type_obj) = self.scope(pkg_scope).lookup(&type_name) {
                                self.result.record_use(sel_sel, type_obj);
                                let is_type = self.tc_objs.lobjs[type_obj].entity_type().is_type_name();
                                let typ = self.tc_objs.lobjs[type_obj].typ();
                                if is_type {
                                    if let Some(t) = typ {
                                        set_underlying(Some(t), &mut self.tc_objs);
                                        return t;
                                    }
                                }
                            }
                            self.error(ty.span, format!("{}.{} is not a type", pkg_name, type_name));
                        } else {
                            self.error(sel.pkg.span, format!("{} is not a package", pkg_name));
                        }
                    } else {
                        self.error(sel.pkg.span, format!("undeclared name: {}", pkg_name));
                    }
                }
                None
            }
            TypeExprKind::Array(arr) => {
                let len = self.array_len(&arr.len, fctx);
                let elem = self.type_expr(&arr.elem, fctx);
                let t = self.tc_objs.new_t_array(elem, len);
                set_underlying(Some(t), &mut self.tc_objs);
                Some(t)
            }
            TypeExprKind::Slice(elem) => {
                let elem_type = self.indirect_type(elem, fctx);
                let t = self.tc_objs.new_t_slice(elem_type);
                set_underlying(Some(t), &mut self.tc_objs);
                Some(t)
            }
            TypeExprKind::Map(map) => {
                let key = self.indirect_type(&map.key, fctx);
                let value = self.indirect_type(&map.value, fctx);
                let t = self.tc_objs.new_t_map(key, value);
                set_underlying(Some(t), &mut self.tc_objs);
                Some(t)
            }
            TypeExprKind::Chan(chan) => {
                let dir = match chan.dir {
                    ast::ChanDir::Both => ChanDir::SendRecv,
                    ast::ChanDir::Send => ChanDir::SendOnly,
                    ast::ChanDir::Recv => ChanDir::RecvOnly,
                };
                let elem = self.indirect_type(&chan.elem, fctx);
                let t = self.tc_objs.new_t_chan(dir, elem);
                set_underlying(Some(t), &mut self.tc_objs);
                Some(t)
            }
            TypeExprKind::Func(func) => {
                let t = self.func_type_ast(func, fctx);
                set_underlying(Some(t), &mut self.tc_objs);
                Some(t)
            }
            TypeExprKind::Struct(s) => {
                let t = self.struct_type(s, fctx);
                set_underlying(Some(t), &mut self.tc_objs);
                Some(t)
            }
            TypeExprKind::Pointer(base) => {
                let base_type = self.indirect_type(base, fctx);
                let t = self.tc_objs.new_t_pointer(base_type);
                set_underlying(Some(t), &mut self.tc_objs);
                Some(t)
            }
            TypeExprKind::Interface(iface) => {
                let t = self.interface_type(iface, def, fctx);
                set_underlying(Some(t), &mut self.tc_objs);
                Some(t)
            }
        };

        if let Some(t) = result_t {
            t
        } else {
            let invalid_type = self.invalid_type();
            set_underlying(Some(invalid_type), &mut self.tc_objs);
            invalid_type
        }
    }

    // =========================================================================
    // Identifier type checking
    // =========================================================================

    /// Type-checks identifier and initializes x with the value or type of ident.
    /// If want_type is set, the identifier is expected to denote a type.
    pub fn ident(
        &mut self,
        x: &mut Operand,
        ident: &Ident,
        _def: Option<TypeKey>,
        want_type: bool,
        fctx: &mut FilesContext,
    ) {
        x.mode = OperandMode::Invalid;
        x.expr_id = None;

        let name = self.resolve_ident(ident);

        // Look up in scope
        if let Some(scope_key) = self.octx.scope {
            if let Some((_skey, okey)) = scope::lookup_parent(scope_key, name, &self.tc_objs) {
                self.result.record_use(ident.clone(), okey);

                // Type-check the object if needed
                let obj = &self.tc_objs.lobjs[okey];
                let mut otype = obj.typ();
                if otype.is_none() || (obj.entity_type().is_type_name() && want_type) {
                    self.obj_decl(okey, None, fctx);
                    otype = self.tc_objs.lobjs[okey].typ();
                }

                let invalid_type = self.invalid_type();
                
                // Extract entity info first to avoid borrow conflicts
                let entity_type = self.tc_objs.lobjs[okey].entity_type().clone();
                let obj_name = self.tc_objs.lobjs[okey].name().to_string();

                match &entity_type {
                    EntityType::PkgName { .. } => {
                        self.error(ident.span, format!("use of package {} not in selector", obj_name));
                        return;
                    }
                    EntityType::Const { val } => {
                        self.add_decl_dep(okey);
                        if otype == Some(invalid_type) {
                            return;
                        }
                        // Check for iota
                        if self.is_iota(okey) {
                            if let Some(iota_val) = &self.octx.iota {
                                x.mode = OperandMode::Constant(iota_val.clone());
                            } else {
                                self.error(ident.span, "cannot use iota outside constant declaration".to_string());
                                return;
                            }
                        } else {
                            x.mode = OperandMode::Constant(val.clone());
                        }
                    }
                    EntityType::TypeName => {
                        x.mode = OperandMode::TypeExpr;
                    }
                    EntityType::Var(_) => {
                        // It's ok to mark non-local variables, but ignore variables
                        // from other packages to avoid potential race conditions with
                        // dot-imported variables.
                        let obj_pkg = self.tc_objs.lobjs[okey].pkg();
                        if obj_pkg == Some(self.pkg) {
                            self.tc_objs.lobjs[okey].set_var_used(true);
                        }
                        self.add_decl_dep(okey);
                        if otype == Some(invalid_type) {
                            return;
                        }
                        x.mode = OperandMode::Variable;
                    }
                    EntityType::Func { .. } => {
                        self.add_decl_dep(okey);
                        x.mode = OperandMode::Value;
                    }
                    EntityType::Builtin(id) => {
                        x.mode = OperandMode::Builtin(*id);
                    }
                    EntityType::Nil => {
                        x.mode = OperandMode::Value;
                    }
                    EntityType::Label { .. } => {
                        // Labels shouldn't appear as values
                        return;
                    }
                }
                x.typ = otype;
                return;
            }
        }

        // Name not found
        if name == "_" {
            self.error(ident.span, "cannot use _ as value or type".to_string());
        } else {
            self.error(ident.span, format!("undeclared name: {}", name));
        }
    }

    /// Checks if an object is the predeclared iota.
    fn is_iota(&self, okey: ObjKey) -> bool {
        self.universe().iota() == okey
    }

    // =========================================================================
    // Array length evaluation
    // =========================================================================

    /// Evaluates an array length expression and returns the length.
    fn array_len(&mut self, e: &Expr, fctx: &mut FilesContext) -> Option<u64> {
        let mut x = Operand::new();
        self.expr(&mut x, e, fctx);
        if let OperandMode::Constant(v) = &x.mode {
            if let Some(t) = x.typ {
                if typ::is_untyped(t, &self.tc_objs) || typ::is_integer(t, &self.tc_objs) {
                    if let Some(n) = v.int_val().and_then(|i| u64::try_from(i).ok()) {
                        return Some(n);
                    }
                }
            }
            self.error(e.span, "array length must be a non-negative integer".to_string());
        } else if x.mode != OperandMode::Invalid {
            self.error(e.span, "array length must be constant".to_string());
        }
        None
    }

    // =========================================================================
    // Function type checking
    // =========================================================================

    /// Type-checks a function type from AST.
    fn func_type_ast(&mut self, func: &ast::FuncType, fctx: &mut FilesContext) -> TypeKey {
        // Create a new scope for the function
        let scope_key = self.tc_objs.new_scope(
            self.octx.scope,
            0, // pos
            0, // end
            "function",
            true,
        );

        // Collect parameters - FuncType has Vec<TypeExpr>, not Vec<Param>
        let mut params = Vec::new();
        for param_type in &func.params {
            let ty = self.indirect_type(param_type, fctx);
            let var = self.tc_objs.new_param_var(0, Some(self.pkg), String::new(), Some(ty));
            params.push(var);
        }
        let variadic = false; // FuncType in GoX doesn't have variadic marker

        // Collect results
        let mut results = Vec::new();
        for result_type in &func.results {
            let ty = self.indirect_type(result_type, fctx);
            let var = self.tc_objs.new_param_var(0, Some(self.pkg), String::new(), Some(ty));
            results.push(var);
        }

        let params_tuple = self.new_tuple(params);
        let results_tuple = self.new_tuple(results);

        self.tc_objs.new_t_signature(Some(scope_key), None, params_tuple, results_tuple, variadic)
    }

    /// Type-checks a function signature and returns its type.
    pub fn func_type_from_sig(&mut self, sig: &FuncSig, fctx: &mut FilesContext) -> TypeKey {
        // Create a new scope for the function
        let scope_key = self.tc_objs.new_scope(
            self.octx.scope,
            0,
            0,
            "function",
            true,
        );

        // Collect parameter types
        let mut param_vars = Vec::new();
        let variadic = sig.variadic;

        for (i, param) in sig.params.iter().enumerate() {
            let param_type = self.indirect_type(&param.ty, fctx);
            
            // For variadic, wrap the last param type in slice
            let final_type = if variadic && i == sig.params.len() - 1 {
                self.tc_objs.new_t_slice(param_type)
            } else {
                param_type
            };

            // Create var for each name, or anonymous if no names
            if param.names.is_empty() {
                let var = self.tc_objs.new_param_var(0, Some(self.pkg), String::new(), Some(final_type));
                param_vars.push(var);
            } else {
                for name in &param.names {
                    let name_str = self.resolve_ident(name).to_string();
                    let var = self.tc_objs.new_param_var(0, Some(self.pkg), name_str.clone(), Some(final_type));
                    self.declare(scope_key, var);
                    param_vars.push(var);
                }
            }
        }

        // Collect result types
        let mut result_vars = Vec::new();
        for result in &sig.results {
            let result_type = self.indirect_type(&result.ty, fctx);
            if let Some(name) = &result.name {
                let name_str = self.resolve_ident(name).to_string();
                let var = self.tc_objs.new_param_var(0, Some(self.pkg), name_str.clone(), Some(result_type));
                self.declare(scope_key, var);
                result_vars.push(var);
            } else {
                let var = self.tc_objs.new_param_var(0, Some(self.pkg), String::new(), Some(result_type));
                result_vars.push(var);
            }
        }

        let params_tuple = self.new_tuple(param_vars);
        let results_tuple = self.new_tuple(result_vars);

        self.tc_objs.new_t_signature(Some(scope_key), None, params_tuple, results_tuple, variadic)
    }

    /// Collects function parameters from FuncSig AST.
    fn collect_params_from_sig(
        &mut self,
        scope_key: ScopeKey,
        params: &[Param],
        variadic: bool,
        fctx: &mut FilesContext,
    ) -> Vec<ObjKey> {
        let mut vars = Vec::new();

        for (i, param) in params.iter().enumerate() {
            let is_last = i == params.len() - 1;
            let param_type = self.indirect_type(&param.ty, fctx);
            
            // Wrap last param in slice if variadic
            let final_type = if variadic && is_last {
                self.tc_objs.new_t_slice(param_type)
            } else {
                param_type
            };

            if param.names.is_empty() {
                let var = self.tc_objs.new_param_var(0, Some(self.pkg), String::new(), Some(final_type));
                vars.push(var);
            } else {
                for name in &param.names {
                    let name_str = self.resolve_ident(name).to_string();
                    let var = self.tc_objs.new_param_var(0, Some(self.pkg), name_str.clone(), Some(final_type));
                    self.declare(scope_key, var);
                    vars.push(var);
                }
            }
        }

        vars
    }

    /// Creates a new tuple type.
    pub fn new_tuple(&mut self, vars: Vec<ObjKey>) -> TypeKey {
        self.tc_objs.new_t_tuple(vars)
    }

    /// Type-checks the type expression (or nil value) and returns the type, or None for nil.
    /// If e is neither a type nor nil, returns Invalid type.
    pub fn type_or_nil(&mut self, e: &Expr, fctx: &mut FilesContext) -> Option<TypeKey> {
        let mut x = Operand::new();
        self.raw_expr(&mut x, e, None, fctx);
        let invalid_type = self.invalid_type();
        match x.mode {
            OperandMode::Invalid => Some(invalid_type),
            OperandMode::NoValue => {
                self.error(e.span, "used as type".to_string());
                Some(invalid_type)
            }
            OperandMode::TypeExpr => x.typ,
            _ => {
                if x.mode == OperandMode::Value && x.is_nil(&self.tc_objs) {
                    None
                } else {
                    self.error(e.span, "is not a type".to_string());
                    Some(invalid_type)
                }
            }
        }
    }

    // =========================================================================
    // Struct type checking
    // =========================================================================

    /// Type-checks a struct type.
    fn struct_type(&mut self, s: &ast::StructType, fctx: &mut FilesContext) -> TypeKey {
        if s.fields.is_empty() {
            return self.tc_objs.new_t_struct(Vec::new(), None);
        }

        let mut fields: Vec<ObjKey> = Vec::new();
        let mut tags: Option<Vec<Option<String>>> = None;
        let mut field_set: std::collections::HashMap<String, ObjKey> = std::collections::HashMap::new();

        for field in &s.fields {
            let field_type = self.type_expr(&field.ty, fctx);
            let tag = field.tag.as_ref().map(|t| t.value.clone());

            if field.names.is_empty() {
                // Embedded field
                // spec: "An embedded type must be specified as a type name T or as a pointer
                // to a non-interface type name *T, and T itself may not be a pointer type."
                let invalid_type = self.invalid_type();
                
                // For embedded fields, extract the type name
                let embedded_name = self.get_embedded_field_name(&field.ty);
                
                // Check embedded field constraints
                let (underlying, is_ptr) = lookup::try_deref(field_type, &self.tc_objs);
                let underlying_type = typ::underlying_type(underlying, &self.tc_objs);
                
                let is_valid = match &self.tc_objs.types[underlying_type] {
                    Type::Basic(_) if underlying_type == invalid_type => false,
                    Type::Pointer(_) => {
                        self.error(field.ty.span, "embedded field type cannot be a pointer".to_string());
                        false
                    }
                    Type::Interface(_) if is_ptr => {
                        self.error(field.ty.span, "embedded field type cannot be a pointer to an interface".to_string());
                        false
                    }
                    _ => true,
                };

                let final_type = if is_valid { field_type } else { invalid_type };
                let fld = self.tc_objs.new_field(
                    0,
                    Some(self.pkg),
                    embedded_name.clone(),
                    Some(final_type),
                    true,
                );
                
                // Check for duplicate field
                if embedded_name != "_" {
                    if let Some(&prev) = field_set.get(&embedded_name) {
                        self.error(field.ty.span, format!("{} redeclared", embedded_name));
                        self.report_alt_decl(prev);
                    } else {
                        field_set.insert(embedded_name, fld);
                    }
                }
                
                self.add_field_with_tag(&mut fields, &mut tags, tag, fld);
            } else {
                // Named fields
                for name in &field.names {
                    let name_str = self.resolve_ident(name).to_string();
                    let fld = self.tc_objs.new_field(
                        0,
                        Some(self.pkg),
                        name_str.clone(),
                        Some(field_type),
                        false,
                    );
                    
                    // Check for duplicate field
                    if name_str != "_" {
                        if let Some(&prev) = field_set.get(&name_str) {
                            self.error(name.span, format!("{} redeclared", name_str));
                            self.report_alt_decl(prev);
                        } else {
                            field_set.insert(name_str, fld);
                        }
                    }
                    
                    self.result.record_def(name.clone(), Some(fld));
                    self.add_field_with_tag(&mut fields, &mut tags, tag.clone(), fld);
                }
            }
        }

        self.tc_objs.new_t_struct(fields, tags)
    }

    /// Adds a field to the fields list and updates tags.
    fn add_field_with_tag(
        &self,
        fields: &mut Vec<ObjKey>,
        tags: &mut Option<Vec<Option<String>>>,
        tag: Option<String>,
        fld: ObjKey,
    ) {
        if tag.is_some() && tags.is_none() {
            *tags = Some(vec![None; fields.len()]);
        }
        if let Some(ref mut tag_vec) = tags {
            tag_vec.push(tag);
        }
        fields.push(fld);
    }

    /// Gets the name for an embedded field from its type expression.
    fn get_embedded_field_name(&self, ty: &TypeExpr) -> String {
        match &ty.kind {
            TypeExprKind::Ident(ident) => self.resolve_ident(ident).to_string(),
            TypeExprKind::Pointer(base) => self.get_embedded_field_name(base),
            TypeExprKind::Selector(sel) => self.resolve_ident(&sel.sel).to_string(),
            _ => String::new(),
        }
    }

    // =========================================================================
    // Interface type checking
    // =========================================================================

    /// Type-checks an interface type.
    fn interface_type(&mut self, iface: &ast::InterfaceType, def: Option<TypeKey>, fctx: &mut FilesContext) -> TypeKey {
        if iface.elems.is_empty() {
            return self.tc_objs.new_t_empty_interface();
        }

        // Create the interface type first (methods will be added later)
        let itype = self.tc_objs.new_t_interface(vec![], vec![]);

        // Collect methods and embedded interfaces
        let mut methods: Vec<ObjKey> = Vec::new();
        let mut embeds: Vec<TypeKey> = Vec::new();
        let mut method_set: std::collections::HashMap<String, ObjKey> = std::collections::HashMap::new();

        // Use named receiver type if available (for better error messages)
        let recv_type = def.unwrap_or(itype);

        for elem in &iface.elems {
            match elem {
                InterfaceElem::Method(method) => {
                    let name_str = self.resolve_ident(&method.name).to_string();
                    
                    // spec: "each method must have a unique non-blank name"
                    if name_str == "_" {
                        self.error(method.name.span, "invalid method name _".to_string());
                        continue;
                    }

                    // Check for duplicate method
                    if let Some(&prev) = method_set.get(&name_str) {
                        self.error(method.name.span, format!("{} redeclared", name_str));
                        self.report_alt_decl(prev);
                        continue;
                    }

                    // Create receiver for the method signature
                    let recv_var = self.tc_objs.new_var(0, Some(self.pkg), String::new(), Some(recv_type));
                    
                    // Type-check the method signature
                    let sig_type = self.func_type_from_sig(&method.sig, fctx);
                    
                    // Update signature with receiver
                    if let Type::Signature(sig) = &mut self.tc_objs.types[sig_type] {
                        sig.set_recv(Some(recv_var));
                    }

                    let method_obj = self.tc_objs.new_func(0, Some(self.pkg), name_str.clone(), Some(sig_type));
                    self.result.record_def(method.name.clone(), Some(method_obj));
                    
                    method_set.insert(name_str, method_obj);
                    methods.push(method_obj);
                }
                InterfaceElem::Embedded(ident) => {
                    // Look up the embedded interface type
                    let mut x = Operand::new();
                    self.ident(&mut x, ident, None, true, fctx);
                    
                    if let Some(typ) = x.typ {
                        let invalid_type = self.invalid_type();
                        if typ == invalid_type {
                            continue; // error reported before
                        }
                        
                        // Check that it's actually an interface
                        let underlying = typ::underlying_type(typ, &self.tc_objs);
                        match &self.tc_objs.types[underlying] {
                            Type::Interface(_) => {
                                embeds.push(typ);
                            }
                            _ => {
                                self.error(ident.span, format!("{} is not an interface", self.resolve_ident(ident)));
                            }
                        }
                    }
                }
            }
        }

        // Sort methods by name for consistent ordering
        methods.sort_by(|a, b| {
            let name_a = self.tc_objs.lobjs[*a].name();
            let name_b = self.tc_objs.lobjs[*b].name();
            name_a.cmp(name_b)
        });

        // Update the interface type with collected methods and embeds
        if let Type::Interface(iface_detail) = &mut self.tc_objs.types[itype] {
            *iface_detail.methods_mut() = methods.clone();
            *iface_detail.embeddeds_mut() = embeds;
            
            // Set all_methods (methods from this interface + embedded)
            // For now, just use explicit methods; embedded methods require more work
            iface_detail.set_complete(methods);
        }

        itype
    }

    // =========================================================================
    // Helper methods
    // =========================================================================

    /// Looks up a predeclared type by name.
    fn lookup_predeclared_type(&self, name: Symbol) -> Option<TypeKey> {
        let name_str = self.interner.resolve(name)?;
        self.universe().lookup_type_by_name(name_str)
    }
}
