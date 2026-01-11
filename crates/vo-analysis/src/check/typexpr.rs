//! Type expression resolution.
//!
//! This module converts AST type expressions (TypeExpr) to internal types (TypeKey).
//! It handles type-checking of type expressions and resolves them to TypeKey values.


use vo_syntax::ast::Ident;
use vo_common::span::Span;

use crate::obj::EntityType;
use crate::objects::{ObjKey, ScopeKey, TypeKey};
use crate::operand::{Operand, OperandMode};
use crate::lookup;
use crate::scope;
use crate::typ::{self, ChanDir, Type};
use vo_syntax::ast::{self, Expr, FuncSig, Param, Receiver, TypeExpr, TypeExprKind, InterfaceElem};

use super::checker::{Checker, ObjContext};
use super::errors::TypeError;

impl Checker {
    // =========================================================================
    // Main type expression entry points
    // =========================================================================

    /// Type-checks the type expression and returns its type, or Invalid Type.
    pub fn type_expr(&mut self, ty: &TypeExpr) -> TypeKey {
        self.defined_type(ty, None)
    }

    /// Like type_expr but also accepts a type name def.
    /// If def is Some, ty is the type specification for the defined type def.
    pub fn defined_type(&mut self, ty: &TypeExpr, def: Option<TypeKey>) -> TypeKey {
        let t = self.type_internal(ty, def);
        debug_assert!(typ::is_typed(t, self.objs()));
        // Record the resolved type for this TypeExpr
        self.result.record_type_expr(ty.id, t);
        t
    }

    /// Like type_expr but breaks infinite size of recursive types.
    /// Used for pointer base types, slice/map element types, function params, etc.
    pub fn indirect_type(&mut self, ty: &TypeExpr) -> TypeKey {
        self.push_obj_path(self.universe().indir());
        let t = self.defined_type(ty, None);
        self.pop_obj_path();
        t
    }

    // =========================================================================
    // Internal type checking driver
    // =========================================================================

    /// Drives type checking of types. Must only be called by defined_type.
    fn type_internal(&mut self, ty: &TypeExpr, def: Option<TypeKey>) -> TypeKey {
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
                self.ident(&mut x, ident, def, true);
                match x.mode {
                    OperandMode::TypeExpr => {
                        set_underlying(x.typ, &mut self.tc_objs);
                        x.typ
                    }
                    OperandMode::Invalid => None,
                    _ => {
                        self.error_code_msg(TypeError::NotAType, ident.span, format!("{} is not a type", self.resolve_ident(ident)));
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
                    if let Some((_, pkg_obj)) = scope::lookup_parent(scope_key, &pkg_name, self.objs()) {
                        let entity = self.lobj(pkg_obj).entity_type().clone();
                        if let EntityType::PkgName { imported, .. } = entity {
                            let pkg_scope = *self.package(imported).scope();
                            if let Some(type_obj) = self.scope(pkg_scope).lookup(&type_name) {
                                self.result.record_use(sel_sel, type_obj);
                                let is_type = self.lobj(type_obj).entity_type().is_type_name();
                                let typ = self.lobj(type_obj).typ();
                                if is_type {
                                    if let Some(t) = typ {
                                        set_underlying(Some(t), &mut self.tc_objs);
                                        // Record type before early return (since we bypass defined_type's record)
                                        self.result.record_type_expr(ty.id, t);
                                        return t;
                                    }
                                }
                                self.error_code_msg(TypeError::NotAType, ty.span, format!("{}.{} is not a type", pkg_name, type_name));
                            } else {
                                self.error_code_msg(TypeError::NotAType, ty.span, format!("{}.{} is not a type", pkg_name, type_name));
                            }
                        } else {
                            self.error_code_msg(TypeError::NotAType, sel.pkg.span, format!("{} is not a package", pkg_name));
                        }
                    } else {
                        self.error_code_msg(TypeError::Undeclared, sel.pkg.span, format!("undeclared name: {}", pkg_name));
                    }
                }
                None
            }
            TypeExprKind::Array(arr) => {
                let len = self.array_len(&arr.len);
                let elem = self.type_expr(&arr.elem);
                let t = self.new_t_array(elem, len);
                set_underlying(Some(t), &mut self.tc_objs);
                Some(t)
            }
            TypeExprKind::Slice(elem) => {
                let elem_type = self.indirect_type(elem);
                let t = self.new_t_slice(elem_type);
                set_underlying(Some(t), &mut self.tc_objs);
                Some(t)
            }
            TypeExprKind::Map(map) => {
                let key = self.indirect_type(&map.key);
                let value = self.indirect_type(&map.value);
                let t = self.new_t_map(key, value);
                set_underlying(Some(t), &mut self.tc_objs);

                // Check map key is comparable (like goscript: delayed check via self.later)
                let key_span = map.key.span;
                let f = move |checker: &mut Checker| {
                    if !crate::typ::comparable(key, &checker.tc_objs) {
                        checker.error_code_msg(TypeError::InvalidOp, key_span, "invalid map key type");
                    }
                };
                self.later(Box::new(f));

                Some(t)
            }
            TypeExprKind::Chan(chan) => {
                let dir = match chan.dir {
                    ast::ChanDir::Both => ChanDir::SendRecv,
                    ast::ChanDir::Send => ChanDir::SendOnly,
                    ast::ChanDir::Recv => ChanDir::RecvOnly,
                };
                let elem = self.indirect_type(&chan.elem);
                let t = self.new_t_chan(dir, elem);
                set_underlying(Some(t), &mut self.tc_objs);
                Some(t)
            }
            TypeExprKind::Func(func) => {
                let t = self.func_type_ast(func);
                set_underlying(Some(t), &mut self.tc_objs);
                Some(t)
            }
            TypeExprKind::Struct(s) => {
                let t = self.struct_type(s);
                set_underlying(Some(t), &mut self.tc_objs);
                Some(t)
            }
            TypeExprKind::Pointer(base) => {
                let base_type = self.indirect_type(base);
                let t = self.new_t_pointer(base_type);
                set_underlying(Some(t), &mut self.tc_objs);
                
                // Spec: pointer types are only valid when base type is a struct
                // Delay this check because the base type may not be fully resolved yet
                // (e.g., recursive types like `type Node struct { left *Node }`)
                let base_span = base.span;
                let f = move |checker: &mut Checker| {
                    let invalid_type = checker.invalid_type();
                    if base_type == invalid_type {
                        return;
                    }
                    let underlying = typ::underlying_type(base_type, checker.objs());
                    if checker.otype(underlying).try_as_struct().is_none() {
                        checker.error_code_msg(
                            TypeError::PointerToNonStruct,
                            base_span,
                            format!("invalid pointer type *{} (base must be struct)", checker.type_str(base_type)),
                        );
                    }
                };
                self.later(Box::new(f));
                
                Some(t)
            }
            TypeExprKind::Interface(iface) => {
                let t = self.interface_type(iface, def);
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
    ) {
        x.mode = OperandMode::Invalid;
        x.clear_expr();

        let name = self.resolve_ident(ident);

        // Look up in scope
        if let Some(scope_key) = self.octx.scope {
            if let Some((_skey, okey)) = scope::lookup_parent(scope_key, name, self.objs()) {
                self.result.record_use(ident.clone(), okey);

                // Type-check the object if needed
                let obj = &self.lobj(okey);
                let mut otype = obj.typ();
                if otype.is_none() || (obj.entity_type().is_type_name() && want_type) {
                    self.obj_decl(okey, None);
                    otype = self.lobj(okey).typ();
                }

                let invalid_type = self.invalid_type();
                
                // Extract entity info first to avoid borrow conflicts
                let entity_type = self.lobj(okey).entity_type().clone();
                let obj_name = self.lobj(okey).name().to_string();

                match &entity_type {
                    EntityType::PkgName { .. } => {
                        self.error_code_msg(TypeError::PackageNotInSelector, ident.span, format!("use of package {} not in selector", obj_name));
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
                                self.error_code(TypeError::IotaOutsideConst, ident.span);
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
                        let obj_pkg = self.lobj(okey).pkg();
                        if obj_pkg == Some(self.pkg) {
                            self.lobj_mut(okey).set_var_used(true);
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
            self.error_code(TypeError::BlankAsValue, ident.span);
        } else {
            self.error_code_msg(TypeError::Undeclared, ident.span, format!("undeclared name: {}", name));
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
    fn array_len(&mut self, e: &Expr) -> Option<u64> {
        let mut x = Operand::new();
        self.expr(&mut x, e);
        if let OperandMode::Constant(v) = &x.mode {
            if let Some(t) = x.typ {
                if typ::is_untyped(t, self.objs()) || typ::is_integer(t, self.objs()) {
                    if let Some(n) = v.int_val().and_then(|i| u64::try_from(i).ok()) {
                        return Some(n);
                    }
                }
            }
            self.error_code(TypeError::ArrayLenNotInteger, e.span);
        } else if x.mode != OperandMode::Invalid {
            self.error_code(TypeError::ArrayLenNotConstant, e.span);
        }
        None
    }

    // =========================================================================
    // Function type checking
    // =========================================================================

    /// Type-checks a function type from AST.
    fn func_type_ast(&mut self, func: &ast::FuncType) -> TypeKey {
        // Create a new scope for the function
        let scope_key = self.new_scope(
            self.octx.scope,
            0, // pos
            0, // end
            "function",
            true,
        );

        // Collect parameters - FuncType uses Vec<Param> with names preserved
        let mut params = Vec::new();
        for param in &func.params {
            let ty = self.indirect_type(&param.ty);
            // Use first name if available, otherwise empty
            let name = param.names.first()
                .map(|n| self.resolve_ident(n).to_string())
                .unwrap_or_default();
            let var = self.new_param_var(Span::default(), Some(self.pkg), name, Some(ty));
            params.push(var);
        }
        let variadic = false; // FuncType in Vo doesn't have variadic marker

        // Collect results
        let mut results = Vec::new();
        for result in &func.results {
            let ty = self.indirect_type(&result.ty);
            let name = result.names.first()
                .map(|n| self.resolve_ident(n).to_string())
                .unwrap_or_default();
            let var = self.new_param_var(Span::default(), Some(self.pkg), name, Some(ty));
            results.push(var);
        }

        let params_tuple = self.new_tuple(params);
        let results_tuple = self.new_tuple(results);

        self.new_t_signature(Some(scope_key), None, params_tuple, results_tuple, variadic)
    }

    /// Type-checks a function signature and returns its type.
    /// Aligned with goscript's func_type implementation.
    pub fn func_type_from_sig(
        &mut self,
        recv: Option<&Receiver>,
        sig: &FuncSig,
    ) -> TypeKey {
        // Create a new scope for the function (like goscript)
        let scope_key = self.new_scope(
            self.octx.scope,
            0,
            0,
            "function",
            true,
        );
        // Record scope for this function signature (like goscript: self.result.record_scope(&ftype, skey))
        self.result.record_scope(sig.span, scope_key);

        // Collect receiver (like goscript's collect_params for recv)
        let recv_list = self.collect_receiver(scope_key, recv);
        
        // Collect params (like goscript's collect_params with variadic_ok=true)
        let (params, variadic) = self.collect_params_from_sig(scope_key, &sig.params, sig.variadic);
        
        // Collect results (like goscript's collect_params with variadic_ok=false)
        let (results, _) = self.collect_results_from_sig(scope_key, &sig.results);

        // Validate receiver (like goscript)
        let recv_okey = if recv.is_some() {
            let r = recv.unwrap();
            let invalid_type = self.invalid_type();
            
            // Vo Receiver is always exactly one, so recv_list has 0 or 1 element
            let recv_var = if recv_list.is_empty() {
                // This shouldn't happen with valid Vo syntax, but handle like goscript
                self.error_code(TypeError::MissingReceiver, r.span);
                self.new_param_var(Span::default(), None, String::new(), Some(invalid_type))
            } else {
                recv_list[0]
            };

            // spec: "The receiver type must be of the form T or *T where T is a type name."
            let recv_var_val = self.lobj(recv_var);
            let recv_type = recv_var_val.typ().unwrap();
            let (t, _) = crate::lookup::try_deref(recv_type, self.objs());
            
            if t != invalid_type {
                let err_msg = if let Some(n) = self.otype(t).try_as_named() {
                    // spec: "The type denoted by T is called the receiver base type; it must not
                    // be a pointer or interface type and it must be declared in the same package
                    // as the method."
                    if let Some(obj_key) = n.obj() {
                        if self.lobj(*obj_key).pkg() != Some(self.pkg) {
                            Some("type not defined in this package")
                        } else {
                            match self.otype(n.underlying()) {
                                Type::Pointer(_) | Type::Interface(_) => {
                                    Some("pointer or interface type")
                                }
                                _ => None,
                            }
                        }
                    } else {
                        None
                    }
                } else {
                    Some("basic or unnamed type")
                };
                
                if let Some(err) = err_msg {
                    self.error_code_msg(TypeError::InvalidReceiver, r.span, format!("invalid receiver ({})", err));
                    // ok to continue
                }
            }

            Some(recv_var)
        } else {
            None
        };

        let params_tuple = self.new_tuple(params);
        let results_tuple = self.new_tuple(results);

        self.new_t_signature(Some(scope_key), recv_okey, params_tuple, results_tuple, variadic)
    }

    /// Collect receiver parameter (adapted from goscript's collect_params for Vo Receiver).
    fn collect_receiver(
        &mut self,
        scope_key: ScopeKey,
        recv: Option<&Receiver>,
    ) -> Vec<ObjKey> {
        let Some(r) = recv else {
            return vec![];
        };

        // Build TypeExpr for base type
        let base_type_expr = TypeExpr {
            id: vo_syntax::ast::TypeExprId::DUMMY,
            kind: TypeExprKind::Ident(r.ty.clone()),
            span: r.ty.span,
        };
        
        // Resolve type using indirect_type (like goscript)
        let base_type = self.indirect_type(&base_type_expr);
        
        // Final type is T or *T
        let recv_type = if r.is_pointer {
            self.new_t_pointer_checked(base_type, r.ty.span)
        } else {
            base_type
        };

        // Create param var and declare (like goscript's collect_params)
        // For anonymous receivers, use empty name (receiver is unused in method body)
        let recv_name = r.name.as_ref()
            .map(|n| self.resolve_ident(n).to_string())
            .unwrap_or_default();
        let par = self.new_param_var(r.span, Some(self.pkg), recv_name, Some(recv_type));
        // Function parameters are visible from the start of the function scope
        let scope_pos = self.scope(scope_key).pos();
        self.declare(scope_key, par, scope_pos);
        // Only record def if receiver has a name
        if let Some(name) = &r.name {
            self.result.record_def(name.clone(), Some(par));
        }

        vec![par]
    }

    /// Collects function parameters from FuncSig AST.
    /// Aligned with goscript's collect_params: returns (vars, variadic).
    /// Like goscript: first resolve type T, then if variadic, change last param's type to []T.
    fn collect_params_from_sig(
        &mut self,
        scope_key: ScopeKey,
        params: &[Param],
        variadic_ok: bool,
    ) -> (Vec<ObjKey>, bool) {
        if params.is_empty() {
            return (vec![], false);
        }

        let mut named = false;
        let mut anonymous = false;
        let mut vars = Vec::new();

        for param in params.iter() {
            // Resolve parameter type (without slice wrapping first, like goscript)
            let param_type = self.indirect_type(&param.ty);

            if param.names.is_empty() {
                // Anonymous parameter
                let var = self.new_param_var(param.ty.span, Some(self.pkg), String::new(), Some(param_type));
                // Record implicit object (like goscript: self.result.record_implicit(fkey, par))
                self.result.record_implicit(param.ty.span, var);
                vars.push(var);
                anonymous = true;
            } else {
                // Named parameters
                for name in &param.names {
                    let name_str = self.resolve_ident(name).to_string();
                    if name_str.is_empty() {
                        // This is an invalid case, but continue like goscript
                    }
                    let var = self.new_param_var(name.span, Some(self.pkg), name_str, Some(param_type));
                    let scope_pos = self.scope(scope_key).pos();
                    self.declare(scope_key, var, scope_pos);
                    self.result.record_def(name.clone(), Some(var));
                    vars.push(var);
                }
                named = true;
            }
        }

        // Check mixed named/anonymous (like goscript)
        if named && anonymous {
            // Vo parser should prevent this, but check anyway
            // goscript: self.invalid_ast(...)
        }

        // For variadic function, change last param's type from T to []T (like goscript)
        // goscript does this AFTER collecting all params
        let variadic = variadic_ok && !vars.is_empty() && !params.is_empty();
        if variadic {
            let last = vars[vars.len() - 1];
            let t = self.new_t_slice(self.lobj(last).typ().unwrap());
            self.lobj_mut(last).set_type(Some(t));
            // Note: goscript also calls record_type_and_value for the ellipsis expression,
            // but Vo uses TypeExpr (not Expr), so we skip this recording
        }

        (vars, variadic)
    }

    /// Collects function results from FuncSig AST.
    /// Like goscript's collect_params with variadic_ok=false.
    fn collect_results_from_sig(
        &mut self,
        scope_key: ScopeKey,
        results: &[ast::ResultParam],
    ) -> (Vec<ObjKey>, bool) {
        if results.is_empty() {
            return (vec![], false);
        }

        let mut vars = Vec::new();

        for result in results {
            let result_type = self.indirect_type(&result.ty);

            if let Some(name) = &result.name {
                let name_str = self.resolve_ident(name).to_string();
                let var = self.new_param_var(name.span, Some(self.pkg), name_str, Some(result_type));
                let scope_pos = self.scope(scope_key).pos();
                self.declare(scope_key, var, scope_pos);
                self.result.record_def(name.clone(), Some(var));
                vars.push(var);
            } else {
                let var = self.new_param_var(result.ty.span, Some(self.pkg), String::new(), Some(result_type));
                vars.push(var);
            }
        }

        (vars, false) // results are never variadic
    }

    /// Creates a new tuple type.
    pub fn new_tuple(&mut self, vars: Vec<ObjKey>) -> TypeKey {
        self.new_t_tuple(vars)
    }

    /// Type-checks the type expression (or nil value) and returns the type, or None for nil.
    /// If e is neither a type nor nil, returns Invalid type.
    pub fn type_or_nil(&mut self, e: &Expr) -> Option<TypeKey> {
        let mut x = Operand::new();
        self.raw_expr(&mut x, e, None);
        let invalid_type = self.invalid_type();
        match x.mode {
            OperandMode::Invalid => Some(invalid_type),
            OperandMode::NoValue => {
                self.error_code(TypeError::UsedAsType, e.span);
                Some(invalid_type)
            }
            OperandMode::TypeExpr => x.typ,
            _ => {
                if x.mode == OperandMode::Value && x.is_nil(self.objs()) {
                    None
                } else {
                    self.error_code(TypeError::NotAType, e.span);
                    Some(invalid_type)
                }
            }
        }
    }

    // =========================================================================
    // Struct type checking
    // =========================================================================

    /// Type-checks a struct type.
    fn struct_type(&mut self, s: &ast::StructType) -> TypeKey {
        if s.fields.is_empty() {
            return self.new_t_struct(Vec::new(), None);
        }

        let mut fields: Vec<ObjKey> = Vec::new();
        let mut tags: Option<Vec<Option<String>>> = None;
        let mut field_set: std::collections::HashMap<String, ObjKey> = std::collections::HashMap::new();

        for field in &s.fields {
            let field_type = self.type_expr(&field.ty);
            let tag = field.tag.as_ref().map(|t| t.value.clone());

            if field.names.is_empty() {
                // Embedded field
                // spec: "An embedded type must be specified as a type name T or as a pointer
                // to a non-interface type name *T, and T itself may not be a pointer type."
                let invalid_type = self.invalid_type();
                
                // For embedded fields, extract the type name
                let embedded_name = self.get_embedded_field_name(&field.ty);
                
                // Check embedded field constraints
                let (underlying, is_ptr) = lookup::try_deref(field_type, self.objs());
                let underlying_type = typ::underlying_type(underlying, self.objs());
                
                let is_valid = match &self.otype(underlying_type) {
                    Type::Basic(_) if underlying_type == invalid_type => false,
                    Type::Pointer(_) => {
                        self.error_code(TypeError::EmbeddedPointer, field.ty.span);
                        false
                    }
                    Type::Interface(_) if is_ptr => {
                        self.error_code(TypeError::EmbeddedPointerInterface, field.ty.span);
                        false
                    }
                    _ => true,
                };

                let final_type = if is_valid { field_type } else { invalid_type };
                let fld = self.new_field(
                    field.ty.span,
                    Some(self.pkg),
                    embedded_name.clone(),
                    Some(final_type),
                    true,
                );
                
                // Check for duplicate field
                if embedded_name != "_" {
                    if let Some(&prev) = field_set.get(&embedded_name) {
                        self.error_code_msg(TypeError::FieldRedeclared, field.ty.span, format!("{} redeclared", embedded_name));
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
                    let fld = self.new_field(
                        name.span,
                        Some(self.pkg),
                        name_str.clone(),
                        Some(field_type),
                        false,
                    );
                    
                    // Check for duplicate field
                    if name_str != "_" {
                        if let Some(&prev) = field_set.get(&name_str) {
                            self.error_code_msg(TypeError::FieldRedeclared, name.span, format!("{} redeclared", name_str));
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

        self.new_t_struct(fields, tags)
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
    /// Aligned with goscript's interface_type: uses info_from_type_lit to collect all methods.
    fn interface_type(&mut self, iface: &ast::InterfaceType, def: Option<TypeKey>) -> TypeKey {
        if iface.elems.is_empty() {
            return self.new_t_empty_interface();
        }

        // Create the interface type first (methods will be added later)
        let itype = self.new_t_interface(vec![], vec![]);

        // Collect embedded interface idents for delayed processing (like goscript)
        let mut embedded_idents: Vec<Ident> = Vec::new();
        for elem in &iface.elems {
            if let InterfaceElem::Embedded(ident) = elem {
                embedded_idents.push(ident.clone());
            }
        }

        // Delay embedded interface checking (like goscript: self.later)
        // Only collects embeds - does NOT call complete() here
        if !embedded_idents.is_empty() {
            let f = move |checker: &mut Checker| {
                let mut embeds: Vec<TypeKey> = Vec::new();
                let invalid_type = checker.invalid_type();
                
                for ident in &embedded_idents {
                    let name = checker.resolve_ident(ident);
                    if let Some(scope_key) = checker.octx.scope {
                        if let Some((_, okey)) = scope::lookup_parent(scope_key, name, &checker.tc_objs) {
                            let typ = checker.tc_objs.lobjs[okey].typ();
                            if let Some(t) = typ {
                                if t == invalid_type {
                                    continue;
                                }
                                let underlying = typ::underlying_type(t, &checker.tc_objs);
                                match &checker.tc_objs.types[underlying] {
                                    Type::Interface(embed) => {
                                        // Correct embedded interfaces must be complete
                                        debug_assert!(embed.all_methods().is_some());
                                        embeds.push(t);
                                    }
                                    _ => {
                                        checker.error_code_msg(TypeError::NotAType, ident.span, format!("{} is not an interface", name));
                                    }
                                }
                            }
                        }
                    }
                }
                
                if let Type::Interface(iface_detail) = &mut checker.tc_objs.types[itype] {
                    *iface_detail.embeddeds_mut() = embeds;
                }
            };
            self.later(Box::new(f));
        }

        // Compute method set using info_from_type_lit (like goscript)
        let (tname, path) = if let Some(d) = def {
            if let Some(named) = self.otype(d).try_as_named() {
                let obj = named.obj().clone();
                (obj, obj.map(|o| vec![o]).unwrap_or_default())
            } else {
                (None, vec![])
            }
        } else {
            (None, vec![])
        };
        
        let scope = self.octx.scope.unwrap_or(self.universe().scope());
        let info = self.info_from_type_lit(scope, iface, tname, &path);
        
        if info.is_none() || info.as_ref().unwrap().is_empty() {
            // Empty interface or error - exit early
            if let Some(iface_detail) = self.otype(itype).try_as_interface() {
                iface_detail.set_empty_complete();
            }
            return itype;
        }

        // Use named receiver type if available (for better error messages)
        let recv_type = def.unwrap_or(itype);

        // Correct receiver type for all methods explicitly declared
        // by this interface after we're done with type-checking at this level.
        // (like goscript's second self.later)
        let f = move |checker: &mut Checker| {
            if let Some(iface_detail) = checker.tc_objs.types[itype].try_as_interface() {
                for &m in iface_detail.methods().iter() {
                    let t = checker.tc_objs.lobjs[m].typ().unwrap();
                    if let Type::Signature(sig) = &checker.tc_objs.types[t] {
                        if let Some(recv_var) = sig.recv() {
                            checker.tc_objs.lobjs[*recv_var].set_type(Some(recv_type));
                        }
                    }
                }
            }
        };
        self.later(Box::new(f));

        // Two-phase processing (like goscript):
        // Phase 1: Create method objects with empty signatures, call set_func
        // Phase 2: Fix signatures using minfo.src_index() to get AST
        
        let info_ref = info.unwrap();
        let explicits = info_ref.explicits;
        let mut sig_fix: Vec<super::interface::MethodInfo> = vec![];
        
        // Phase 1: Create method objects
        for (i, minfo) in info_ref.methods.iter().enumerate() {
            let fun = if minfo.func().is_none() {
                // Method not yet type-checked, get name from AST using src_index
                let src_index = minfo.src_index().expect("MethodInfo must have src_index when func is None");
                let method_ast = match &iface.elems[src_index] {
                    InterfaceElem::Method(m) => m,
                    _ => panic!("src_index should point to a Method element"),
                };
                let name = self.resolve_ident(&method_ast.name).to_string();
                
                // Create receiver
                let recv_var = self.new_var(Span::default(), Some(self.pkg), String::new(), Some(recv_type));
                
                // Create empty signature (will be fixed in phase 2)
                let empty_tuple = self.new_t_tuple(vec![]);
                let sig_type = self.new_t_signature(None, Some(recv_var), empty_tuple, empty_tuple, false);

                let fun_key = self.new_func(method_ast.name.span, Some(self.pkg), name.clone(), Some(sig_type), false);
                
                // Record definition for the method
                self.result.record_def(method_ast.name.clone(), Some(fun_key));
                
                minfo.set_func(fun_key);
                sig_fix.push(minfo.clone());
                fun_key
            } else {
                minfo.func().unwrap()
            };
            
            // Add to interface type
            if let Type::Interface(iface_detail) = &mut self.otype_mut(itype) {
                if i < explicits {
                    iface_detail.methods_mut().push(fun);
                }
                iface_detail.all_methods_push(fun);
            }
        }

        // Phase 2: Fix signatures now that we have collected all methods
        // (possibly embedded) methods must be type-checked within their scope
        let saved_context = self.octx.clone();
        for minfo in sig_fix {
            let src_index = minfo.src_index().unwrap();
            let method_ast = match &iface.elems[src_index] {
                InterfaceElem::Method(m) => m,
                _ => continue,
            };
            
            // Type-check method signature within its scope (like goscript)
            self.octx = ObjContext::new();
            self.octx.scope = minfo.scope();
            
            // Type-check the method signature
            let sig_type = self.func_type_from_sig(None, &method_ast.sig);
            
            // Update the method's signature, keeping the receiver
            let fun_key = minfo.func().unwrap();
            let old_sig_type = self.lobj(fun_key).typ().unwrap();
            let recv = if let Type::Signature(old_sig) = &self.otype(old_sig_type) {
                *old_sig.recv()
            } else {
                None
            };
            
            if let Type::Signature(sig) = &mut self.otype_mut(sig_type) {
                sig.set_recv(recv);
            }
            
            // Update the function's type to the new signature
            self.lobj_mut(fun_key).set_type(Some(sig_type));
        }
        self.octx = saved_context;

        // Sort methods by name - collect data first to avoid borrow conflicts
        if let Type::Interface(iface_detail) = &self.otype(itype) {
            let mut methods: Vec<_> = iface_detail.methods().iter()
                .map(|&k| (k, self.lobj(k).name().to_string()))
                .collect();
            methods.sort_by(|a, b| a.1.cmp(&b.1));
            let sorted_methods: Vec<_> = methods.into_iter().map(|(k, _)| k).collect();
            
            let all_methods_opt = iface_detail.all_methods().clone();
            let sorted_all = all_methods_opt.map(|all| {
                let mut all_with_names: Vec<_> = all.iter()
                    .map(|&k| (k, self.lobj(k).name().to_string()))
                    .collect();
                all_with_names.sort_by(|a, b| a.1.cmp(&b.1));
                all_with_names.into_iter().map(|(k, _)| k).collect()
            });
            
            // Now update with mutable borrow
            if let Type::Interface(iface_detail) = &mut self.otype_mut(itype) {
                *iface_detail.methods_mut() = sorted_methods;
                *iface_detail.all_methods_mut() = sorted_all;
            }
        }

        itype
    }

}
