//! Declaration type checking.
//!
//! This module type-checks package-level and local declarations including:
//! - Constant declarations (const)
//! - Variable declarations (var)
//! - Type declarations (type)
//! - Function declarations (func)
//!
//! Adapted from goscript with GoX-specific modifications.


use gox_common::span::Span;
use gox_syntax::ast::{Expr, TypeExpr};

use crate::constant::Value as ConstValue;
use crate::obj::{EntityType, ObjColor};
use crate::objects::{DeclInfoKey, ObjKey, ScopeKey, TypeKey};
use crate::operand::Operand;
use crate::scope::Scope;
use crate::typ;

use super::checker::{Checker, ObjContext};
use super::resolver::DeclInfo;

impl Checker {
    /// Reports the location of an alternative declaration.
    pub fn report_alt_decl(&self, okey: ObjKey) {
        let lobj = self.lobj(okey);
        self.error(Span::default(), format!("\tother declaration of {}", lobj.name()));
    }

    /// Declares an object in a scope.
    /// Returns error if name already exists (except for blank identifier "_").
    pub fn declare(&mut self, skey: ScopeKey, okey: ObjKey) {
        // spec: "The blank identifier, represented by the underscore
        // character _, may be used in a declaration like any other
        // identifier but the declaration does not introduce a new binding."
        if self.lobj(okey).name() != "_" {
            let alt = Scope::insert(skey, okey, &mut self.tc_objs);
            if let Some(o) = alt {
                let lobj = self.lobj(okey);
                self.error(
                    Span::default(),
                    format!("{} redeclared in this block", lobj.name()),
                );
                self.report_alt_decl(o);
                return;
            }
        }
    }

    /// Type-checks an object declaration.
    /// Uses color-based cycle detection (White -> Gray -> Black).
    pub fn obj_decl(&mut self, okey: ObjKey, def: Option<TypeKey>) {
        // During type-checking, white objects may be assigned a type without
        // traversing through obj_decl. Update colors of those objects here.
        if self.lobj(okey).color() == ObjColor::White && self.lobj(okey).typ().is_some() {
            self.lobj_mut(okey).set_color(ObjColor::Black);
            return;
        }

        match self.lobj(okey).color() {
            ObjColor::White => {
                debug_assert!(self.lobj(okey).typ().is_none());
                let idx = self.push_obj_path(okey);
                self.lobj_mut(okey).set_color(ObjColor::Gray(idx));

                let dkey = match self.obj_map.get(&okey) {
                    Some(&k) => k,
                    None => {
                        // Object not in obj_map - predeclared or imported
                        let popped = self.pop_obj_path();
                        self.lobj_mut(popped).set_color(ObjColor::Black);
                        return;
                    }
                };

                let d = self.decl_info(dkey);
                // Create a new octx for the checker
                let mut octx = ObjContext::new();
                octx.scope = Some(d.file_scope());
                std::mem::swap(&mut self.octx, &mut octx);

                let lobj = &self.lobj(okey);
                match lobj.entity_type() {
                    EntityType::Const { .. } => {
                        self.octx.decl = Some(dkey);
                        if let DeclInfo::Const(cd) = self.decl_info(dkey) {
                            let (typ, init) = (cd.typ.clone(), cd.init.clone());
                            self.const_decl(okey, &typ, &init);
                        }
                    }
                    EntityType::Var { .. } => {
                        self.octx.decl = Some(dkey);
                        if let DeclInfo::Var(vd) = self.decl_info(dkey) {
                            let (lhs, typ, init) = (vd.lhs.clone(), vd.typ.clone(), vd.init.clone());
                            self.var_decl(okey, lhs.as_ref(), &typ, &init);
                        }
                    }
                    EntityType::TypeName => {
                        if let DeclInfo::Type(td) = self.decl_info(dkey) {
                            let (typ, alias) = (td.typ.clone(), td.alias);
                            self.type_decl(okey, &typ, def, alias);
                        }
                    }
                    EntityType::Func { .. } => {
                        self.func_decl(okey, dkey);
                    }
                    _ => {}
                }

                // Restore octx
                std::mem::swap(&mut self.octx, &mut octx);
                let popped = self.pop_obj_path();
                self.lobj_mut(popped).set_color(ObjColor::Black);
            }
            ObjColor::Black => {
                debug_assert!(self.lobj(okey).typ().is_some());
            }
            ObjColor::Gray(_) => {
                // We have a cycle.
                let invalid_type = self.invalid_type();
                let lobj = &self.lobj(okey);
                match lobj.entity_type() {
                    EntityType::Const { .. } | EntityType::Var { .. } => {
                        if self.invalid_type_cycle(okey) || lobj.typ().is_none() {
                            self.lobj_mut(okey).set_type(Some(invalid_type));
                        }
                    }
                    EntityType::TypeName => {
                        if self.invalid_type_cycle(okey) {
                            self.lobj_mut(okey).set_type(Some(invalid_type));
                        }
                    }
                    EntityType::Func { .. } => {
                        // Don't set obj.typ to Invalid here - functions need a Signature type
                        let _ = self.invalid_type_cycle(okey);
                    }
                    _ => {}
                }
                debug_assert!(self.lobj(okey).typ().is_some());
            }
        }
    }

    /// Returns true if the cycle starting with obj is invalid and reports an error.
    pub fn invalid_type_cycle(&self, okey: ObjKey) -> bool {
        let lobj = self.lobj(okey);
        let mut has_indir = false;
        let mut has_type_def = false;
        let mut nval = 0;

        let start = match lobj.color() {
            ObjColor::Gray(v) => v,
            _ => return false,
        };

        let cycle = &self.obj_path[start..];
        let mut ncycle = cycle.len();

        for o in cycle {
            let oval = self.lobj(*o);
            match oval.entity_type() {
                EntityType::Const { .. } | EntityType::Var { .. } => {
                    nval += 1;
                }
                EntityType::TypeName => {
                    // Check if it's the indirection marker
                    if self.universe().indir() == *o {
                        ncycle -= 1;
                        has_indir = true;
                    } else {
                        // Check if it's an alias
                        let alias = if let Some(&d) = self.obj_map.get(o) {
                            if let DeclInfo::Type(td) = self.decl_info(d) {
                                td.alias
                            } else {
                                false
                            }
                        } else {
                            false
                        };
                        if !alias {
                            has_type_def = true;
                        }
                    }
                }
                EntityType::Func { .. } => {} // ignored for now
                _ => {}
            }
        }

        // A cycle involving only constants and variables is invalid but we
        // ignore them here because they are reported via the initialization
        // cycle check.
        if nval == ncycle {
            return false;
        }

        // A cycle involving only types (and possibly functions) must have at
        // least one indirection and one type definition to be permitted.
        if nval == 0 && has_indir && has_type_def {
            return false; // cycle is permitted
        }

        // Report error
        self.error(
            Span::default(),
            format!("illegal cycle in declaration of {}", lobj.name()),
        );
        for o in cycle {
            if self.universe().indir() == *o {
                continue;
            }
            self.error(Span::default(), format!("\t{} refers to", self.lobj(*o).name()));
        }
        self.error(Span::default(), format!("\t{}", lobj.name()));

        true
    }

    /// Type-checks a constant declaration.
    pub fn const_decl(
        &mut self,
        okey: ObjKey,
        typ: &Option<TypeExpr>,
        init: &Option<Expr>,
    ) {
        debug_assert!(self.lobj(okey).typ().is_none());

        // Set iota value
        let iota_val = self.lobj(okey).const_val().clone();
        self.octx.iota = Some(iota_val);

        // Provide valid constant value under all circumstances
        self.lobj_mut(okey).set_const_val(ConstValue::Unknown);

        // Determine type, if any
        if let Some(e) = typ {
            let t = self.type_expr(e);
            let tval = &self.otype(t);
            if !tval.is_const_type(self.objs()) {
                let invalid_type = self.invalid_type();
                if tval.underlying().unwrap_or(t) != invalid_type {
                    self.error(Span::default(), "invalid constant type".to_string());
                }
                self.lobj_mut(okey).set_type(Some(invalid_type));
                self.octx.iota = None;
                return;
            }
            self.lobj_mut(okey).set_type(Some(t));
        }

        let mut x = Operand::new();
        if let Some(expr) = init {
            self.expr(&mut x, expr);
        }
        self.init_const(okey, &mut x);

        // Clear iota
        self.octx.iota = None;
    }

    /// Type-checks a variable declaration.
    pub fn var_decl(
        &mut self,
        okey: ObjKey,
        lhs: Option<&Vec<ObjKey>>,
        typ: &Option<TypeExpr>,
        init: &Option<Expr>,
    ) {
        debug_assert!(self.lobj(okey).typ().is_none());

        // Determine type, if any
        if let Some(texpr) = typ {
            let t = self.type_expr(texpr);
            self.lobj_mut(okey).set_type(Some(t));
        }

        // Check initialization
        if init.is_none() {
            if typ.is_none() {
                let invalid = self.invalid_type();
                self.lobj_mut(okey).set_type(Some(invalid));
            }
            return;
        }

        if lhs.is_none() || lhs.as_ref().unwrap().len() == 1 {
            let mut x = Operand::new();
            self.expr(&mut x, init.as_ref().unwrap());
            self.init_var(okey, &mut x, "variable declaration");
            return;
        }

        // Multiple variables on LHS with one init expr
        if typ.is_some() {
            let t = self.lobj(okey).typ();
            for o in lhs.as_ref().unwrap().iter() {
                self.lobj_mut(*o).set_type(t);
            }
        }

        self.init_vars(lhs.as_ref().unwrap(), &[init.clone().unwrap()], None);
    }

    /// Type-checks a type declaration.
    pub fn type_decl(
        &mut self,
        okey: ObjKey,
        typ: &TypeExpr,
        def: Option<TypeKey>,
        alias: bool,
    ) {
        debug_assert!(self.lobj(okey).typ().is_none());

        if alias {
            let invalid = self.invalid_type();
            self.lobj_mut(okey).set_type(Some(invalid));
            let t = self.type_expr(typ);
            self.lobj_mut(okey).set_type(Some(t));
        } else {
            let named_key = self.new_t_named(Some(okey), None, vec![]);
            if let Some(d) = def {
                if let Some(named) = self.otype_mut(d).try_as_named_mut() {
                    named.set_underlying(named_key);
                }
            }
            // Make sure recursive type declarations terminate
            self.lobj_mut(okey).set_type(Some(named_key));

            // Determine underlying type of named
            self.defined_type(typ, Some(named_key));

            // Resolve forward chain to final unnamed underlying type
            let underlying = typ::deep_underlying_type(named_key, self.objs());
            if let Some(named) = self.otype_mut(named_key).try_as_named_mut() {
                named.set_underlying(underlying);
            }
        }

        self.add_method_decls(okey);
    }

    /// Type-checks a function declaration.
    pub fn func_decl(&mut self, okey: ObjKey, dkey: DeclInfoKey) {
        debug_assert!(self.lobj(okey).typ().is_none());
        debug_assert!(self.octx.iota.is_none());

        // Set guard signature to prevent infinite recursion
        let guard_sig = self.universe().guard_sig();
        self.lobj_mut(okey).set_type(Some(guard_sig));

        // Get function declaration info
        let d = self.decl_info(dkey);
        if let DeclInfo::Func(fd) = d {
            let fdecl = fd.fdecl.clone();

            // Type-check function signature (with receiver if present)
            let sig_key = self.func_type_from_sig(fdecl.receiver.as_ref(), &fdecl.sig);
            self.lobj_mut(okey).set_type(Some(sig_key));

            // Check for 'init' func
            let sig = self.otype(sig_key).try_as_signature().unwrap();
            let lobj = &self.lobj(okey);
            if sig.recv().is_none()
                && lobj.name() == "init"
                && (sig.params_count(self.objs()) > 0 || sig.results_count(self.objs()) > 0)
            {
                self.error(
                    Span::default(),
                    "func init must have no arguments and no return values".to_string(),
                );
            }

            // Queue function body for later checking
            if fdecl.body.is_some() {
                let name = lobj.name().to_string();
                let body = fdecl.body.clone();
                self.later(Box::new(move |checker: &mut Checker| {
                    if let Some(b) = &body {
                        checker.func_body(None, &name, sig_key, b, None);
                    }
                }));
            }
        }
    }

    /// Adds method declarations to a type.
    pub fn add_method_decls(&mut self, okey: ObjKey) {
        // Get associated methods
        if !self.methods.contains_key(&okey) {
            return;
        }
        let methods = self.methods.remove(&okey).unwrap();

        let type_key = match self.lobj(okey).typ() {
            Some(t) => t,
            None => return,
        };

        // Collect existing field names and methods to check for duplicates
        let mut mset: std::collections::HashMap<String, ObjKey> = std::collections::HashMap::new();

        if let Some(named) = self.otype(type_key).try_as_named() {
            // Add struct fields if underlying is a struct
            if let Some(struc) = self.otype(named.underlying()).try_as_struct() {
                for f in struc.fields().iter() {
                    let fname = self.lobj(*f).name();
                    if fname != "_" {
                        mset.insert(fname.to_string(), *f);
                    }
                }
            }
            // Add existing methods
            for m in named.methods().iter() {
                let mname = self.lobj(*m).name();
                debug_assert!(mname != "_");
                mset.insert(mname.to_string(), *m);
            }
        }

        // Filter valid methods and check for duplicates
        let mut valids: Vec<ObjKey> = Vec::new();
        for m in methods {
            let mobj = self.lobj(m);
            let mname = mobj.name().to_string();
            debug_assert!(mname != "_");

            if let Some(&alt) = mset.get(&mname) {
                let alt_obj = self.lobj(alt);
                match alt_obj.entity_type() {
                    EntityType::Var { .. } => {
                        self.error(
                            Span::default(),
                            format!("field and method with the same name {}", mname),
                        );
                    }
                    EntityType::Func { .. } => {
                        self.error(
                            Span::default(),
                            format!("method {} already declared", mname),
                        );
                    }
                    _ => {}
                }
                self.report_alt_decl(alt);
            } else {
                mset.insert(mname, m);
                valids.push(m);
            }
        }

        // Append valid methods to the named type
        if let Some(named) = self.otype_mut(type_key).try_as_named_mut() {
            named.methods_mut().append(&mut valids);
        }
    }

    /// Type-checks a declaration statement (const, var, or type) inside a function.
    pub fn decl_stmt(&mut self, decl: &gox_syntax::ast::Decl) {
        use gox_syntax::ast::Decl;

        match decl {
            Decl::Const(cdecl) => {
                let mut last_full_spec: Option<&gox_syntax::ast::ConstSpec> = None;

                for (iota, spec) in cdecl.specs.iter().enumerate() {
                    let top = self.delayed_count();

                    // Determine which spec to use for type/values
                    let current_spec = if spec.ty.is_some() || !spec.values.is_empty() {
                        last_full_spec = Some(spec);
                        Some(spec)
                    } else {
                        last_full_spec
                    };

                    // Create and type-check each constant
                    let lhs: Vec<ObjKey> = spec
                        .names
                        .iter()
                        .enumerate()
                        .map(|(i, name)| {
                            let okey = self.new_const(
                                0,
                                Some(self.pkg),
                                self.resolve_ident(name).to_string(),
                                None,
                                ConstValue::with_i64(iota as i64),
                            );

                            let init = current_spec
                                .and_then(|s| s.values.get(i).cloned());
                            let typ = current_spec
                                .and_then(|s| s.ty.clone());

                            self.const_decl(okey, &typ, &init);
                            self.result.record_def(name.clone(), Some(okey));
                            okey
                        })
                        .collect();

                    self.arity_match_const(spec, current_spec);

                    // Process function literals before scope changes
                    self.process_delayed(top);

                    // Declare constants in current scope
                    if let Some(scope) = self.octx.scope {
                        for okey in lhs {
                            self.declare(scope, okey);
                        }
                    }
                }
            }

            Decl::Var(vdecl) => {
                for spec in &vdecl.specs {
                    let top = self.delayed_count();

                    // Create variables
                    let vars: Vec<ObjKey> = spec
                        .names
                        .iter()
                        .map(|name| {
                            let okey = self.new_var(
                                0,
                                Some(self.pkg),
                                self.resolve_ident(name).to_string(),
                                None,
                            );
                            self.result.record_def(name.clone(), Some(okey));
                            okey
                        })
                        .collect();

                    // Type-check based on init pattern
                    let n_to_1 = spec.values.len() == 1 && spec.names.len() > 1;
                    if n_to_1 {
                        // Multiple vars, single init expression
                        self.var_decl(
                            vars[0],
                            Some(&vars),
                            &spec.ty,
                            &Some(spec.values[0].clone()),
                        );
                    } else {
                        // 1-to-1 or no init
                        for (i, &okey) in vars.iter().enumerate() {
                            self.var_decl(
                                okey,
                                None,
                                &spec.ty,
                                &spec.values.get(i).cloned(),
                            );
                        }
                    }

                    self.arity_match_var(spec);

                    // Process function literals before scope changes
                    self.process_delayed(top);

                    // Declare variables in current scope
                    if let Some(scope) = self.octx.scope {
                        for okey in vars {
                            self.declare(scope, okey);
                        }
                    }
                }
            }

            Decl::Type(tdecl) => {
                let okey = self.new_type_name(
                    0,
                    Some(self.pkg),
                    self.resolve_ident(&tdecl.name).to_string(),
                    None,
                );
                self.result.record_def(tdecl.name.clone(), Some(okey));

                // Declare in scope first (type scope starts at identifier)
                if let Some(scope) = self.octx.scope {
                    self.declare(scope, okey);
                }

                // Mark gray and type-check
                let idx = self.push_obj_path(okey);
                self.lobj_mut(okey).set_color(ObjColor::Gray(idx));
                // GoX doesn't have type aliases in the same way as Go
                self.type_decl(okey, &tdecl.ty, None, false);
                let popped = self.pop_obj_path();
                self.lobj_mut(popped).set_color(ObjColor::Black);
            }

            Decl::Func(_) => {
                self.error(Span::default(), "unexpected function declaration in statement".to_string());
            }
        }
    }

    /// Checks arity for constant declarations.
    fn arity_match_const(
        &self,
        spec: &gox_syntax::ast::ConstSpec,
        init_spec: Option<&gox_syntax::ast::ConstSpec>,
    ) {
        let l = spec.names.len();
        let r = init_spec.map_or(0, |s| s.values.len());

        if r == 0 {
            // No init expressions at all - error for const
            self.error(spec.span, "missing value in const declaration".to_string());
        } else if l < r {
            // More values than names
            if l < spec.values.len() {
                self.error(spec.values[l].span, "extra init expr".to_string());
            } else if let Some(_is) = init_spec {
                self.error(spec.span, "extra init expr from previous spec".to_string());
            }
        } else if l > r {
            // More names than values
            self.error(spec.span, format!("missing init expr for {}", self.resolve_ident(&spec.names[r])));
        }
    }

    /// Checks arity for variable declarations.
    fn arity_match_var(&self, spec: &gox_syntax::ast::VarSpec) {
        let l = spec.names.len();
        let r = spec.values.len();

        if r == 0 {
            // No init expressions - must have type
            if spec.ty.is_none() {
                self.error(spec.span, "missing type or init expr".to_string());
            }
        } else if l < r {
            // More values than names
            self.error(spec.values[l].span, "extra init expr".to_string());
        } else if l > r && r != 1 {
            // More names than values (unless it's N-to-1 assignment)
            self.error(spec.span, format!("assignment mismatch: {} variables but {} values", l, r));
        }
    }
}
