//! Interface checking.
//!
//! This module implements collection of interface methods without relying on
//! partially computed types of methods or interfaces for interface types
//! declared at the package level.
//!
//! Because interfaces must not embed themselves, directly or indirectly,
//! the method set of a valid interface can always be computed independent
//! of any cycles that might exist via method signatures.
//!
//! Adapted from goscript with GoX-specific modifications.

#![allow(dead_code)]

use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use gox_common::span::Span;
use gox_syntax::ast::{InterfaceElem, InterfaceType};

use crate::obj::EntityType;
use crate::objects::{ObjKey, PackageKey, ScopeKey, TCObjects, TypeKey};
use crate::typ::{self, InterfaceDetail, Type};

use super::checker::{Checker, FilesContext};

/// Shared reference to IfaceInfo.
pub type RcIfaceInfo = Rc<IfaceInfo>;

/// Information about an interface method.
/// At least one of scope or func must be Some.
#[derive(Debug, Clone)]
pub struct MethodInfo {
    /// Scope of interface method; or None.
    scope: Option<ScopeKey>,
    /// Method name (for methods not yet type-checked).
    name: Option<String>,
    /// Corresponding fully type-checked method (LangObj::Func); or None.
    func: Option<ObjKey>,
}

impl MethodInfo {
    pub fn with_func(func: ObjKey) -> MethodInfo {
        MethodInfo {
            scope: None,
            name: None,
            func: Some(func),
        }
    }

    pub fn with_scope_name(scope: ScopeKey, name: String) -> MethodInfo {
        MethodInfo {
            scope: Some(scope),
            name: Some(name),
            func: None,
        }
    }

    pub fn scope(&self) -> Option<ScopeKey> {
        self.scope
    }

    pub fn func(&self) -> Option<ObjKey> {
        self.func
    }

    pub fn set_func(&mut self, func: ObjKey) {
        self.func = Some(func);
    }

    /// Returns the method name.
    pub fn method_name(&self, tc_objs: &TCObjects) -> String {
        if let Some(okey) = self.func {
            tc_objs.lobjs[okey].name().to_string()
        } else {
            self.name.clone().unwrap_or_default()
        }
    }

    /// Returns the method id (for duplicate checking).
    pub fn id(&self, _pkg: PackageKey, tc_objs: &TCObjects) -> String {
        if let Some(okey) = self.func {
            tc_objs.lobjs[okey].id(tc_objs).to_string()
        } else if let Some(name) = &self.name {
            name.clone()
        } else {
            String::new()
        }
    }
}

/// Interface method set information.
#[derive(Debug, Clone, Default)]
pub struct IfaceInfo {
    /// Number of explicitly declared methods.
    pub explicits: usize,
    /// All methods (explicit + embedded).
    pub methods: Vec<MethodInfo>,
}

impl IfaceInfo {
    pub fn new(explicits: usize, methods: Vec<MethodInfo>) -> IfaceInfo {
        IfaceInfo { explicits, methods }
    }

    pub fn new_empty() -> IfaceInfo {
        IfaceInfo::new(0, vec![])
    }

    pub fn is_empty(&self) -> bool {
        self.methods.is_empty()
    }
}

impl Checker {
    /// Computes method set for an interface from its InterfaceDetail.
    /// This is used for interfaces that have already been type-checked.
    pub fn info_from_interface_detail(&self, iface: &InterfaceDetail) -> RcIfaceInfo {
        let all_methods_ref = iface.all_methods();
        let all_methods = match all_methods_ref.as_ref() {
            Some(m) => m,
            None => {
                // Interface not complete, return just explicit methods
                let mis: Vec<MethodInfo> = iface
                    .methods()
                    .iter()
                    .map(|x| MethodInfo::with_func(*x))
                    .collect();
                return Rc::new(IfaceInfo::new(mis.len(), mis));
            }
        };
        let all_methods_len = all_methods.len();

        let mut mis: Vec<MethodInfo> = iface
            .methods()
            .iter()
            .map(|x| MethodInfo::with_func(*x))
            .collect();

        if all_methods_len == iface.methods().len() {
            return Rc::new(IfaceInfo::new(all_methods_len, mis));
        }

        // There are embedded methods, put them after explicit methods
        let set: HashSet<ObjKey> = iface.methods().iter().copied().collect();
        let mut embedded: Vec<MethodInfo> = all_methods
            .iter()
            .filter_map(|x| {
                if set.contains(x) {
                    None
                } else {
                    Some(MethodInfo::with_func(*x))
                }
            })
            .collect();
        mis.append(&mut embedded);
        Rc::new(IfaceInfo::new(iface.methods().len(), mis))
    }

    /// Computes method set for an interface from its type.
    pub fn info_from_type(&self, iface_type: TypeKey) -> RcIfaceInfo {
        let iface = match &self.tc_objs.types[iface_type] {
            Type::Interface(i) => i,
            _ => return Rc::new(IfaceInfo::new_empty()),
        };
        self.info_from_interface_detail(iface)
    }

    /// Computes method set for an interface literal (from AST).
    /// If a corresponding type name exists (tname is Some), it is used for
    /// cycle detection and to cache the method set.
    /// Returns None if there is a cycle via embedded interfaces.
    pub fn info_from_type_lit(
        &mut self,
        scope: ScopeKey,
        iface: &InterfaceType,
        tname: Option<ObjKey>,
        path: &Vec<ObjKey>,
        fctx: &mut FilesContext,
    ) -> Option<RcIfaceInfo> {
        // If the interface is named, check if we computed info already.
        // This prevents stack overflow with recursive interface declarations.
        if let Some(okey) = tname {
            debug_assert!(path.last() == Some(&okey));
            if let Some(info) = fctx.ifaces.get(&okey) {
                if info.is_none() {
                    // We have a cycle
                    self.has_cycle(okey, path, true);
                }
                return info.clone();
            } else {
                // Computation started but not complete
                fctx.ifaces.insert(okey, None);
            }
        }

        let iinfo = if iface.elems.is_empty() {
            Rc::new(IfaceInfo::new_empty())
        } else {
            let mut mset: HashMap<String, MethodInfo> = HashMap::new();
            let mut methods = vec![];
            let mut embeddeds = vec![];

            for elem in &iface.elems {
                match elem {
                    InterfaceElem::Method(m) => {
                        let name = self.resolve_ident(&m.name).to_string();
                        if name == "_" {
                            self.error(m.span, "invalid method name _".to_string());
                            continue;
                        }

                        let mi = MethodInfo::with_scope_name(scope, name.clone());
                        if self.declare_in_method_set(&mut mset, name.clone(), mi.clone(), m.span) {
                            methods.push(mi);
                        }
                    }
                    InterfaceElem::Embedded(ident) => {
                        // Look up embedded interface
                        let type_name = self.resolve_ident(ident).to_string();
                        if let Some(e) = self.info_from_type_name(scope, &type_name, path, fctx) {
                            embeddeds.push((e, ident.span));
                        }
                    }
                }
            }

            let explicits = methods.len();

            // Collect methods of embedded interfaces
            for (e, _span) in embeddeds {
                for m in e.methods.iter() {
                    let name = m.method_name(&self.tc_objs);
                    if self.declare_in_method_set(&mut mset, name, m.clone(), Span::default()) {
                        methods.push(m.clone());
                    }
                }
            }

            Rc::new(IfaceInfo::new(explicits, methods))
        };

        // Mark as complete
        if let Some(okey) = tname {
            fctx.ifaces.insert(okey, Some(iinfo.clone()));
        }

        Some(iinfo)
    }

    /// Computes method set for a type name that should denote an interface.
    fn info_from_type_name(
        &mut self,
        scope: ScopeKey,
        name: &str,
        path: &Vec<ObjKey>,
        _fctx: &mut FilesContext,
    ) -> Option<RcIfaceInfo> {
        let start = path.len();
        let mut cur_path = path.clone();

        // Look up the type name
        let lookup = self.scope(scope).lookup_parent(name, &self.tc_objs);
        if lookup.is_none() {
            return None;
        }
        let (_, tname) = lookup.unwrap();
        let tname_val = self.lobj(tname);

        if !tname_val.entity_type().is_type_name() {
            return None;
        }

        // Check for cycles
        if self.has_cycle(tname, &cur_path[start..], false) {
            return None;
        }
        if self.has_cycle(tname, &cur_path, true) {
            return None;
        }

        cur_path.push(tname);

        // If tname has a type, check if it's an interface
        if let Some(ty) = tname_val.typ() {
            let ty = typ::underlying_type(ty, &self.tc_objs);
            if let Type::Interface(iface) = &self.tc_objs.types[ty] {
                return Some(self.info_from_interface_detail(iface));
            }
        }

        None
    }

    /// Computes method set for a qualified type name (pkg.Type).
    fn info_from_qualified_type_name(
        &self,
        scope: ScopeKey,
        pkg_name: &str,
        type_name: &str,
    ) -> Option<RcIfaceInfo> {
        // Look up the package
        let lookup = self.scope(scope).lookup_parent(pkg_name, &self.tc_objs);
        if lookup.is_none() {
            return None;
        }
        let (_, obj1) = lookup.unwrap();
        let obj_val = self.lobj(obj1);

        if let EntityType::PkgName { imported, .. } = obj_val.entity_type() {
            let imported_val = self.package(*imported);
            let pkg_scope = self.scope(*imported_val.scope());

            if let Some(obj2) = pkg_scope.lookup(type_name) {
                let obj_val2 = self.lobj(obj2);
                if !obj_val2.exported() {
                    return None;
                }
                if obj_val2.entity_type().is_type_name() {
                    if let Some(ty) = obj_val2.typ() {
                        let ty = typ::underlying_type(ty, &self.tc_objs);
                        if let Type::Interface(iface) = &self.tc_objs.types[ty] {
                            return Some(self.info_from_interface_detail(iface));
                        }
                    }
                }
            }
        }
        None
    }

    /// Computes method set for an interface.
    pub fn interface_method_set(&self, iface: TypeKey) -> IfaceInfo {
        let iface_detail = match &self.tc_objs.types[iface] {
            Type::Interface(i) => i,
            _ => return IfaceInfo::new_empty(),
        };

        let rc = self.info_from_interface_detail(iface_detail);
        (*rc).clone()
    }

    /// Checks if type T implements interface I.
    pub fn implements(&self, t: TypeKey, iface: TypeKey) -> bool {
        self.missing_method(t, iface).is_none()
    }

    /// Computes method set of a type.
    pub fn method_set(&self, _t: TypeKey) -> HashMap<String, ObjKey> {
        // Full implementation would compute the method set for any type
        HashMap::new()
    }

    /// Returns missing method info if T doesn't implement I.
    /// Returns (method_name, have_type, want_type) or None if T implements I.
    pub fn missing_method(
        &self,
        _t: TypeKey,
        iface: TypeKey,
    ) -> Option<(String, Option<TypeKey>, Option<TypeKey>)> {
        let iface_detail = match &self.tc_objs.types[iface] {
            Type::Interface(i) => i,
            _ => return None,
        };

        // Get all methods of the interface
        let all_methods_ref = iface_detail.all_methods();
        let all_methods = match all_methods_ref.as_ref() {
            Some(m) => m,
            None => return None,
        };

        if all_methods.is_empty() {
            return None; // Empty interface, anything implements it
        }

        // For each method in the interface, check if t has it
        // Full implementation would use lookup_field_or_method
        // For now, just return None (assumes implementation)
        None
    }

    /// Declares a method in a method set, checking for duplicates.
    pub fn declare_in_method_set(
        &self,
        set: &mut HashMap<String, MethodInfo>,
        name: String,
        mi: MethodInfo,
        pos: Span,
    ) -> bool {
        if let Some(alt) = set.insert(name.clone(), mi) {
            // Duplicate method
            self.error(pos, format!("{} redeclared", name));
            let _ = alt; // Would report other declaration location
            false
        } else {
            true
        }
    }
}
