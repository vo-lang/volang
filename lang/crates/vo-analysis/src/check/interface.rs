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
//! Adapted from goscript with Vo-specific modifications.


use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use vo_common::span::Span;
use vo_syntax::ast::{InterfaceElem, InterfaceType};

use crate::objects::{ObjKey, ScopeKey, TCObjects};
use crate::typ::{self, InterfaceDetail, Type};

use super::checker::Checker;
use super::errors::TypeError;

/// Shared reference to IfaceInfo.
pub type RcIfaceInfo = Rc<IfaceInfo>;

/// Information about an interface method.
/// At least one of src or func must be Some.
/// (Methods declared in the current package have a non-None scope
/// and src, and eventually a non-None func field; imported and pre-
/// declared methods have a None scope and src, and only a non-None func field.)
/// Uses Rc<RefCell<>> for interior mutability (like goscript).
#[derive(Debug, Clone)]
pub struct MethodInfo {
    data: Rc<RefCell<MethodInfoData>>,
}

#[derive(Debug)]
struct MethodInfoData {
    /// Scope of interface method; or None.
    scope: Option<ScopeKey>,
    /// Index of the method in the interface's elems array; or None.
    /// This is Vo's equivalent of goscript's FieldKey.
    src_index: Option<usize>,
    /// Corresponding fully type-checked method (LangObj::Func); or None.
    func: Option<ObjKey>,
}

impl MethodInfo {
    pub(crate) fn with_func(func: ObjKey) -> MethodInfo {
        MethodInfo {
            data: Rc::new(RefCell::new(MethodInfoData {
                scope: None,
                src_index: None,
                func: Some(func),
            })),
        }
    }

    pub(crate) fn with_scope_src(scope: ScopeKey, src_index: usize) -> MethodInfo {
        MethodInfo {
            data: Rc::new(RefCell::new(MethodInfoData {
                scope: Some(scope),
                src_index: Some(src_index),
                func: None,
            })),
        }
    }

    pub(crate) fn scope(&self) -> Option<ScopeKey> {
        self.data.borrow().scope
    }

    pub(crate) fn src_index(&self) -> Option<usize> {
        self.data.borrow().src_index
    }

    pub(crate) fn func(&self) -> Option<ObjKey> {
        self.data.borrow().func
    }

    pub(crate) fn set_func(&self, func: ObjKey) {
        self.data.borrow_mut().func = Some(func);
    }

    /// Returns the method name (requires func to be set).
    pub(crate) fn method_name(&self, tc_objs: &TCObjects) -> String {
        if let Some(okey) = self.func() {
            tc_objs.lobjs[okey].name().to_string()
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
    pub(crate) fn new(explicits: usize, methods: Vec<MethodInfo>) -> IfaceInfo {
        IfaceInfo { explicits, methods }
    }

    pub(crate) fn new_empty() -> IfaceInfo {
        IfaceInfo::new(0, vec![])
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.methods.is_empty()
    }
}

impl Checker {
    /// info_from_type_lit computes the method set for the given interface iface
    /// declared in scope.
    /// If a corresponding type name exists (tname is Some), it is used for
    /// cycle detection and to cache the method set.
    /// The result is the method set, or None if there is a cycle via embedded
    /// interfaces.
    /// If tname is not None it must be the last element in path.
    /// Aligned with goscript/types/src/check/interface.rs::info_from_type_lit
    pub(crate) fn info_from_type_lit(
        &mut self,
        skey: ScopeKey,
        iface: &InterfaceType,
        tname: Option<ObjKey>,
        path: &Vec<ObjKey>,
    ) -> Option<RcIfaceInfo> {
        // If the interface is named, check if we computed info already.
        //
        // This is not simply an optimization; we may run into stack
        // overflow with recursive interface declarations.
        //
        // While at it, use the same mechanism to detect cycles.
        if let Some(okey) = tname {
            debug_assert!(path[path.len() - 1] == okey);
            if let Some(info) = self.ifaces.get(&okey) {
                let cloned_info = info.clone();
                if info.is_none() {
                    // We have a cycle and use has_cycle to report it.
                    let yes = self.has_cycle(okey, path, true);
                    assert!(yes);
                }
                return cloned_info;
            } else {
                // computation started but not complete
                self.ifaces.insert(okey, None);
            }
        }

        let iinfo = if iface.elems.is_empty() {
            Rc::new(IfaceInfo::new_empty())
        } else {
            let mut mset: HashMap<String, MethodInfo> = HashMap::new();
            let mut methods = vec![];
            let mut embeddeds = vec![];
            let mut positions = vec![];

            for (elem_index, elem) in iface.elems.iter().enumerate() {
                match elem {
                    InterfaceElem::Method(m) => {
                        // We have a method with name.
                        // spec: "As with all method sets, in an interface type,
                        // each method must have a unique non-blank name."
                        let name = self.resolve_ident(&m.name).to_string();
                        if name == "_" {
                            self.error_code_msg(TypeError::InvalidOp, m.span, "invalid method name _");
                            continue;
                        }

                        let mi = MethodInfo::with_scope_src(skey, elem_index);
                        if self.declare_in_method_set(&mut mset, &name, mi.clone(), m.span) {
                            methods.push(mi);
                        }
                    }
                    InterfaceElem::Embedded(ident) => {
                        // We have an embedded interface with simple name.
                        let type_name = self.resolve_ident(ident).to_string();
                        if let Some(e) = self.info_from_type_name(skey, &type_name, path) {
                            embeddeds.push(e);
                            positions.push(ident.span);
                        }
                    }
                    InterfaceElem::EmbeddedQualified { pkg, name, span } => {
                        // We have an embedded interface with qualified name (pkg.Type).
                        let pkg_name = self.resolve_ident(pkg).to_string();
                        let type_name = self.resolve_ident(name).to_string();
                        if let Some(e) = self.info_from_qualified_type_name(skey, &pkg_name, &type_name) {
                            embeddeds.push(e);
                            positions.push(*span);
                        }
                    }
                }
            }

            let explicits = methods.len();

            // collect methods of embedded interfaces
            for (i, e) in embeddeds.into_iter().enumerate() {
                let pos = positions[i];
                for m in e.methods.iter() {
                    let name = m.method_name(self.objs());
                    if self.declare_in_method_set(&mut mset, &name, m.clone(), pos) {
                        methods.push(m.clone());
                    }
                }
            }

            Rc::new(IfaceInfo::new(explicits, methods))
        };

        // mark ifaces as complete
        if let Some(okey) = tname {
            self.ifaces.insert(okey, Some(iinfo.clone()));
        }

        Some(iinfo)
    }

    /// info_from_type_name computes the method set for the given type name
    /// which must denote a type whose underlying type is an interface.
    /// The same result qualifications apply as for info_from_type_lit.
    /// info_from_type_name should only be called from info_from_type_lit.
    /// Aligned with goscript/types/src/check/interface.rs::info_from_type_name
    fn info_from_type_name(
        &mut self,
        skey: ScopeKey,
        name: &str,
        path: &Vec<ObjKey>,
    ) -> Option<RcIfaceInfo> {
        // A single call of info_from_type_name handles a sequence of (possibly
        // recursive) type declarations connected via unqualified type names.
        let start = path.len();
        let mut cur_path = path.clone();
        let mut cur_name = name.to_string();

        loop {
            let lookup = crate::scope::lookup_parent(skey, &cur_name, self.objs());
            if lookup.is_none() {
                break;
            }
            let (_, tname) = lookup.unwrap();
            let tname_val = self.lobj(tname);

            if !tname_val.entity_type().is_type_name() {
                break;
            }

            // We have a type name. It may be predeclared (error type),
            // imported (dot import), or declared by a type declaration.
            // It may not be an interface (e.g., predeclared type int).
            // Resolve it by analyzing each possible case.

            // Abort but don't report an error if we have a "type name only"
            // cycle (see goscript comment).
            if self.has_cycle(tname, &cur_path[start..], false) {
                break;
            }

            // Abort and report an error if we have a general cycle.
            if self.has_cycle(tname, &cur_path, true) {
                break;
            }

            cur_path.push(tname);

            // If tname is a package-level type declaration, check obj_map.
            // Follow the RHS of that declaration if so.
            if let Some(decl_key) = self.obj_map.get(&tname).copied() {
                use super::resolver::DeclInfo;
                use vo_syntax::ast::TypeExprKind;
                
                // Extract data we need before mutable borrow
                let decl = &self.tc_objs.decls[decl_key];
                let type_info = if let DeclInfo::Type(type_decl) = decl {
                    Some((type_decl.file_scope, type_decl.typ.kind.clone()))
                } else {
                    None
                };
                
                if let Some((file_scope, type_kind)) = type_info {
                    // Check what the RHS type expression is
                    match type_kind {
                        TypeExprKind::Ident(ident) => {
                            // type tname T - follow T
                            cur_name = self.resolve_ident(&ident).to_string();
                            continue;
                        }
                        TypeExprKind::Interface(iface_lit) => {
                            // type tname interface{...}
                            return self.info_from_type_lit(
                                file_scope,
                                &iface_lit,
                                Some(tname),
                                &cur_path,
                            );
                        }
                        // type tname X // and X is not an interface type
                        _ => break,
                    }
                } else {
                    break;
                }
            } else {
                // If tname is not a package-level declaration, in a well-typed
                // program it should be a predeclared (error type), imported (dot
                // import), or function local declaration. Either way, it should
                // have been fully declared before use, except if there is a direct
                // cycle, and direct cycles will be caught above. Also, the denoted
                // type should be an interface (e.g., int is not an interface).
                if let Some(ty) = tname_val.typ() {
                    let ty = typ::underlying_type(ty, self.objs());
                    if let Type::Interface(i) = self.otype(ty) {
                        return Some(self.info_from_type(i));
                    }
                }
                break;
            }
        }
        None
    }

    /// info_from_qualified_type_name returns the method set for the given qualified
    /// type name (pkg.Type), or None.
    /// Aligned with goscript/types/src/check/interface.rs::info_from_qualified_type_mame
    fn info_from_qualified_type_name(
        &self,
        skey: ScopeKey,
        pkg_name: &str,
        type_name: &str,
    ) -> Option<RcIfaceInfo> {
        // Look up the package name in scope using the standalone lookup_parent function
        use crate::scope::lookup_parent;
        let (_, obj) = lookup_parent(skey, pkg_name, self.objs())?;
        let obj_val = self.lobj(obj);

        // Check if it's a package name
        use crate::obj::EntityType;
        if let EntityType::PkgName { imported, .. } = obj_val.entity_type() {
            // Look up the type in the imported package's scope
            let imported_val = &self.tc_objs.pkgs[*imported];
            let pkg_scope = *imported_val.scope();
            let scope = &self.tc_objs.scopes[pkg_scope];
            
            if let Some(obj2) = scope.lookup(type_name) {
                let obj_val2 = self.lobj(obj2);
                
                // Check if it's exported
                if !obj_val2.exported() {
                    return None;
                }
                
                // Check if it's a type name
                if !obj_val2.entity_type().is_type_name() {
                    return None;
                }
                
                // Get the underlying type and check if it's an interface
                // Note: This requires the imported package to be fully type-checked first.
                // If typ() is None, the imported package hasn't been processed yet.
                if let Some(t) = obj_val2.typ() {
                    let t = typ::underlying_type(t, self.objs());
                    if let Type::Interface(iface) = self.otype(t) {
                        return Some(self.info_from_type(iface));
                    }
                }
            }
        }
        None
    }

    /// info_from_type computes the method set for the given interface type.
    /// Aligned with goscript/types/src/check/interface.rs::info_from_type
    fn info_from_type(&self, iface: &InterfaceDetail) -> RcIfaceInfo {
        let all_methods_ref = iface.all_methods();
        let all_methods = all_methods_ref.as_ref().unwrap();
        let all_methods_len = all_methods.len();

        let mut mis: Vec<MethodInfo> = iface
            .methods()
            .iter()
            .map(|x| MethodInfo::with_func(*x))
            .collect();

        if all_methods_len == iface.methods().len() {
            return Rc::new(IfaceInfo::new(all_methods_len, mis));
        }

        // there are embedded methods, put them after explicit methods
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

    /// like declare_in_set but for method infos.
    /// Aligned with goscript/types/src/check/interface.rs::declare_in_method_set
    /// 
    /// Per Go spec: if the same method is inherited from multiple embedded interfaces
    /// with identical signatures, it's allowed (diamond pattern). Only report error
    /// if signatures differ.
    fn declare_in_method_set(
        &self,
        set: &mut HashMap<String, MethodInfo>,
        name: &str,
        mi: MethodInfo,
        pos: Span,
    ) -> bool {
        // Check if method already exists
        if let Some(existing) = set.get(name) {
            // Both methods have func set (from embedded interfaces) - compare signatures
            if let (Some(existing_func), Some(new_func)) = (existing.func(), mi.func()) {
                let existing_type = self.lobj(existing_func).typ();
                let new_type = self.lobj(new_func).typ();
                
                // If signatures are identical (structurally), this is valid (diamond pattern)
                // Use typ::identical_o to compare types structurally, not by TypeKey
                if typ::identical_o(existing_type, new_type, self.objs()) {
                    return true;
                }
                
                // Signatures differ - this is an error
                self.error_code_msg(
                    TypeError::MethodRedeclared, 
                    pos, 
                    format!("{} redeclared with different signature", name)
                );
                return false;
            }
            
            // One or both don't have func set yet (explicit methods being collected)
            // This is a redeclaration error
            self.error_code_msg(TypeError::MethodRedeclared, pos, format!("{} redeclared", name));
            return false;
        }
        
        // Method doesn't exist yet, add it
        set.insert(name.to_string(), mi);
        true
    }

}
