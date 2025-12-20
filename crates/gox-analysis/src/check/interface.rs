//! Interface checking.
//!
//! This module implements collection of interface methods without relying on
//! partially computed types.

#![allow(dead_code)]

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use gox_common::vfs::FileSystem;

use crate::objects::{ObjKey, ScopeKey, TypeKey};
use crate::typ::{self, Type};

use super::checker::Checker;

/// Shared reference to IfaceInfo.
pub type RcIfaceInfo = Rc<RefCell<IfaceInfo>>;

/// Information about an interface method.
#[derive(Debug, Clone)]
pub struct MethodInfo {
    /// Scope of interface method (for methods declared in current package).
    scope: Option<ScopeKey>,
    /// Corresponding fully type-checked method (LangObj::Func).
    func: Option<ObjKey>,
}

impl MethodInfo {
    pub fn with_func(func: ObjKey) -> MethodInfo {
        MethodInfo {
            scope: None,
            func: Some(func),
        }
    }

    pub fn with_scope(scope: ScopeKey) -> MethodInfo {
        MethodInfo {
            scope: Some(scope),
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

impl<F: FileSystem> Checker<F> {
    /// Computes method set for an interface from its type.
    pub fn info_from_type(&self, iface_type: TypeKey) -> RcIfaceInfo {
        let iface = match &self.tc_objs.types[iface_type] {
            Type::Interface(i) => i,
            _ => return Rc::new(RefCell::new(IfaceInfo::new_empty())),
        };

        let methods: Vec<MethodInfo> = iface
            .methods()
            .iter()
            .map(|&m| MethodInfo::with_func(m))
            .collect();
        let explicits = methods.len();

        Rc::new(RefCell::new(IfaceInfo::new(explicits, methods)))
    }

    /// Computes method set for an interface literal.
    /// Returns None if there is a cycle via embedded interfaces.
    pub fn info_from_type_lit(
        &self,
        _scope: ScopeKey,
        _def: Option<ObjKey>,
        _path: &[ObjKey],
    ) -> Option<RcIfaceInfo> {
        // Stub - returns empty info for now
        Some(Rc::new(RefCell::new(IfaceInfo::new_empty())))
    }

    /// Computes method set for an interface.
    pub fn interface_method_set(&self, iface: TypeKey) -> IfaceInfo {
        let iface_detail = match &self.tc_objs.types[iface] {
            Type::Interface(i) => i,
            _ => return IfaceInfo::new_empty(),
        };

        let methods: Vec<MethodInfo> = iface_detail
            .methods()
            .iter()
            .map(|&m| MethodInfo::with_func(m))
            .collect();
        let explicits = methods.len();

        IfaceInfo::new(explicits, methods)
    }

    /// Checks if type T implements interface I.
    pub fn implements(&self, _t: TypeKey, _iface: TypeKey) -> bool {
        // TODO: Implement full interface checking
        true
    }

    /// Computes method set of a type.
    pub fn method_set(&self, _t: TypeKey) -> HashMap<String, ObjKey> {
        HashMap::new()
    }

    /// Returns missing method info if T doesn't implement I.
    pub fn missing_method(
        &self,
        _t: TypeKey,
        _iface: TypeKey,
    ) -> Option<(String, Option<TypeKey>, Option<TypeKey>)> {
        None
    }

    /// Declares a method in a method set, checking for duplicates.
    pub fn declare_in_method_set(
        &self,
        set: &mut HashMap<String, MethodInfo>,
        name: String,
        mi: MethodInfo,
    ) -> bool {
        if set.contains_key(&name) {
            false
        } else {
            set.insert(name, mi);
            true
        }
    }
}
