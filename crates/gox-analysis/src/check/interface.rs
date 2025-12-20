//! Interface checking (stub implementation).

#![allow(dead_code)]

use std::collections::HashMap;

use gox_common::vfs::FileSystem;

use crate::objects::{ObjKey, TypeKey};

use super::checker::Checker;

/// Information about an interface method.
#[derive(Debug, Clone)]
pub struct MethodInfo {
    pub name: String,
    pub func: Option<ObjKey>,
    pub typ: Option<TypeKey>,
}

/// Interface method set information.
#[derive(Debug, Clone, Default)]
pub struct IfaceInfo {
    pub explicits: usize,
    pub methods: Vec<MethodInfo>,
}

impl<F: FileSystem> Checker<F> {
    /// Computes method set for an interface (stub).
    pub fn interface_method_set(&self, _iface: TypeKey) -> IfaceInfo {
        IfaceInfo::default()
    }

    /// Checks if type T implements interface I (stub).
    pub fn implements(&self, _t: TypeKey, _iface: TypeKey) -> bool {
        // TODO: Implement full interface checking
        true
    }

    /// Computes method set of a type (stub).
    pub fn method_set(&self, _t: TypeKey) -> HashMap<String, MethodInfo> {
        HashMap::new()
    }

    /// Returns missing method info if T doesn't implement I.
    pub fn missing_method(&self, _t: TypeKey, _iface: TypeKey) -> Option<(String, Option<TypeKey>, Option<TypeKey>)> {
        None
    }
}
