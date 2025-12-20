//! Declaration resolver - collects and organizes package-level declarations.
//!
//! This module handles the first pass of type checking: collecting all
//! package-level declarations and organizing them for later type checking.

#![allow(dead_code)]

use std::collections::HashSet;

use gox_common_core::ExprId;

use crate::objects::{ObjKey, ScopeKey};

/// NodeId for referencing AST nodes - uses ExprId for now.
pub type NodeId = ExprId;

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
