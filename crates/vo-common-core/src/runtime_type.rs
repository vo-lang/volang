//! Runtime type representation for type identity checking.
//!
//! This module defines `RuntimeType`, a complete representation of Go types
//! that enables runtime type identity checks according to Go spec.
//!
//! Type identity rules (from Go spec):
//! - Two array types are identical if they have identical element types and the same array length.
//! - Two slice types are identical if they have identical element types.
//! - Two struct types are identical if they have the same sequence of fields, and if corresponding
//!   pairs of fields have the same names, identical types, and identical tags, and are either both
//!   embedded or both not embedded. Non-exported field names from different packages are always different.
//! - Two pointer types are identical if they have identical base types.
//! - Two function types are identical if they have the same number of parameters and result values,
//!   corresponding parameter and result types are identical, and either both functions are variadic
//!   or neither is. Parameter and result names are not required to match.
//! - Two interface types are identical if they define the same type set.
//! - Two map types are identical if they have identical key and element types.
//! - Two channel types are identical if they have identical element types and the same direction.

#[cfg(not(feature = "std"))]
use alloc::boxed::Box;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use crate::symbol::Symbol;
use crate::types::ValueKind;

/// Runtime type representation for type identity checking.
/// 
/// All nested types store rttid (u32) instead of Box<RuntimeType>
/// to enable O(1) type lookup at runtime.
#[derive(Debug, Clone)]
pub enum RuntimeType {
    /// Basic types: int, string, bool, float64, etc.
    Basic(ValueKind),
    
    /// Named type, identified by named_type_id.
    /// Named types are always different from any other type.
    Named(u32),
    
    /// Pointer type: *T - stores elem rttid
    Pointer(u32),
    
    /// Array type: [N]T - stores elem rttid
    Array {
        len: u64,
        elem: u32,
    },
    
    /// Slice type: []T - stores elem rttid
    Slice(u32),
    
    /// Map type: map[K]V - stores key and val rttid
    Map {
        key: u32,
        val: u32,
    },
    
    /// Channel type: chan T, chan<- T, <-chan T - stores elem rttid
    Chan {
        dir: ChanDir,
        elem: u32,
    },
    
    /// Function type: func(params) results - stores param/result rttids
    Func {
        params: Vec<u32>,
        results: Vec<u32>,
        variadic: bool,
    },
    
    /// Anonymous struct type
    Struct {
        fields: Vec<StructField>,
    },
    
    /// Anonymous interface type
    Interface {
        methods: Vec<InterfaceMethod>,
    },
    
    /// Tuple type (for function multi-value returns) - stores elem rttids
    Tuple(Vec<u32>),
}

/// Channel direction.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum ChanDir {
    /// Bidirectional: chan T
    Both = 0,
    /// Send-only: chan<- T
    Send = 1,
    /// Receive-only: <-chan T
    Recv = 2,
}

/// A field in an anonymous struct type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructField {
    /// Field name (interned symbol).
    pub name: Symbol,
    /// Field type rttid.
    pub typ: u32,
    /// Struct tag (Symbol::DUMMY if no tag).
    pub tag: Symbol,
    /// Whether this field is embedded.
    pub embedded: bool,
    /// Package where the field is defined (for non-exported field identity).
    /// Symbol::DUMMY for exported fields.
    pub pkg: Symbol,
}

/// A method in an anonymous interface type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InterfaceMethod {
    /// Method name (interned symbol).
    pub name: Symbol,
    /// Method signature rttid (must point to RuntimeType::Func).
    pub sig: u32,
}

impl RuntimeType {
    /// Returns true if this is a named type.
    #[inline]
    pub fn is_named(&self) -> bool {
        matches!(self, RuntimeType::Named(_))
    }
    
    /// If this is a Named type, returns the named_type_id.
    #[inline]
    pub fn as_named(&self) -> Option<u32> {
        match self {
            RuntimeType::Named(id) => Some(*id),
            _ => None,
        }
    }
    
    /// Returns true if this is a basic type.
    #[inline]
    pub fn is_basic(&self) -> bool {
        matches!(self, RuntimeType::Basic(_))
    }
}

impl StructField {
    /// Creates a new struct field.
    pub fn new(name: Symbol, typ: u32, tag: Symbol, embedded: bool, pkg: Symbol) -> Self {
        Self { name, typ, tag, embedded, pkg }
    }
}

impl InterfaceMethod {
    /// Creates a new interface method.
    pub fn new(name: Symbol, sig: u32) -> Self {
        Self { name, sig }
    }
}

// Manual PartialEq/Eq/Hash implementation for RuntimeType
// Interface comparison uses method set equality (order-independent)
impl PartialEq for RuntimeType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Basic(a), Self::Basic(b)) => a == b,
            (Self::Named(a), Self::Named(b)) => a == b,
            (Self::Pointer(a), Self::Pointer(b)) => a == b,
            (Self::Array { len: l1, elem: e1 }, Self::Array { len: l2, elem: e2 }) => l1 == l2 && e1 == e2,
            (Self::Slice(a), Self::Slice(b)) => a == b,
            (Self::Map { key: k1, val: v1 }, Self::Map { key: k2, val: v2 }) => k1 == k2 && v1 == v2,
            (Self::Chan { dir: d1, elem: e1 }, Self::Chan { dir: d2, elem: e2 }) => d1 == d2 && e1 == e2,
            (Self::Func { params: p1, results: r1, variadic: v1 }, Self::Func { params: p2, results: r2, variadic: v2 }) => {
                p1 == p2 && r1 == r2 && v1 == v2
            }
            (Self::Struct { fields: f1 }, Self::Struct { fields: f2 }) => f1 == f2,
            (Self::Interface { methods: m1 }, Self::Interface { methods: m2 }) => {
                // Interface equality: same method set (order-independent)
                if m1.len() != m2.len() {
                    return false;
                }
                // For each method in m1, find matching method in m2
                m1.iter().all(|method| m2.contains(method))
            }
            (Self::Tuple(a), Self::Tuple(b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for RuntimeType {}

impl core::hash::Hash for RuntimeType {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            Self::Basic(vk) => vk.hash(state),
            Self::Named(id) => id.hash(state),
            Self::Pointer(elem) => elem.hash(state),
            Self::Array { len, elem } => { len.hash(state); elem.hash(state); }
            Self::Slice(elem) => elem.hash(state),
            Self::Map { key, val } => { key.hash(state); val.hash(state); }
            Self::Chan { dir, elem } => { dir.hash(state); elem.hash(state); }
            Self::Func { params, results, variadic } => {
                params.hash(state); results.hash(state); variadic.hash(state);
            }
            Self::Struct { fields } => fields.hash(state),
            Self::Interface { methods } => {
                // Hash method count and sum of (name ^ sig) for order-independence
                methods.len().hash(state);
                let mut combined: u64 = 0;
                for m in methods {
                    combined = combined.wrapping_add(m.name.as_u32() as u64 ^ m.sig as u64);
                }
                combined.hash(state);
            }
            Self::Tuple(elems) => elems.hash(state),
        }
    }
}
