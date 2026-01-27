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
use alloc::string::String;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use crate::types::{ValueKind, ValueRttid};

/// Runtime type representation for type identity checking.
/// 
/// All nested types store ValueRttid = (rttid << 8) | value_kind
/// to enable O(1) type and value_kind lookup at runtime without table queries.
#[derive(Debug, Clone)]
pub enum RuntimeType {
    /// Basic types: int, string, bool, float64, etc.
    Basic(ValueKind),
    
    /// Named type, identified by named_type_id.
    /// Named types are always different from any other type.
    /// `struct_meta_id` is set if underlying type is a struct (for dynamic access).
    Named {
        id: u32,
        struct_meta_id: Option<u32>,
    },
    
    /// Pointer type: *T
    Pointer(ValueRttid),
    
    /// Array type: [N]T
    Array {
        len: u64,
        elem: ValueRttid,
    },
    
    /// Slice type: []T
    Slice(ValueRttid),
    
    /// Map type: map[K]V
    Map {
        key: ValueRttid,
        val: ValueRttid,
    },
    
    /// Channel type: chan T, chan<- T, <-chan T
    Chan {
        dir: ChanDir,
        elem: ValueRttid,
    },
    
    /// Function type: func(params) results
    Func {
        params: Vec<ValueRttid>,
        results: Vec<ValueRttid>,
        variadic: bool,
    },
    
    /// Anonymous struct type
    /// `meta_id` is the struct_meta index for dynamic access.
    Struct {
        fields: Vec<StructField>,
        meta_id: u32,
    },
    
    /// Anonymous interface type
    /// `meta_id` is the interface_meta index for dynamic access.
    Interface {
        methods: Vec<InterfaceMethod>,
        meta_id: u32,
    },
    
    /// Tuple type (for function multi-value returns)
    Tuple(Vec<ValueRttid>),
    
    /// Port type: port[T] for cross-island communication
    Port(ValueRttid),
    
    /// Island type: represents a VM instance
    Island,
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
    /// Field name.
    pub name: String,
    /// Field type.
    pub typ: ValueRttid,
    /// Struct tag (empty string if no tag).
    pub tag: String,
    /// Whether this field is embedded.
    pub embedded: bool,
    /// Package path where the field is defined (for non-exported field identity).
    /// Empty string for exported fields.
    pub pkg: String,
}

/// A method in an anonymous interface type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InterfaceMethod {
    /// Method name.
    pub name: String,
    /// Method signature (must point to RuntimeType::Func).
    pub sig: ValueRttid,
}

impl RuntimeType {
    /// Returns true if this is a named type.
    #[inline]
    pub fn is_named(&self) -> bool {
        matches!(self, RuntimeType::Named { .. })
    }
    
    /// If this is a Named type, returns the named_type_id.
    #[inline]
    pub fn as_named(&self) -> Option<u32> {
        match self {
            RuntimeType::Named { id, .. } => Some(*id),
            _ => None,
        }
    }
    
    /// Get struct_meta_id for dynamic access.
    /// Works for Named types with struct underlying, and anonymous Struct types.
    #[inline]
    pub fn struct_meta_id(&self) -> Option<u32> {
        match self {
            RuntimeType::Named { struct_meta_id, .. } => *struct_meta_id,
            RuntimeType::Struct { meta_id, .. } => Some(*meta_id),
            _ => None,
        }
    }
    
    /// Get interface_meta_id for dynamic access.
    #[inline]
    pub fn interface_meta_id(&self) -> Option<u32> {
        match self {
            RuntimeType::Interface { meta_id, .. } => Some(*meta_id),
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
    pub fn new(name: String, typ: ValueRttid, tag: String, embedded: bool, pkg: String) -> Self {
        Self { name, typ, tag, embedded, pkg }
    }
}

impl InterfaceMethod {
    /// Creates a new interface method.
    pub fn new(name: String, sig: ValueRttid) -> Self {
        Self { name, sig }
    }
}

// Manual PartialEq/Eq/Hash implementation for RuntimeType
// Interface comparison uses method set equality (order-independent)
impl PartialEq for RuntimeType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Basic(a), Self::Basic(b)) => a == b,
            (Self::Named { id: a, .. }, Self::Named { id: b, .. }) => a == b,
            (Self::Pointer(a), Self::Pointer(b)) => a == b,
            (Self::Array { len: l1, elem: e1 }, Self::Array { len: l2, elem: e2 }) => l1 == l2 && e1 == e2,
            (Self::Slice(a), Self::Slice(b)) => a == b,
            (Self::Map { key: k1, val: v1 }, Self::Map { key: k2, val: v2 }) => k1 == k2 && v1 == v2,
            (Self::Chan { dir: d1, elem: e1 }, Self::Chan { dir: d2, elem: e2 }) => d1 == d2 && e1 == e2,
            (Self::Func { params: p1, results: r1, variadic: v1 }, Self::Func { params: p2, results: r2, variadic: v2 }) => {
                p1 == p2 && r1 == r2 && v1 == v2
            }
            (Self::Struct { fields: f1, .. }, Self::Struct { fields: f2, .. }) => f1 == f2,
            (Self::Interface { methods: m1, .. }, Self::Interface { methods: m2, .. }) => {
                // Interface equality: same method set (order-independent)
                if m1.len() != m2.len() {
                    return false;
                }
                // For each method in m1, find matching method in m2
                m1.iter().all(|method| m2.contains(method))
            }
            (Self::Tuple(a), Self::Tuple(b)) => a == b,
            (Self::Port(a), Self::Port(b)) => a == b,
            (Self::Island, Self::Island) => true,
            _ => false,
        }
    }
}

impl Eq for RuntimeType {}

/// Simple hasher for order-independent interface method hashing.
struct SimpleHasher(u64);

impl core::hash::Hasher for SimpleHasher {
    fn finish(&self) -> u64 { self.0 }
    fn write(&mut self, bytes: &[u8]) {
        for &b in bytes {
            self.0 = self.0.wrapping_mul(31).wrapping_add(b as u64);
        }
    }
}

impl core::hash::Hash for RuntimeType {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            Self::Basic(vk) => vk.hash(state),
            Self::Named { id, .. } => id.hash(state),
            Self::Pointer(elem) => elem.hash(state),
            Self::Array { len, elem } => { len.hash(state); elem.hash(state); }
            Self::Slice(elem) => elem.hash(state),
            Self::Map { key, val } => { key.hash(state); val.hash(state); }
            Self::Chan { dir, elem } => { dir.hash(state); elem.hash(state); }
            Self::Func { params, results, variadic } => {
                params.hash(state); results.hash(state); variadic.hash(state);
            }
            Self::Struct { fields, .. } => fields.hash(state),
            Self::Interface { methods, .. } => {
                // Hash method count and XOR of method hashes for order-independence
                methods.len().hash(state);
                let mut combined: u64 = 0;
                for m in methods {
                    let mut h = SimpleHasher(0);
                    core::hash::Hash::hash(m, &mut h);
                    combined ^= h.0;
                }
                combined.hash(state);
            }
            Self::Tuple(elems) => elems.hash(state),
            Self::Port(elem) => elem.hash(state),
            Self::Island => {}
        }
    }
}
