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
/// Composite types (Pointer/Array/Slice/Map/Chan) store elem rttid (u32)
/// instead of Box<RuntimeType> to enable O(1) elem type lookup at runtime.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    
    /// Function type: func(params) results
    Func {
        params: Vec<RuntimeType>,
        results: Vec<RuntimeType>,
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
    
    /// Tuple type (for function multi-value returns)
    Tuple(Vec<RuntimeType>),
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
    /// Field type.
    pub typ: Box<RuntimeType>,
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
    /// Method signature (must be RuntimeType::Func).
    pub sig: Box<RuntimeType>,
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
    pub fn new(name: Symbol, typ: RuntimeType, tag: Symbol, embedded: bool, pkg: Symbol) -> Self {
        Self {
            name,
            typ: Box::new(typ),
            tag,
            embedded,
            pkg,
        }
    }
    
    /// Returns a mutable reference to the field type.
    #[inline]
    pub fn typ_mut(&mut self) -> &mut RuntimeType {
        &mut self.typ
    }
}

impl InterfaceMethod {
    /// Creates a new interface method.
    pub fn new(name: Symbol, sig: RuntimeType) -> Self {
        Self {
            name,
            sig: Box::new(sig),
        }
    }
    
    /// Returns a mutable reference to the method signature.
    #[inline]
    pub fn sig_mut(&mut self) -> &mut RuntimeType {
        &mut self.sig
    }
    
    /// Check if a concrete method signature matches this interface method.
    /// Returns true if the concrete method can implement this interface method.
    pub fn matches_signature(&self, concrete_sig: &RuntimeType) -> bool {
        // Both must be Func types
        match (&*self.sig, concrete_sig) {
            (RuntimeType::Func { params: iface_params, results: iface_results, variadic: iface_variadic },
             RuntimeType::Func { params: concrete_params, results: concrete_results, variadic: concrete_variadic }) => {
                // Variadic must match
                if iface_variadic != concrete_variadic {
                    return false;
                }
                // Parameter count must match
                if iface_params.len() != concrete_params.len() {
                    return false;
                }
                // Result count must match
                if iface_results.len() != concrete_results.len() {
                    return false;
                }
                // All parameter types must be identical
                for (iface_p, concrete_p) in iface_params.iter().zip(concrete_params.iter()) {
                    if iface_p != concrete_p {
                        return false;
                    }
                }
                // All result types must be identical
                for (iface_r, concrete_r) in iface_results.iter().zip(concrete_results.iter()) {
                    if iface_r != concrete_r {
                        return false;
                    }
                }
                true
            }
            _ => false,
        }
    }
}
