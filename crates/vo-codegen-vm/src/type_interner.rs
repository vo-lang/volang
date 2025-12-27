//! Type interning for runtime type identity.
//!
//! This module provides `TypeInterner`, which assigns unique runtime type IDs (rttid)
//! to structurally identical types. This enables O(1) type identity checks at runtime.

use std::collections::HashMap;
use vo_common_core::{RuntimeType, Symbol, ValueKind, ChanDir, StructField, InterfaceMethod};
use vo_analysis::arena::ArenaKey;
use vo_analysis::objects::TypeKey;
use vo_analysis::typ::{Type, BasicType};

/// A type interner that assigns unique runtime type IDs to types.
///
/// Structurally identical types receive the same rttid, enabling
/// fast type identity checks at runtime.
#[derive(Debug)]
pub struct TypeInterner {
    cache: HashMap<RuntimeType, u32>,
    types: Vec<RuntimeType>,
}

impl TypeInterner {
    /// Creates a new empty type interner.
    pub fn new() -> Self {
        Self {
            cache: HashMap::new(),
            types: Vec::new(),
        }
    }

    /// Interns a runtime type, returning its rttid.
    ///
    /// If the type was already interned, returns the existing rttid.
    pub fn intern(&mut self, rt: RuntimeType) -> u32 {
        if let Some(&id) = self.cache.get(&rt) {
            return id;
        }
        let id = self.types.len() as u32;
        self.cache.insert(rt.clone(), id);
        self.types.push(rt);
        id
    }

    /// Returns the number of interned types.
    pub fn len(&self) -> usize {
        self.types.len()
    }

    /// Returns true if no types have been interned.
    pub fn is_empty(&self) -> bool {
        self.types.is_empty()
    }

    /// Consumes the interner and returns the vector of runtime types.
    pub fn into_vec(self) -> Vec<RuntimeType> {
        self.types
    }

    /// Returns a reference to the interned types.
    pub fn types(&self) -> &[RuntimeType] {
        &self.types
    }
}

impl Default for TypeInterner {
    fn default() -> Self {
        Self::new()
    }
}

/// Converts a type-checked TypeKey to a RuntimeType.
///
/// named_type_ids maps TypeKey -> named_type_id for Named types.
pub fn type_key_to_runtime_type(
    type_key: TypeKey,
    tc_objs: &vo_analysis::objects::TCObjects,
    str_interner: &vo_common::SymbolInterner,
    named_type_ids: &std::collections::HashMap<TypeKey, u32>,
) -> RuntimeType {
    match &tc_objs.types[type_key] {
        Type::Basic(basic) => {
            RuntimeType::Basic(basic_type_to_value_kind(basic.typ()))
        }
        
        Type::Named(named) => {
            // Try direct lookup first
            if let Some(&id) = named_type_ids.get(&type_key) {
                RuntimeType::Named(id)
            } else {
                // Fallback: find by ObjKey (same Named type may have different TypeKey)
                let id = named_type_ids.iter()
                    .find(|(&k, _)| {
                        if let Type::Named(n) = &tc_objs.types[k] {
                            n.obj() == named.obj()
                        } else {
                            false
                        }
                    })
                    .map(|(_, &id)| id)
                    .expect("Named type must have a named_type_id");
                RuntimeType::Named(id)
            }
        }
        
        Type::Pointer(ptr) => {
            let base = type_key_to_runtime_type(ptr.base(), tc_objs, str_interner, named_type_ids);
            RuntimeType::Pointer(Box::new(base))
        }
        
        Type::Array(arr) => {
            let elem = type_key_to_runtime_type(arr.elem(), tc_objs, str_interner, named_type_ids);
            RuntimeType::Array {
                len: arr.len().unwrap_or(0),
                elem: Box::new(elem),
            }
        }
        
        Type::Slice(slice) => {
            let elem = type_key_to_runtime_type(slice.elem(), tc_objs, str_interner, named_type_ids);
            RuntimeType::Slice(Box::new(elem))
        }
        
        Type::Map(map) => {
            let key = type_key_to_runtime_type(map.key(), tc_objs, str_interner, named_type_ids);
            let val = type_key_to_runtime_type(map.elem(), tc_objs, str_interner, named_type_ids);
            RuntimeType::Map {
                key: Box::new(key),
                val: Box::new(val),
            }
        }
        
        Type::Chan(chan) => {
            let elem = type_key_to_runtime_type(chan.elem(), tc_objs, str_interner, named_type_ids);
            let dir = match chan.dir() {
                vo_analysis::typ::ChanDir::SendRecv => ChanDir::Both,
                vo_analysis::typ::ChanDir::SendOnly => ChanDir::Send,
                vo_analysis::typ::ChanDir::RecvOnly => ChanDir::Recv,
            };
            RuntimeType::Chan {
                dir,
                elem: Box::new(elem),
            }
        }
        
        Type::Signature(sig) => {
            // params() and results() return TypeKey pointing to Tuple types
            let params_tuple = &tc_objs.types[sig.params()];
            let params: Vec<RuntimeType> = if let Type::Tuple(tuple) = params_tuple {
                tuple.vars().iter()
                    .filter_map(|&p| {
                        let obj = &tc_objs.lobjs[p];
                        obj.typ().map(|t| type_key_to_runtime_type(t, tc_objs, str_interner, named_type_ids))
                    })
                    .collect()
            } else {
                Vec::new()
            };
            
            let results_tuple = &tc_objs.types[sig.results()];
            let results: Vec<RuntimeType> = if let Type::Tuple(tuple) = results_tuple {
                tuple.vars().iter()
                    .filter_map(|&r| {
                        let obj = &tc_objs.lobjs[r];
                        obj.typ().map(|t| type_key_to_runtime_type(t, tc_objs, str_interner, named_type_ids))
                    })
                    .collect()
            } else {
                Vec::new()
            };
            
            RuntimeType::Func {
                params,
                results,
                variadic: sig.variadic(),
            }
        }
        
        Type::Struct(s) => {
            let fields: Vec<StructField> = s.fields().iter()
                .map(|&f| {
                    let obj = &tc_objs.lobjs[f];
                    let name = Symbol::from_raw(
                        str_interner.get(obj.name()).map(|s| s.as_u32()).unwrap_or(u32::MAX)
                    );
                    let typ = obj.typ()
                        .map(|t| type_key_to_runtime_type(t, tc_objs, str_interner, named_type_ids))
                        .unwrap_or(RuntimeType::Basic(ValueKind::Void));
                    
                    // Get tag from field - TODO: extract tag if available
                    let tag = Symbol::DUMMY;
                    
                    let embedded = obj.entity_type().var_property().embedded;
                    
                    // Get package for non-exported fields
                    let pkg = if obj.exported() {
                        Symbol::DUMMY
                    } else {
                        obj.pkg()
                            .and_then(|p| tc_objs.pkgs.get(p))
                            .and_then(|pkg| pkg.name().as_ref())
                            .and_then(|name| str_interner.get(name))
                            .map(|s| Symbol::from_raw(s.as_u32()))
                            .unwrap_or(Symbol::DUMMY)
                    };
                    
                    StructField::new(name, typ, tag, embedded, pkg)
                })
                .collect();
            
            RuntimeType::Struct { fields }
        }
        
        Type::Interface(iface) => {
            let methods: Vec<InterfaceMethod> = {
                let all_methods = iface.all_methods();
                let method_keys: &[vo_analysis::objects::ObjKey] = all_methods.as_ref()
                    .map(|v| v.as_slice())
                    .unwrap_or_else(|| iface.methods());
                
                method_keys.iter()
                    .map(|&m| {
                        let obj = &tc_objs.lobjs[m];
                        let name = Symbol::from_raw(
                            str_interner.get(obj.name()).map(|s| s.as_u32()).unwrap_or(u32::MAX)
                        );
                        let sig = obj.typ()
                            .map(|t| type_key_to_runtime_type(t, tc_objs, str_interner, named_type_ids))
                            .unwrap_or(RuntimeType::Func {
                                params: Vec::new(),
                                results: Vec::new(),
                                variadic: false,
                            });
                        
                        InterfaceMethod::new(name, sig)
                    })
                    .collect()
            };
            
            RuntimeType::Interface { methods }
        }
        
        Type::Tuple(tuple) => {
            let elems: Vec<RuntimeType> = tuple.vars().iter()
                .filter_map(|&v| {
                    let obj = &tc_objs.lobjs[v];
                    obj.typ().map(|t| type_key_to_runtime_type(t, tc_objs, str_interner, named_type_ids))
                })
                .collect();
            RuntimeType::Tuple(elems)
        }
    }
}

/// Converts a BasicType to ValueKind.
fn basic_type_to_value_kind(typ: BasicType) -> ValueKind {
    match typ {
        BasicType::Bool | BasicType::UntypedBool => ValueKind::Bool,
        BasicType::Int | BasicType::UntypedInt => ValueKind::Int,
        BasicType::Int8 => ValueKind::Int8,
        BasicType::Int16 => ValueKind::Int16,
        BasicType::Int32 | BasicType::UntypedRune | BasicType::Rune => ValueKind::Int32,
        BasicType::Int64 => ValueKind::Int64,
        BasicType::Uint => ValueKind::Uint,
        BasicType::Uint8 | BasicType::Byte => ValueKind::Uint8,
        BasicType::Uint16 => ValueKind::Uint16,
        BasicType::Uint32 => ValueKind::Uint32,
        BasicType::Uint64 => ValueKind::Uint64,
        BasicType::Uintptr => ValueKind::Uint64, // Map uintptr to uint64
        BasicType::Float32 => ValueKind::Float32,
        BasicType::Float64 | BasicType::UntypedFloat => ValueKind::Float64,
        BasicType::Str | BasicType::UntypedString => ValueKind::String,
        BasicType::UntypedNil => ValueKind::Void,
        BasicType::Invalid => ValueKind::Void,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_interner_basic() {
        let mut interner = TypeInterner::new();
        
        let rt1 = RuntimeType::Basic(ValueKind::Int);
        let rt2 = RuntimeType::Basic(ValueKind::String);
        let rt3 = RuntimeType::Basic(ValueKind::Int);
        
        let id1 = interner.intern(rt1);
        let id2 = interner.intern(rt2);
        let id3 = interner.intern(rt3);
        
        assert_eq!(id1, id3); // Same type, same id
        assert_ne!(id1, id2); // Different types, different ids
        assert_eq!(interner.len(), 2);
    }

    #[test]
    fn test_interner_composite() {
        let mut interner = TypeInterner::new();
        
        let slice_int = RuntimeType::Slice(Box::new(RuntimeType::Basic(ValueKind::Int)));
        let slice_int2 = RuntimeType::Slice(Box::new(RuntimeType::Basic(ValueKind::Int)));
        let slice_str = RuntimeType::Slice(Box::new(RuntimeType::Basic(ValueKind::String)));
        
        let id1 = interner.intern(slice_int);
        let id2 = interner.intern(slice_int2);
        let id3 = interner.intern(slice_str);
        
        assert_eq!(id1, id2);
        assert_ne!(id1, id3);
    }
}
