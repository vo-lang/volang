//! Type interning for runtime type identity.
//!
//! This module provides `TypeInterner`, which assigns unique runtime type IDs (rttid)
//! to structurally identical types. This enables O(1) type identity checks at runtime.

use std::collections::HashMap;
use vo_runtime::{RuntimeType, Symbol, ValueKind, ChanDir, StructField, InterfaceMethod};
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
    /// Creates a new type interner with all Basic types pre-registered.
    /// rttid for Basic types = ValueKind value.
    pub fn new() -> Self {
        let mut interner = Self {
            cache: HashMap::new(),
            types: Vec::new(),
        };
        // Pre-register basic types (no internal type info) so rttid matches ValueKind value
        for &vk in &ValueKind::BASIC {
            let rt = RuntimeType::Basic(vk);
            let id = vk as u32;
            interner.cache.insert(rt.clone(), id);
            // Ensure types vec is large enough
            while interner.types.len() <= id as usize {
                interner.types.push(RuntimeType::Basic(ValueKind::Void));
            }
            interner.types[id as usize] = rt;
        }
        interner
    }
    
    /// Interns a runtime type, returning its rttid.
    ///
    /// Basic types are pre-registered, so this just returns the cached id.
    /// User-defined types get new ids starting from 24.
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

/// Converts a type-checked TypeKey to a RuntimeType and interns it, returning rttid.
/// For composite types (Pointer/Array/Slice/Map/Chan), this first interns inner types
/// to get their rttids, then constructs the outer type with those rttids.
///
/// named_type_ids maps TypeKey -> named_type_id for Named types.
pub fn intern_type_key(
    interner: &mut TypeInterner,
    type_key: TypeKey,
    tc_objs: &vo_analysis::objects::TCObjects,
    str_interner: &vo_common::SymbolInterner,
    named_type_ids: &std::collections::HashMap<TypeKey, u32>,
) -> u32 {
    let rt = match &tc_objs.types[type_key] {
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
            let elem_rttid = intern_type_key(interner, ptr.base(), tc_objs, str_interner, named_type_ids);
            RuntimeType::Pointer(elem_rttid)
        }
        
        Type::Array(arr) => {
            let elem_rttid = intern_type_key(interner, arr.elem(), tc_objs, str_interner, named_type_ids);
            RuntimeType::Array {
                len: arr.len().unwrap_or(0),
                elem: elem_rttid,
            }
        }
        
        Type::Slice(slice) => {
            let elem_rttid = intern_type_key(interner, slice.elem(), tc_objs, str_interner, named_type_ids);
            RuntimeType::Slice(elem_rttid)
        }
        
        Type::Map(map) => {
            let key_rttid = intern_type_key(interner, map.key(), tc_objs, str_interner, named_type_ids);
            let val_rttid = intern_type_key(interner, map.elem(), tc_objs, str_interner, named_type_ids);
            RuntimeType::Map {
                key: key_rttid,
                val: val_rttid,
            }
        }
        
        Type::Chan(chan) => {
            let elem_rttid = intern_type_key(interner, chan.elem(), tc_objs, str_interner, named_type_ids);
            let dir = match chan.dir() {
                vo_analysis::typ::ChanDir::SendRecv => ChanDir::Both,
                vo_analysis::typ::ChanDir::SendOnly => ChanDir::Send,
                vo_analysis::typ::ChanDir::RecvOnly => ChanDir::Recv,
            };
            RuntimeType::Chan {
                dir,
                elem: elem_rttid,
            }
        }
        
        Type::Signature(sig) => {
            // params() and results() return TypeKey pointing to Tuple types
            // Recursively intern each param/result type to get rttid
            let params_tuple = &tc_objs.types[sig.params()];
            let params: Vec<u32> = if let Type::Tuple(tuple) = params_tuple {
                tuple.vars().iter()
                    .filter_map(|&p| {
                        let obj = &tc_objs.lobjs[p];
                        obj.typ().map(|t| {
                            let rt = type_key_to_runtime_type(interner, t, tc_objs, str_interner, named_type_ids);
                            interner.intern(rt)
                        })
                    })
                    .collect()
            } else {
                Vec::new()
            };
            
            let results_tuple = &tc_objs.types[sig.results()];
            let results: Vec<u32> = if let Type::Tuple(tuple) = results_tuple {
                tuple.vars().iter()
                    .filter_map(|&r| {
                        let obj = &tc_objs.lobjs[r];
                        obj.typ().map(|t| {
                            let rt = type_key_to_runtime_type(interner, t, tc_objs, str_interner, named_type_ids);
                            interner.intern(rt)
                        })
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
                    let typ_rttid = obj.typ()
                        .map(|t| {
                            let rt = type_key_to_runtime_type(interner, t, tc_objs, str_interner, named_type_ids);
                            interner.intern(rt)
                        })
                        .unwrap_or(ValueKind::Void as u32);
                    
                    let tag = Symbol::DUMMY;
                    let embedded = obj.entity_type().var_property().embedded;
                    
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
                    
                    StructField::new(name, typ_rttid, tag, embedded, pkg)
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
                        let sig_rttid = obj.typ()
                            .map(|t| {
                                let rt = type_key_to_runtime_type(interner, t, tc_objs, str_interner, named_type_ids);
                                interner.intern(rt)
                            })
                            .unwrap_or_else(|| {
                                interner.intern(RuntimeType::Func { params: Vec::new(), results: Vec::new(), variadic: false })
                            });
                        
                        InterfaceMethod::new(name, sig_rttid)
                    })
                    .collect()
            };
            
            RuntimeType::Interface { methods }
        }
        
        Type::Tuple(tuple) => {
            let elems: Vec<u32> = tuple.vars().iter()
                .filter_map(|&v| {
                    let obj = &tc_objs.lobjs[v];
                    obj.typ().map(|t| {
                        let rt = type_key_to_runtime_type(interner, t, tc_objs, str_interner, named_type_ids);
                        interner.intern(rt)
                    })
                })
                .collect();
            RuntimeType::Tuple(elems)
        }
    };
    
    interner.intern(rt)
}

/// Helper: converts a type_key to RuntimeType (for nested types in Func/Struct/Interface).
/// This doesn't intern, just returns the RuntimeType.
fn type_key_to_runtime_type(
    interner: &mut TypeInterner,
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
            if let Some(&id) = named_type_ids.get(&type_key) {
                RuntimeType::Named(id)
            } else {
                let id = named_type_ids.iter()
                    .find(|(&k, _)| {
                        if let Type::Named(n) = &tc_objs.types[k] {
                            n.obj() == named.obj()
                        } else {
                            false
                        }
                    })
                    .map(|(_, &id)| id)
                    .unwrap_or(0);
                RuntimeType::Named(id)
            }
        }
        Type::Pointer(ptr) => {
            let elem_rttid = intern_type_key(interner, ptr.base(), tc_objs, str_interner, named_type_ids);
            RuntimeType::Pointer(elem_rttid)
        }
        Type::Array(arr) => {
            let elem_rttid = intern_type_key(interner, arr.elem(), tc_objs, str_interner, named_type_ids);
            RuntimeType::Array { len: arr.len().unwrap_or(0), elem: elem_rttid }
        }
        Type::Slice(slice) => {
            let elem_rttid = intern_type_key(interner, slice.elem(), tc_objs, str_interner, named_type_ids);
            RuntimeType::Slice(elem_rttid)
        }
        Type::Map(map) => {
            let key_rttid = intern_type_key(interner, map.key(), tc_objs, str_interner, named_type_ids);
            let val_rttid = intern_type_key(interner, map.elem(), tc_objs, str_interner, named_type_ids);
            RuntimeType::Map { key: key_rttid, val: val_rttid }
        }
        Type::Chan(chan) => {
            let elem_rttid = intern_type_key(interner, chan.elem(), tc_objs, str_interner, named_type_ids);
            let dir = match chan.dir() {
                vo_analysis::typ::ChanDir::SendRecv => ChanDir::Both,
                vo_analysis::typ::ChanDir::SendOnly => ChanDir::Send,
                vo_analysis::typ::ChanDir::RecvOnly => ChanDir::Recv,
            };
            RuntimeType::Chan { dir, elem: elem_rttid }
        }
        Type::Signature(sig) => {
            let params_tuple = &tc_objs.types[sig.params()];
            let params: Vec<u32> = if let Type::Tuple(tuple) = params_tuple {
                tuple.vars().iter()
                    .filter_map(|&p| {
                        let obj = &tc_objs.lobjs[p];
                        obj.typ().map(|t| {
                            let rt = type_key_to_runtime_type(interner, t, tc_objs, str_interner, named_type_ids);
                            interner.intern(rt)
                        })
                    })
                    .collect()
            } else {
                Vec::new()
            };
            let results_tuple = &tc_objs.types[sig.results()];
            let results: Vec<u32> = if let Type::Tuple(tuple) = results_tuple {
                tuple.vars().iter()
                    .filter_map(|&r| {
                        let obj = &tc_objs.lobjs[r];
                        obj.typ().map(|t| {
                            let rt = type_key_to_runtime_type(interner, t, tc_objs, str_interner, named_type_ids);
                            interner.intern(rt)
                        })
                    })
                    .collect()
            } else {
                Vec::new()
            };
            RuntimeType::Func { params, results, variadic: sig.variadic() }
        }
        Type::Struct(s) => {
            let fields: Vec<StructField> = s.fields().iter()
                .map(|&f| {
                    let obj = &tc_objs.lobjs[f];
                    let name = Symbol::from_raw(
                        str_interner.get(obj.name()).map(|s| s.as_u32()).unwrap_or(u32::MAX)
                    );
                    let typ_rttid = obj.typ()
                        .map(|t| {
                            let rt = type_key_to_runtime_type(interner, t, tc_objs, str_interner, named_type_ids);
                            interner.intern(rt)
                        })
                        .unwrap_or(ValueKind::Void as u32);
                    let tag = Symbol::DUMMY;
                    let embedded = obj.entity_type().var_property().embedded;
                    let pkg = if obj.exported() { Symbol::DUMMY } else {
                        obj.pkg()
                            .and_then(|p| tc_objs.pkgs.get(p))
                            .and_then(|pkg| pkg.name().as_ref())
                            .and_then(|name| str_interner.get(name))
                            .map(|s| Symbol::from_raw(s.as_u32()))
                            .unwrap_or(Symbol::DUMMY)
                    };
                    StructField::new(name, typ_rttid, tag, embedded, pkg)
                })
                .collect();
            RuntimeType::Struct { fields }
        }
        Type::Interface(iface) => {
            let all_methods = iface.all_methods();
            let method_keys: &[vo_analysis::objects::ObjKey] = all_methods.as_ref()
                .map(|v| v.as_slice())
                .unwrap_or_else(|| iface.methods());
            let methods: Vec<InterfaceMethod> = method_keys.iter()
                .map(|&m| {
                    let obj = &tc_objs.lobjs[m];
                    let name = Symbol::from_raw(
                        str_interner.get(obj.name()).map(|s| s.as_u32()).unwrap_or(u32::MAX)
                    );
                    let sig_rttid = obj.typ()
                        .map(|t| {
                            let rt = type_key_to_runtime_type(interner, t, tc_objs, str_interner, named_type_ids);
                            interner.intern(rt)
                        })
                        .unwrap_or_else(|| {
                            interner.intern(RuntimeType::Func { params: Vec::new(), results: Vec::new(), variadic: false })
                        });
                    InterfaceMethod::new(name, sig_rttid)
                })
                .collect();
            RuntimeType::Interface { methods }
        }
        Type::Tuple(tuple) => {
            let elems: Vec<u32> = tuple.vars().iter()
                .filter_map(|&v| {
                    let obj = &tc_objs.lobjs[v];
                    obj.typ().map(|t| {
                        let rt = type_key_to_runtime_type(interner, t, tc_objs, str_interner, named_type_ids);
                        interner.intern(rt)
                    })
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
        
        // Basic types are pre-registered, rttid = ValueKind value
        assert_eq!(id1, ValueKind::Int as u32);
        assert_eq!(id2, ValueKind::String as u32);
        assert_eq!(id1, id3); // Same type, same id
        assert_ne!(id1, id2); // Different types, different ids
    }

    #[test]
    fn test_interner_composite() {
        let mut interner = TypeInterner::new();
        
        // Slice stores elem rttid (u32), basic type rttid = ValueKind value
        let int_rttid = ValueKind::Int as u32;
        let string_rttid = ValueKind::String as u32;
        
        let slice_int = RuntimeType::Slice(int_rttid);
        let slice_int2 = RuntimeType::Slice(int_rttid);
        let slice_str = RuntimeType::Slice(string_rttid);
        
        let id1 = interner.intern(slice_int);
        let id2 = interner.intern(slice_int2);
        let id3 = interner.intern(slice_str);
        
        assert_eq!(id1, id2);
        assert_ne!(id1, id3);
    }
}
