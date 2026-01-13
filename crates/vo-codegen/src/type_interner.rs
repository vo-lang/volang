//! Type interning for runtime type identity.
//!
//! This module provides `TypeInterner`, which assigns unique runtime type IDs (rttid)
//! to structurally identical types. This enables O(1) type identity checks at runtime.

use std::collections::HashMap;
use vo_runtime::{RuntimeType, ValueKind, ValueRttid, ChanDir, StructField, InterfaceMethod};
use vo_analysis::objects::{TypeKey, ObjKey};
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
    
    /// Returns a mutable reference to the interned types.
    /// Used in finalize phase to fill meta_id fields.
    pub fn types_mut(&mut self) -> &mut [RuntimeType] {
        &mut self.types
    }
}

impl Default for TypeInterner {
    fn default() -> Self {
        Self::new()
    }
}

/// Additional metadata for type interning.
/// Uses mutable references to allow dynamic registration of named types and anonymous structs.
pub struct InternContext<'a> {
    pub named_type_ids: &'a mut std::collections::HashMap<ObjKey, u32>,
    pub named_type_metas: &'a mut Vec<vo_common_core::bytecode::NamedTypeMeta>,
    pub struct_meta_ids: &'a mut std::collections::HashMap<vo_analysis::objects::TypeKey, u32>,
    pub struct_metas: &'a mut Vec<vo_common_core::bytecode::StructMeta>,
    pub interface_meta_ids: &'a std::collections::HashMap<vo_analysis::objects::TypeKey, u32>,
}

/// Converts a type-checked TypeKey to a RuntimeType and interns it, returning ValueRttid.
/// For composite types (Pointer/Array/Slice/Map/Chan), this first interns inner types
/// to get their ValueRttids, then constructs the outer type with those ValueRttids.
pub fn intern_type_key(
    interner: &mut TypeInterner,
    type_key: TypeKey,
    tc_objs: &vo_analysis::objects::TCObjects,
    str_interner: &vo_common::SymbolInterner,
    ctx: &mut InternContext,
) -> ValueRttid {
    let (rt, vk) = type_key_to_runtime_type(interner, type_key, tc_objs, str_interner, ctx);
    let rttid = interner.intern(rt);
    ValueRttid::new(rttid, vk)
}

/// Helper: converts a type_key to (RuntimeType, ValueKind).
/// This doesn't intern, just returns the RuntimeType and its ValueKind.
fn type_key_to_runtime_type(
    interner: &mut TypeInterner,
    type_key: TypeKey,
    tc_objs: &vo_analysis::objects::TCObjects,
    str_interner: &vo_common::SymbolInterner,
    ctx: &mut InternContext,
) -> (RuntimeType, ValueKind) {
    match &tc_objs.types[type_key] {
        Type::Basic(basic) => {
            let vk = basic_type_to_value_kind(basic.typ());
            (RuntimeType::Basic(vk), vk)
        }
        Type::Named(named) => {
            // Named types are nominal for identity, but their ValueKind is determined by
            // the underlying type category.
            //
            // IMPORTANT: do NOT recursively convert the full underlying RuntimeType here.
            // For self-referential named interface types (e.g. builtin error with Unwrap() error),
            // recursing into interface method signatures would cause infinite recursion.
            let underlying = vo_analysis::typ::underlying_type(type_key, tc_objs);
            let vk = match &tc_objs.types[underlying] {
                Type::Basic(b) => basic_type_to_value_kind(b.typ()),
                Type::Pointer(_) => ValueKind::Pointer,
                Type::Array(_) => ValueKind::Array,
                Type::Slice(_) => ValueKind::Slice,
                Type::Map(_) => ValueKind::Map,
                Type::Chan(_) => ValueKind::Channel,
                Type::Signature(_) => ValueKind::Closure,
                Type::Struct(_) => ValueKind::Struct,
                Type::Interface(_) => ValueKind::Interface,
                Type::Tuple(_) => ValueKind::Void,
                Type::Named(_) => {
                    panic!("type_interner: unexpected Named underlying for a Named type")
                }
            };
            // Use ObjKey for lookup - dynamically register if not found
            let id = if let Some(&obj_key) = named.obj().as_ref() {
                *ctx.named_type_ids.entry(obj_key).or_insert_with(|| {
                    let id = ctx.named_type_metas.len() as u32;
                    // Push placeholder - will be filled in later by register_named_type_meta
                    ctx.named_type_metas.push(vo_common_core::bytecode::NamedTypeMeta {
                        name: String::new(),
                        underlying_meta: vo_runtime::ValueMeta::new(0, vo_runtime::ValueKind::Void),
                        methods: std::collections::HashMap::new(),
                    });
                    id
                })
            } else {
                0
            };
            // Get struct_meta_id from named_type's underlying type
            let struct_meta_id = ctx.struct_meta_ids.get(&type_key).copied();
            (RuntimeType::Named { id, struct_meta_id }, vk)
        }
        Type::Pointer(ptr) => {
            let elem_value_rttid = intern_type_key(interner, ptr.base(), tc_objs, str_interner, ctx);
            (RuntimeType::Pointer(elem_value_rttid), ValueKind::Pointer)
        }
        Type::Array(arr) => {
            let elem_value_rttid = intern_type_key(interner, arr.elem(), tc_objs, str_interner, ctx);
            (RuntimeType::Array { len: arr.len().unwrap_or(0), elem: elem_value_rttid }, ValueKind::Array)
        }
        Type::Slice(slice) => {
            let elem_value_rttid = intern_type_key(interner, slice.elem(), tc_objs, str_interner, ctx);
            (RuntimeType::Slice(elem_value_rttid), ValueKind::Slice)
        }
        Type::Map(map) => {
            let key_value_rttid = intern_type_key(interner, map.key(), tc_objs, str_interner, ctx);
            let val_value_rttid = intern_type_key(interner, map.elem(), tc_objs, str_interner, ctx);
            (RuntimeType::Map { key: key_value_rttid, val: val_value_rttid }, ValueKind::Map)
        }
        Type::Chan(chan) => {
            let elem_value_rttid = intern_type_key(interner, chan.elem(), tc_objs, str_interner, ctx);
            let dir = match chan.dir() {
                vo_analysis::typ::ChanDir::SendRecv => ChanDir::Both,
                vo_analysis::typ::ChanDir::SendOnly => ChanDir::Send,
                vo_analysis::typ::ChanDir::RecvOnly => ChanDir::Recv,
            };
            (RuntimeType::Chan { dir, elem: elem_value_rttid }, ValueKind::Channel)
        }
        Type::Signature(sig) => {
            let params_tuple = &tc_objs.types[sig.params()];
            let params: Vec<ValueRttid> = if let Type::Tuple(tuple) = params_tuple {
                tuple.vars().iter()
                    .filter_map(|&p| {
                        let obj = &tc_objs.lobjs[p];
                        obj.typ().map(|t| {
                            let (rt, vk) = type_key_to_runtime_type(interner, t, tc_objs, str_interner, ctx);
                            let rttid = interner.intern(rt);
                            ValueRttid::new(rttid, vk)
                        })
                    })
                    .collect()
            } else {
                Vec::new()
            };
            let results_tuple = &tc_objs.types[sig.results()];
            let results: Vec<ValueRttid> = if let Type::Tuple(tuple) = results_tuple {
                tuple.vars().iter()
                    .filter_map(|&r| {
                        let obj = &tc_objs.lobjs[r];
                        obj.typ().map(|t| {
                            let (rt, vk) = type_key_to_runtime_type(interner, t, tc_objs, str_interner, ctx);
                            let rttid = interner.intern(rt);
                            ValueRttid::new(rttid, vk)
                        })
                    })
                    .collect()
            } else {
                Vec::new()
            };
            (RuntimeType::Func { params, results, variadic: sig.variadic() }, ValueKind::Closure)
        }
        Type::Struct(s) => {
            // Check if already registered (named struct)
            let existing_meta_id = ctx.struct_meta_ids.get(&type_key).copied();
            let needs_registration = existing_meta_id.is_none();
            
            // Single pass: build RuntimeType fields and StructMeta simultaneously
            let mut rt_fields = Vec::with_capacity(s.fields().len());
            let mut field_metas = if needs_registration { Vec::with_capacity(s.fields().len()) } else { Vec::new() };
            let mut slot_types = if needs_registration { Vec::new() } else { Vec::new() };
            let mut offset = 0u16;
            
            for &f in s.fields() {
                let obj = &tc_objs.lobjs[f];
                let name = obj.name().to_string();
                let embedded = obj.entity_type().var_property().embedded;
                let pkg = if obj.exported() { String::new() } else {
                    obj.pkg()
                        .and_then(|p| tc_objs.pkgs.get(p))
                        .map(|pkg| pkg.path().to_string())
                        .unwrap_or_default()
                };
                
                let (typ_value_rttid, field_slot_count) = if let Some(field_type) = obj.typ() {
                    let (rt, vk) = type_key_to_runtime_type(interner, field_type, tc_objs, str_interner, ctx);
                    let rttid = interner.intern(rt);
                    let slot_count = vo_analysis::check::type_info::type_slot_count(field_type, tc_objs);
                    
                    if needs_registration {
                        let field_slot_types = vo_analysis::check::type_info::type_slot_types(field_type, tc_objs);
                        slot_types.extend(field_slot_types);
                    }
                    
                    (ValueRttid::new(rttid, vk), slot_count)
                } else {
                    (ValueRttid::new(ValueKind::Void as u32, ValueKind::Void), 1)
                };
                
                rt_fields.push(StructField::new(name.clone(), typ_value_rttid, String::new(), embedded, pkg));
                
                if needs_registration {
                    field_metas.push(vo_common_core::bytecode::FieldMeta {
                        name,
                        offset,
                        slot_count: field_slot_count,
                        type_info: typ_value_rttid,
                    });
                    offset += field_slot_count;
                }
            }
            
            let meta_id = if let Some(id) = existing_meta_id {
                id
            } else {
                // Register anonymous struct
                if slot_types.is_empty() {
                    slot_types.push(vo_runtime::SlotType::Value);
                }
                let field_index: HashMap<String, usize> = field_metas.iter()
                    .enumerate()
                    .map(|(i, f)| (f.name.clone(), i))
                    .collect();
                let meta = vo_common_core::bytecode::StructMeta { slot_types, fields: field_metas, field_index };
                let id = ctx.struct_metas.len() as u32;
                ctx.struct_metas.push(meta);
                ctx.struct_meta_ids.insert(type_key, id);
                id
            };
            
            (RuntimeType::Struct { fields: rt_fields, meta_id }, ValueKind::Struct)
        }
        Type::Interface(iface) => {
            let all_methods = iface.all_methods();
            let method_keys: &[vo_analysis::objects::ObjKey] = all_methods.as_ref()
                .map(|v| v.as_slice())
                .unwrap_or_else(|| iface.methods());
            let methods: Vec<InterfaceMethod> = method_keys.iter()
                .map(|&m| {
                    let obj = &tc_objs.lobjs[m];
                    let name = obj.name().to_string();
                    let sig_value_rttid = obj.typ()
                        .map(|t| {
                            let (rt, _vk) = type_key_to_runtime_type(interner, t, tc_objs, str_interner, ctx);
                            let rttid = interner.intern(rt);
                            ValueRttid::new(rttid, ValueKind::Closure)
                        })
                        .unwrap_or_else(|| {
                            let rttid = interner.intern(RuntimeType::Func { params: Vec::new(), results: Vec::new(), variadic: false });
                            ValueRttid::new(rttid, ValueKind::Closure)
                        });
                    InterfaceMethod::new(name, sig_value_rttid)
                })
                .collect();
            // Get meta_id from interface_meta_ids mapping
            let meta_id = ctx.interface_meta_ids.get(&type_key).copied().unwrap_or(0);
            (RuntimeType::Interface { methods, meta_id }, ValueKind::Interface)
        }
        Type::Tuple(tuple) => {
            let elems: Vec<ValueRttid> = tuple.vars().iter()
                .filter_map(|&v| {
                    let obj = &tc_objs.lobjs[v];
                    obj.typ().map(|t| {
                        let (rt, vk) = type_key_to_runtime_type(interner, t, tc_objs, str_interner, ctx);
                        let rttid = interner.intern(rt);
                        ValueRttid::new(rttid, vk)
                    })
                })
                .collect();
            (RuntimeType::Tuple(elems), ValueKind::Void)
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
        
        // Slice stores elem ValueRttid, basic type rttid = ValueKind value
        let int_value_rttid = ValueRttid::new(ValueKind::Int as u32, ValueKind::Int);
        let string_value_rttid = ValueRttid::new(ValueKind::String as u32, ValueKind::String);
        
        let slice_int = RuntimeType::Slice(int_value_rttid);
        let slice_int2 = RuntimeType::Slice(int_value_rttid);
        let slice_str = RuntimeType::Slice(string_value_rttid);
        
        let id1 = interner.intern(slice_int);
        let id2 = interner.intern(slice_int2);
        let id3 = interner.intern(slice_str);
        
        assert_eq!(id1, id2);
        assert_ne!(id1, id3);
    }
}
