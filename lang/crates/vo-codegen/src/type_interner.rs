//! Type interning for runtime type identity.
//!
//! This module provides `TypeInterner`, which assigns unique runtime type IDs (rttid)
//! to structurally identical types. This enables O(1) type identity checks at runtime.

use std::collections::HashMap;
use vo_analysis::objects::{ObjKey, TypeKey};
use vo_analysis::typ::Type;
use vo_runtime::bytecode::{InterfaceMeta, InterfaceMethodMeta, StructMeta};
use vo_runtime::{
    ChanDir, InterfaceMethod, RuntimeType, StructField, ValueKind, ValueRttid, INVALID_META_ID,
};

/// Return the ID that would be assigned to the next entry in a packed 24-bit
/// metadata table.
///
/// `INVALID_META_ID` is a wire-format sentinel, so a table may contain at most
/// `INVALID_META_ID` entries with IDs `0..INVALID_META_ID - 1`.
pub(crate) fn checked_next_packed_id(table: &str, len: usize) -> Result<u32, String> {
    let id = u32::try_from(len).map_err(|_| {
        format!(
            "{table} table length {len} exceeds the packed 24-bit ID domain; id 0x{INVALID_META_ID:06x} is reserved"
        )
    })?;
    if id >= INVALID_META_ID {
        return Err(format!(
            "{table} table is full at {len} entries; id 0x{INVALID_META_ID:06x} is reserved"
        ));
    }
    Ok(id)
}

/// Validate an already-built packed-ID table without deriving its length
/// through a narrowing cast.
pub(crate) fn validate_packed_table_len(table: &str, len: usize) -> Result<(), String> {
    if len > INVALID_META_ID as usize {
        return Err(format!(
            "{table} table has {len} entries, exceeding the packed 24-bit limit of {INVALID_META_ID}; id 0x{INVALID_META_ID:06x} is reserved"
        ));
    }
    Ok(())
}

/// Validate a packed table reference against both the reserved sentinel and
/// the current table bounds.
pub(crate) fn validate_packed_id(table: &str, id: u32, len: usize) -> Result<(), String> {
    if id >= INVALID_META_ID || usize::try_from(id).map_or(true, |index| index >= len) {
        return Err(format!(
            "{table} id {id} is outside its {len}-entry table or uses reserved id 0x{INVALID_META_ID:06x}"
        ));
    }
    Ok(())
}

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
    pub fn intern(&mut self, rt: RuntimeType) -> Result<u32, String> {
        if let Some(&id) = self.cache.get(&rt) {
            return Ok(id);
        }
        let id = checked_next_packed_id("runtime type", self.types.len())?;
        self.types.push(rt);
        self.cache.insert(self.types[id as usize].clone(), id);
        Ok(id)
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
    pub interface_meta_ids: &'a mut std::collections::HashMap<vo_analysis::objects::TypeKey, u32>,
    pub interface_metas: &'a mut Vec<vo_common_core::bytecode::InterfaceMeta>,
    pub layout_errors: &'a mut Vec<String>,
}

fn field_meta_equivalent(
    left: &vo_common_core::bytecode::FieldMeta,
    right: &vo_common_core::bytecode::FieldMeta,
) -> bool {
    left.name == right.name
        && left.offset == right.offset
        && left.slot_count == right.slot_count
        && left.type_info == right.type_info
        && left.embedded == right.embedded
        && left.tag == right.tag
}

fn struct_meta_equivalent(left: &StructMeta, right: &StructMeta) -> bool {
    left.slot_types == right.slot_types
        && left.fields.len() == right.fields.len()
        && left
            .fields
            .iter()
            .zip(right.fields.iter())
            .all(|(left, right)| field_meta_equivalent(left, right))
}

fn find_equivalent_struct_meta(
    struct_metas: &[StructMeta],
    candidate: &StructMeta,
) -> Result<Option<u32>, String> {
    struct_metas
        .iter()
        .position(|existing| struct_meta_equivalent(existing, candidate))
        .map(|idx| {
            let id = u32::try_from(idx)
                .map_err(|_| format!("struct metadata index {idx} exceeds the u32 index domain"))?;
            validate_packed_id("struct metadata", id, struct_metas.len())?;
            Ok(id)
        })
        .transpose()
}

fn void_value_rttid() -> ValueRttid {
    ValueRttid::new(ValueKind::Void as u32, ValueKind::Void)
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
    if !ctx.layout_errors.is_empty() {
        return void_value_rttid();
    }
    match try_intern_type_key(interner, type_key, tc_objs, str_interner, ctx) {
        Ok(value_rttid) => value_rttid,
        Err(error) => {
            ctx.layout_errors.push(error);
            void_value_rttid()
        }
    }
}

fn try_intern_type_key(
    interner: &mut TypeInterner,
    type_key: TypeKey,
    tc_objs: &vo_analysis::objects::TCObjects,
    str_interner: &vo_common::SymbolInterner,
    ctx: &mut InternContext,
) -> Result<ValueRttid, String> {
    let (rt, vk) = type_key_to_runtime_type(interner, type_key, tc_objs, str_interner, ctx)?;
    let rttid = interner.intern(rt)?;
    match interner.types().get(rttid as usize) {
        Some(RuntimeType::Struct { meta_id, .. }) => {
            ctx.struct_meta_ids.insert(type_key, *meta_id);
        }
        Some(RuntimeType::Interface { meta_id, .. }) => {
            ctx.interface_meta_ids.insert(type_key, *meta_id);
        }
        _ => {}
    }
    ValueRttid::try_new(rttid, vk).ok_or_else(|| {
        format!(
            "runtime type id {rttid} exceeds the packed 24-bit ID domain; id 0x{INVALID_META_ID:06x} is reserved"
        )
    })
}

fn tuple_value_rttids(
    interner: &mut TypeInterner,
    tuple_key: TypeKey,
    tc_objs: &vo_analysis::objects::TCObjects,
    str_interner: &vo_common::SymbolInterner,
    ctx: &mut InternContext,
    context: &str,
) -> Result<Vec<ValueRttid>, String> {
    let Type::Tuple(tuple) = &tc_objs.types[tuple_key] else {
        return Err(format!(
            "{context}: signature metadata must reference a tuple type"
        ));
    };
    tuple
        .vars()
        .iter()
        .map(|&obj_key| -> Result<ValueRttid, String> {
            let typ = tc_objs.lobjs[obj_key]
                .typ()
                .ok_or_else(|| format!("{context}: tuple object is missing type metadata"))?;
            try_intern_type_key(interner, typ, tc_objs, str_interner, ctx)
        })
        .collect()
}

/// Helper: converts a type_key to (RuntimeType, ValueKind).
/// This doesn't intern, just returns the RuntimeType and its ValueKind.
fn type_key_to_runtime_type(
    interner: &mut TypeInterner,
    type_key: TypeKey,
    tc_objs: &vo_analysis::objects::TCObjects,
    str_interner: &vo_common::SymbolInterner,
    ctx: &mut InternContext,
) -> Result<(RuntimeType, ValueKind), String> {
    let result = match &tc_objs.types[type_key] {
        Type::Basic(_) => {
            let vk = vo_analysis::check::type_info::type_value_kind(type_key, tc_objs);
            (RuntimeType::Basic(vk), vk)
        }
        Type::Named(named) => {
            // Named types are nominal for identity, but their ValueKind is determined by
            // the underlying type category.
            //
            // IMPORTANT: do NOT recursively convert the full underlying RuntimeType here.
            // For self-referential named interface types (e.g. builtin error with Unwrap() error),
            // recursing into interface method signatures would cause infinite recursion.
            let vk = vo_analysis::check::type_info::type_value_kind(type_key, tc_objs);
            // Use ObjKey for lookup - dynamically register if not found
            let id = if let Some(&obj_key) = named.obj().as_ref() {
                if let Some(&id) = ctx.named_type_ids.get(&obj_key) {
                    if id >= INVALID_META_ID || ctx.named_type_metas.get(id as usize).is_none() {
                        return Err(format!(
                            "named type metadata id {id} is outside its table or uses reserved id 0x{INVALID_META_ID:06x}"
                        ));
                    }
                    id
                } else {
                    let id =
                        checked_next_packed_id("named type metadata", ctx.named_type_metas.len())?;
                    // Push placeholder - will be filled in later by register_named_type_meta
                    ctx.named_type_metas
                        .push(vo_common_core::bytecode::NamedTypeMeta {
                            name: String::new(),
                            underlying_meta: vo_runtime::ValueMeta::new(
                                0,
                                vo_runtime::ValueKind::Void,
                            ),
                            underlying_rttid: vo_runtime::ValueRttid::new(
                                0,
                                vo_runtime::ValueKind::Void,
                            ),
                            methods: std::collections::BTreeMap::new(),
                        });
                    ctx.named_type_ids.insert(obj_key, id);
                    id
                }
            } else {
                0
            };
            // Get struct_meta_id from named_type's underlying type
            let struct_meta_id = ctx.struct_meta_ids.get(&type_key).copied();
            (RuntimeType::Named { id, struct_meta_id }, vk)
        }
        Type::Pointer(ptr) => {
            let elem_value_rttid =
                try_intern_type_key(interner, ptr.base(), tc_objs, str_interner, ctx)?;
            (RuntimeType::Pointer(elem_value_rttid), ValueKind::Pointer)
        }
        Type::Array(arr) => {
            let elem_value_rttid =
                try_intern_type_key(interner, arr.elem(), tc_objs, str_interner, ctx)?;
            let len = arr.len().ok_or_else(|| {
                "array length must be resolved before runtime type interning".to_string()
            })?;
            (
                RuntimeType::Array {
                    len,
                    elem: elem_value_rttid,
                },
                ValueKind::Array,
            )
        }
        Type::Slice(slice) => {
            let elem_value_rttid =
                try_intern_type_key(interner, slice.elem(), tc_objs, str_interner, ctx)?;
            (RuntimeType::Slice(elem_value_rttid), ValueKind::Slice)
        }
        Type::Map(map) => {
            let key_value_rttid =
                try_intern_type_key(interner, map.key(), tc_objs, str_interner, ctx)?;
            let val_value_rttid =
                try_intern_type_key(interner, map.elem(), tc_objs, str_interner, ctx)?;
            (
                RuntimeType::Map {
                    key: key_value_rttid,
                    val: val_value_rttid,
                },
                ValueKind::Map,
            )
        }
        Type::Chan(chan) => {
            let elem_value_rttid =
                try_intern_type_key(interner, chan.elem(), tc_objs, str_interner, ctx)?;
            let dir = match chan.dir() {
                vo_analysis::typ::ChanDir::SendRecv => ChanDir::Both,
                vo_analysis::typ::ChanDir::SendOnly => ChanDir::Send,
                vo_analysis::typ::ChanDir::RecvOnly => ChanDir::Recv,
            };
            (
                RuntimeType::Chan {
                    dir,
                    elem: elem_value_rttid,
                },
                ValueKind::Channel,
            )
        }
        Type::Port(port) => {
            let elem_value_rttid =
                try_intern_type_key(interner, port.elem(), tc_objs, str_interner, ctx)?;
            let dir = match port.dir() {
                vo_analysis::typ::ChanDir::SendRecv => ChanDir::Both,
                vo_analysis::typ::ChanDir::SendOnly => ChanDir::Send,
                vo_analysis::typ::ChanDir::RecvOnly => ChanDir::Recv,
            };
            (
                RuntimeType::Port {
                    dir,
                    elem: elem_value_rttid,
                },
                ValueKind::Port,
            )
        }
        Type::Signature(sig) => {
            let params = tuple_value_rttids(
                interner,
                sig.params(),
                tc_objs,
                str_interner,
                ctx,
                "function parameter runtime type",
            )?;
            let results = tuple_value_rttids(
                interner,
                sig.results(),
                tc_objs,
                str_interner,
                ctx,
                "function result runtime type",
            )?;
            (
                RuntimeType::Func {
                    params,
                    results,
                    variadic: sig.variadic(),
                },
                ValueKind::Closure,
            )
        }
        Type::Struct(s) => {
            // Check if already registered (named struct)
            let existing_meta_id = ctx.struct_meta_ids.get(&type_key).copied();
            if let Some(id) = existing_meta_id {
                validate_packed_id("struct metadata", id, ctx.struct_metas.len())?;
            }
            let needs_registration = existing_meta_id.is_none();

            // Single pass: build RuntimeType fields and StructMeta simultaneously
            let mut rt_fields = Vec::with_capacity(s.fields().len());
            let mut field_metas = if needs_registration {
                Vec::with_capacity(s.fields().len())
            } else {
                Vec::new()
            };
            let mut slot_types = Vec::new();
            let mut offset = 0u16;

            for (i, &f) in s.fields().iter().enumerate() {
                let obj = &tc_objs.lobjs[f];
                let name = obj.name().to_string();
                let embedded = obj.entity_type().var_property().embedded;
                let pkg = if obj.exported() {
                    String::new()
                } else {
                    obj.pkg()
                        .and_then(|p| tc_objs.pkgs.get(p))
                        .map(|pkg| pkg.path().to_string())
                        .unwrap_or_default()
                };
                let tag = s.tag(i).cloned();

                let (typ_value_rttid, field_slot_count) = if let Some(field_type) = obj.typ() {
                    let typ_value_rttid =
                        try_intern_type_key(interner, field_type, tc_objs, str_interner, ctx)?;
                    let slots =
                        vo_analysis::check::type_info::type_slot_count_usize(field_type, tc_objs)
                            .ok_or_else(|| {
                            format!("type slot count overflow for type {:?}", field_type)
                        })?;
                    let slot_count = u16::try_from(slots)
                        .map_err(|_| format!("type slot count exceeds u16::MAX: {slots} slots"))?;

                    if needs_registration {
                        let field_slot_types =
                            vo_analysis::check::type_info::type_slot_types(field_type, tc_objs);
                        slot_types.extend(field_slot_types);
                    }

                    (typ_value_rttid, slot_count)
                } else {
                    (ValueRttid::new(ValueKind::Void as u32, ValueKind::Void), 1)
                };

                rt_fields.push(StructField::new(
                    name.clone(),
                    typ_value_rttid,
                    tag.clone().unwrap_or_default(),
                    embedded,
                    pkg,
                ));

                if needs_registration {
                    let next_offset = offset.checked_add(field_slot_count).ok_or_else(|| {
                        format!(
                            "type slot count exceeds u16::MAX: {} slots",
                            offset as usize + field_slot_count as usize
                        )
                    })?;
                    field_metas.push(vo_common_core::bytecode::FieldMeta {
                        name,
                        offset,
                        slot_count: field_slot_count,
                        type_info: typ_value_rttid,
                        embedded,
                        tag,
                    });
                    offset = next_offset;
                }
            }

            let meta_id = if let Some(id) = existing_meta_id {
                id
            } else {
                // Register anonymous struct
                if slot_types.is_empty() {
                    slot_types.push(vo_runtime::SlotType::Value);
                }
                let field_index: HashMap<String, usize> = field_metas
                    .iter()
                    .enumerate()
                    .filter(|(_, field)| field.name != "_")
                    .map(|(i, f)| (f.name.clone(), i))
                    .collect();
                let meta = vo_common_core::bytecode::StructMeta {
                    slot_types,
                    fields: field_metas,
                    field_index,
                };
                let id = if let Some(id) = find_equivalent_struct_meta(ctx.struct_metas, &meta)? {
                    id
                } else {
                    let id = checked_next_packed_id("struct metadata", ctx.struct_metas.len())?;
                    ctx.struct_metas.push(meta);
                    id
                };
                ctx.struct_meta_ids.insert(type_key, id);
                id
            };

            (
                RuntimeType::Struct {
                    fields: rt_fields,
                    meta_id,
                },
                ValueKind::Struct,
            )
        }
        Type::Interface(iface) => {
            let all_methods = iface.all_methods();
            let method_keys: &[vo_analysis::objects::ObjKey] = all_methods
                .as_ref()
                .map(|v| v.as_slice())
                .unwrap_or_else(|| iface.methods());
            let methods: Vec<InterfaceMethod> = method_keys
                .iter()
                .map(|&m| -> Result<InterfaceMethod, String> {
                    let obj = &tc_objs.lobjs[m];
                    let name = obj.id(tc_objs).into_owned();
                    let typ = obj.typ().ok_or_else(|| {
                        format!("interface method {name} is missing type metadata")
                    })?;
                    let sig_value_rttid =
                        try_intern_type_key(interner, typ, tc_objs, str_interner, ctx)?;
                    if sig_value_rttid.value_kind() != ValueKind::Closure {
                        return Err(format!(
                            "interface method {name} runtime type is not a function signature"
                        ));
                    }
                    Ok(InterfaceMethod::new(name, sig_value_rttid))
                })
                .collect::<Result<_, _>>()?;
            let meta_id = get_or_create_interface_meta_id(
                interner,
                type_key,
                iface,
                tc_objs,
                str_interner,
                ctx,
            )?;
            (
                RuntimeType::Interface { methods, meta_id },
                ValueKind::Interface,
            )
        }
        Type::Tuple(tuple) => {
            let elems: Vec<ValueRttid> = tuple
                .vars()
                .iter()
                .map(|&v| -> Result<ValueRttid, String> {
                    let obj = &tc_objs.lobjs[v];
                    let typ = obj.typ().ok_or_else(|| {
                        "tuple runtime type element is missing type metadata".to_string()
                    })?;
                    try_intern_type_key(interner, typ, tc_objs, str_interner, ctx)
                })
                .collect::<Result<_, _>>()?;
            (RuntimeType::Tuple(elems), ValueKind::Void)
        }
        Type::Island => (RuntimeType::Island, ValueKind::Island),
    };
    Ok(result)
}

fn get_or_create_interface_meta_id(
    interner: &mut TypeInterner,
    type_key: TypeKey,
    iface: &vo_analysis::typ::InterfaceDetail,
    tc_objs: &vo_analysis::objects::TCObjects,
    str_interner: &vo_common::SymbolInterner,
    ctx: &mut InternContext,
) -> Result<u32, String> {
    let all_methods = iface.all_methods();
    let method_keys: &[vo_analysis::objects::ObjKey] = all_methods
        .as_ref()
        .map(|v| v.as_slice())
        .unwrap_or_else(|| iface.methods());
    if method_keys.is_empty() {
        validate_packed_id("interface metadata", 0, ctx.interface_metas.len())?;
        ctx.interface_meta_ids.insert(type_key, 0);
        return Ok(0);
    }
    if let Some(id) = ctx.interface_meta_ids.get(&type_key).copied() {
        validate_packed_id("interface metadata", id, ctx.interface_metas.len())?;
        return Ok(id);
    }

    let id = checked_next_packed_id("interface metadata", ctx.interface_metas.len())?;
    ctx.interface_meta_ids.insert(type_key, id);
    ctx.interface_metas.push(InterfaceMeta {
        name: String::new(),
        method_names: Vec::new(),
        methods: Vec::new(),
    });

    let mut method_names = Vec::with_capacity(method_keys.len());
    let mut methods = Vec::with_capacity(method_keys.len());
    for &method_key in method_keys {
        let obj = &tc_objs.lobjs[method_key];
        let name = obj.id(tc_objs).into_owned();
        let sig_type = obj
            .typ()
            .ok_or_else(|| format!("interface method {name} is missing signature type"))?;
        let signature_rttid =
            try_intern_type_key(interner, sig_type, tc_objs, str_interner, ctx)?.rttid();
        method_names.push(name.clone());
        methods.push(InterfaceMethodMeta {
            name,
            signature_rttid,
        });
    }

    ctx.interface_metas[id as usize] = InterfaceMeta {
        name: String::new(),
        method_names,
        methods,
    };
    Ok(id)
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

        let id1 = interner.intern(rt1).unwrap();
        let id2 = interner.intern(rt2).unwrap();
        let id3 = interner.intern(rt3).unwrap();

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

        let id1 = interner.intern(slice_int).unwrap();
        let id2 = interner.intern(slice_int2).unwrap();
        let id3 = interner.intern(slice_str).unwrap();

        assert_eq!(id1, id2);
        assert_ne!(id1, id3);
    }

    #[test]
    fn packed_id_boundaries_do_not_require_large_allocations() {
        let max_valid_id = INVALID_META_ID - 1;

        assert_eq!(
            checked_next_packed_id("test", (max_valid_id - 1) as usize),
            Ok(max_valid_id - 1)
        );
        assert_eq!(
            checked_next_packed_id("test", max_valid_id as usize),
            Ok(max_valid_id)
        );
        assert!(checked_next_packed_id("test", INVALID_META_ID as usize).is_err());

        assert!(validate_packed_table_len("test", INVALID_META_ID as usize).is_ok());
        assert!(validate_packed_table_len("test", INVALID_META_ID as usize + 1).is_err());

        assert!(validate_packed_id("test", max_valid_id, INVALID_META_ID as usize).is_ok());
        assert!(validate_packed_id("test", INVALID_META_ID, INVALID_META_ID as usize).is_err());
    }
}
