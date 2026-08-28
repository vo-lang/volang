//! Backend-independent metadata rules for dynamic field selection.
//!
//! Execution backends own object access and boxing, while this module owns
//! the language-visible rules for exported names, `dyn` tags, embedded-field
//! promotion, ambiguity, and pointer traversal. Keeping those decisions here
//! prevents reflection semantics from drifting between runtimes.

#[cfg(not(feature = "std"))]
use alloc::vec;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;
#[cfg(feature = "std")]
use std::vec::Vec;

use crate::bytecode::{FieldMeta, Module, StructMeta};
use crate::types::{SlotType, ValueRttid};
use crate::{is_exported_name, lookup_struct_tag_value, RuntimeType, ValueKind};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct DynamicPtrDeref {
    pub offset: u16,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DynamicFieldInfo {
    pub offset: u16,
    pub slot_count: u16,
    pub value_rttid: ValueRttid,
    pub slot_types: Vec<SlotType>,
    pub ptr_derefs: Vec<DynamicPtrDeref>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DynamicFieldLookup {
    Found(DynamicFieldInfo),
    Missing,
    Ambiguous,
    Invalid,
}

struct FieldSearchNode {
    struct_meta_id: usize,
    base_offset: u16,
    path_tail: Option<usize>,
    multiplicity: u8,
}

struct DerefLink {
    parent: Option<usize>,
    deref: DynamicPtrDeref,
}

pub fn dynamic_field_name(field: &FieldMeta) -> Option<&str> {
    if field.name == "_" {
        return None;
    }
    if let Some(tag_name) = field
        .tag
        .as_deref()
        .and_then(|tag| lookup_struct_tag_value(tag, "dyn"))
    {
        return (!tag_name.is_empty() && tag_name != "-").then_some(tag_name);
    }
    is_exported_name(&field.name).then_some(field.name.as_str())
}

fn dynamic_embedding_is_visible(field: &FieldMeta) -> bool {
    if field.name == "_" {
        return false;
    }
    match field
        .tag
        .as_deref()
        .and_then(|tag| lookup_struct_tag_value(tag, "dyn"))
    {
        Some("") | Some("-") => false,
        Some(_) | None => true,
    }
}

fn checked_field_range<'a>(meta: &'a StructMeta, field: &FieldMeta) -> Option<&'a [SlotType]> {
    let start = usize::from(field.offset);
    let end = start.checked_add(usize::from(field.slot_count))?;
    meta.slot_types.get(start..end)
}

fn embedded_struct_meta(module: &Module, field: &FieldMeta) -> Option<(usize, bool)> {
    let resolver = module.runtime_type_resolver();
    let (_, runtime_type) = resolver.resolve_value_rttid(field.type_info)?;
    let (struct_rttid, is_pointer) = match runtime_type {
        RuntimeType::Pointer(inner) => (*inner, true),
        RuntimeType::Struct { .. } => (field.type_info, false),
        _ => return None,
    };
    let struct_meta = resolver.canonical_value_meta_for_value_rttid(struct_rttid)?;
    if struct_meta.value_kind() != ValueKind::Struct {
        return None;
    }
    let struct_meta_id = usize::try_from(struct_meta.meta_id()).ok()?;
    module.struct_metas.get(struct_meta_id)?;
    Some((struct_meta_id, is_pointer))
}

fn materialize_deref_path(mut tail: Option<usize>, links: &[DerefLink]) -> Vec<DynamicPtrDeref> {
    let mut reversed = Vec::new();
    while let Some(index) = tail {
        let Some(link) = links.get(index) else {
            return Vec::new();
        };
        reversed.push(link.deref);
        tail = link.parent;
    }
    reversed.reverse();
    reversed
}

fn depth_for(seen_depths: &[(usize, usize)], meta_id: usize) -> Option<usize> {
    seen_depths
        .iter()
        .find_map(|(candidate, depth)| (*candidate == meta_id).then_some(*depth))
}

/// Resolve a dynamic field using the same shallowest-depth promotion rules as
/// ordinary field selection. Two candidates at that depth are ambiguous.
pub fn lookup_dynamic_field(
    module: &Module,
    struct_meta_id: usize,
    field_name: &str,
) -> DynamicFieldLookup {
    let resolver = module.runtime_type_resolver();
    let mut seen_depths = vec![(struct_meta_id, 0usize)];
    let mut links = Vec::new();
    let mut depth = 0usize;
    let mut level = vec![FieldSearchNode {
        struct_meta_id,
        base_offset: 0,
        path_tail: None,
        multiplicity: 1,
    }];

    while !level.is_empty() {
        let mut found = None;
        let mut ambiguous = false;

        for node in &level {
            let Some(meta) = module.struct_metas.get(node.struct_meta_id) else {
                return DynamicFieldLookup::Invalid;
            };
            for field in &meta.fields {
                if dynamic_field_name(field) != Some(field_name) {
                    continue;
                }
                let Some(slot_types) = checked_field_range(meta, field) else {
                    return DynamicFieldLookup::Invalid;
                };
                if resolver
                    .slot_count_for_value_rttid(field.type_info)
                    .is_none_or(|slots| slots != usize::from(field.slot_count))
                {
                    return DynamicFieldLookup::Invalid;
                }
                let Some(offset) = node.base_offset.checked_add(field.offset) else {
                    return DynamicFieldLookup::Invalid;
                };
                if field.slot_count > 0 && offset.checked_add(field.slot_count - 1).is_none() {
                    return DynamicFieldLookup::Invalid;
                }
                let candidate = DynamicFieldInfo {
                    offset,
                    slot_count: field.slot_count,
                    value_rttid: field.type_info,
                    slot_types: slot_types.to_vec(),
                    ptr_derefs: materialize_deref_path(node.path_tail, &links),
                };
                if node.multiplicity > 1 || found.is_some() {
                    ambiguous = true;
                } else {
                    found = Some(candidate);
                }
            }
        }

        if ambiguous {
            return DynamicFieldLookup::Ambiguous;
        }
        if let Some(field) = found {
            return DynamicFieldLookup::Found(field);
        }

        let Some(next_depth) = depth.checked_add(1) else {
            return DynamicFieldLookup::Invalid;
        };
        let mut next_level: Vec<FieldSearchNode> = Vec::new();
        let mut next_indices: Vec<(usize, usize)> = Vec::new();

        for node in &level {
            let Some(meta) = module.struct_metas.get(node.struct_meta_id) else {
                return DynamicFieldLookup::Invalid;
            };
            for field in &meta.fields {
                if !field.embedded || !dynamic_embedding_is_visible(field) {
                    continue;
                }
                if checked_field_range(meta, field).is_none()
                    || resolver
                        .slot_count_for_value_rttid(field.type_info)
                        .is_none_or(|slots| slots != usize::from(field.slot_count))
                {
                    return DynamicFieldLookup::Invalid;
                }
                let Some((embedded_meta_id, is_pointer)) = embedded_struct_meta(module, field)
                else {
                    return DynamicFieldLookup::Invalid;
                };
                if depth_for(&seen_depths, embedded_meta_id).is_some_and(|seen| seen < next_depth) {
                    continue;
                }
                if let Some(index) = next_indices.iter().find_map(|(candidate, index)| {
                    (*candidate == embedded_meta_id).then_some(*index)
                }) {
                    next_level[index].multiplicity = 2;
                    continue;
                }

                let Some(absolute_offset) = node.base_offset.checked_add(field.offset) else {
                    return DynamicFieldLookup::Invalid;
                };
                let (base_offset, path_tail) = if is_pointer {
                    let link_index = links.len();
                    links.push(DerefLink {
                        parent: node.path_tail,
                        deref: DynamicPtrDeref {
                            offset: absolute_offset,
                        },
                    });
                    (0, Some(link_index))
                } else {
                    (absolute_offset, node.path_tail)
                };
                let index = next_level.len();
                next_indices.push((embedded_meta_id, index));
                if depth_for(&seen_depths, embedded_meta_id).is_none() {
                    seen_depths.push((embedded_meta_id, next_depth));
                }
                next_level.push(FieldSearchNode {
                    struct_meta_id: embedded_meta_id,
                    base_offset,
                    path_tail,
                    multiplicity: node.multiplicity,
                });
            }
        }

        level = next_level;
        depth = next_depth;
    }

    DynamicFieldLookup::Missing
}

fn named_type_implements_interface(
    module: &Module,
    source: ValueRttid,
    target_iface_id: u32,
) -> bool {
    let Some(target) = module.interface_metas.get(target_iface_id as usize) else {
        return false;
    };
    if target.methods.is_empty() {
        return true;
    }
    let Some(named_id) = module.named_type_id_for_rttid(source.rttid()) else {
        return false;
    };
    let Some(named) = module.named_type_metas.get(named_id as usize) else {
        return false;
    };
    let source_is_pointer = source.value_kind() == ValueKind::Pointer;
    target.methods.iter().all(|required| {
        named.methods.get(&required.name).is_some_and(|method| {
            method.signature_rttid == required.signature_rttid
                && (source_is_pointer || !method.is_pointer_receiver)
        })
    })
}

fn interface_method_set_includes(
    module: &Module,
    source_meta_id: u32,
    target_meta_id: u32,
) -> bool {
    let Some(source) = module.interface_metas.get(source_meta_id as usize) else {
        return false;
    };
    let Some(target) = module.interface_metas.get(target_meta_id as usize) else {
        return false;
    };
    target.methods.iter().all(|required| {
        source.methods.iter().any(|method| {
            method.name == required.name && method.signature_rttid == required.signature_rttid
        })
    })
}

/// Apply ordinary assignment compatibility using only verified module
/// metadata. Backends use this for dynamic values and FFI boundaries.
pub fn runtime_value_is_assignable(
    source: ValueRttid,
    target: ValueRttid,
    module: &Module,
) -> bool {
    let resolver = module.runtime_type_resolver();
    let Some((source_underlying, source_runtime_type)) = resolver.resolve_value_rttid(source)
    else {
        return false;
    };
    let Some((target_underlying, target_runtime_type)) = resolver.resolve_value_rttid(target)
    else {
        return false;
    };

    if let RuntimeType::Interface {
        meta_id: target_meta_id,
        ..
    } = target_runtime_type
    {
        if source == target {
            return true;
        }
        let Some(target_interface) = module.interface_metas.get(*target_meta_id as usize) else {
            return false;
        };
        if target_interface.methods.is_empty() {
            return true;
        }
        if let RuntimeType::Interface {
            meta_id: source_meta_id,
            ..
        } = source_runtime_type
        {
            return interface_method_set_includes(module, *source_meta_id, *target_meta_id);
        }
        return named_type_implements_interface(module, source, *target_meta_id);
    }

    if source == target {
        return true;
    }
    let Some(source_top_level) = module.runtime_types.get(source.rttid() as usize) else {
        return false;
    };
    let Some(target_top_level) = module.runtime_types.get(target.rttid() as usize) else {
        return false;
    };
    if matches!(source_top_level, RuntimeType::Named { .. })
        && matches!(target_top_level, RuntimeType::Named { .. })
    {
        return false;
    }
    source_underlying == target_underlying
}
