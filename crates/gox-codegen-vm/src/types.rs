//! Type utilities for mapping Type → RuntimeTypeId and SlotType.

use gox_analysis::types::{BasicType, NamedTypeInfo, Type};
use gox_common_core::{RuntimeTypeId, SlotType};

/// Map Type to RuntimeTypeId.
pub fn type_to_runtime_id(ty: &Type, named_types: &[NamedTypeInfo]) -> u32 {
    match ty {
        Type::Basic(b) => basic_to_runtime_id(*b),
        Type::Slice(_) => RuntimeTypeId::Slice as u32,
        Type::Map(_) => RuntimeTypeId::Map as u32,
        Type::Array(_) => RuntimeTypeId::Array as u32,
        Type::Chan(_) => RuntimeTypeId::Channel as u32,
        Type::Func(_) => RuntimeTypeId::Closure as u32,
        Type::Pointer(inner) => type_to_runtime_id(inner, named_types),
        Type::Struct(_) => RuntimeTypeId::FirstStruct as u32,
        Type::Interface(_) => RuntimeTypeId::FirstInterface as u32,
        Type::Named(id) => {
            let idx = id.0 as usize;
            if idx < named_types.len() {
                type_to_runtime_id(&named_types[idx].underlying, named_types)
            } else {
                RuntimeTypeId::Nil as u32
            }
        }
        Type::Nil | Type::Invalid | Type::Tuple(_) | Type::Untyped(_) => RuntimeTypeId::Nil as u32,
    }
}

/// Get slot count for a type.
pub fn type_slots(ty: &Type, named_types: &[NamedTypeInfo]) -> u16 {
    match ty {
        Type::Basic(_) => 1,
        Type::Slice(_) | Type::Map(_) | Type::Chan(_) | Type::Func(_) | Type::Pointer(_) => 1,
        Type::Array(arr) => (arr.len as u16) * type_slots(&arr.elem, named_types),
        Type::Struct(s) => s.fields.iter().map(|f| type_slots(&f.ty, named_types)).sum(),
        Type::Interface(_) => 2,
        Type::Named(id) => {
            let idx = id.0 as usize;
            if idx < named_types.len() {
                type_slots(&named_types[idx].underlying, named_types)
            } else {
                1
            }
        }
        Type::Nil | Type::Invalid | Type::Tuple(_) | Type::Untyped(_) => 1,
    }
}

/// Get SlotType list for GC scanning.
pub fn type_slot_types(ty: &Type, named_types: &[NamedTypeInfo]) -> Vec<SlotType> {
    match ty {
        Type::Basic(BasicType::String) => vec![SlotType::GcRef],
        Type::Basic(_) => vec![SlotType::Value],
        Type::Slice(_) | Type::Map(_) | Type::Chan(_) | Type::Func(_) | Type::Pointer(_) => {
            vec![SlotType::GcRef]
        }
        Type::Array(arr) => {
            let elem = type_slot_types(&arr.elem, named_types);
            let mut result = Vec::new();
            for _ in 0..arr.len {
                result.extend(elem.iter().copied());
            }
            result
        }
        Type::Struct(s) => {
            let mut result = Vec::new();
            for field in &s.fields {
                result.extend(type_slot_types(&field.ty, named_types));
            }
            result
        }
        Type::Interface(_) => vec![SlotType::Interface0, SlotType::Interface1],
        Type::Named(id) => {
            let idx = id.0 as usize;
            if idx < named_types.len() {
                type_slot_types(&named_types[idx].underlying, named_types)
            } else {
                vec![SlotType::Value]
            }
        }
        Type::Nil | Type::Invalid | Type::Tuple(_) | Type::Untyped(_) => vec![SlotType::Value],
    }
}

fn basic_to_runtime_id(b: BasicType) -> u32 {
    match b {
        BasicType::Bool => RuntimeTypeId::Bool as u32,
        BasicType::Int => RuntimeTypeId::Int as u32,
        BasicType::Int8 => RuntimeTypeId::Int8 as u32,
        BasicType::Int16 => RuntimeTypeId::Int16 as u32,
        BasicType::Int32 => RuntimeTypeId::Int32 as u32,
        BasicType::Int64 => RuntimeTypeId::Int64 as u32,
        BasicType::Uint => RuntimeTypeId::Uint as u32,
        BasicType::Uint8 => RuntimeTypeId::Uint8 as u32,
        BasicType::Uint16 => RuntimeTypeId::Uint16 as u32,
        BasicType::Uint32 => RuntimeTypeId::Uint32 as u32,
        BasicType::Uint64 => RuntimeTypeId::Uint64 as u32,
        BasicType::Float32 => RuntimeTypeId::Float32 as u32,
        BasicType::Float64 => RuntimeTypeId::Float64 as u32,
        BasicType::String => RuntimeTypeId::String as u32,
    }
}
