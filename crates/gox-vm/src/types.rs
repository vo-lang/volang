//! Type metadata and type system.

use alloc::{string::{String, ToString}, vec, vec::Vec};
use hashbrown::HashMap;
use gox_common_core::{RuntimeTypeId, SlotType, ValueKind};

/// Type ID (index into type table).
pub type TypeId = u32;

/// Field layout info for compact struct representation.
#[derive(Clone, Debug, Default)]
pub struct FieldLayout {
    /// Byte offset from start of struct data.
    pub byte_offset: u32,
    /// Size in bytes (1, 2, 4, or 8).
    pub size: u8,
    /// Whether this field is signed (for sign extension on read).
    pub signed: bool,
}

impl FieldLayout {
    pub fn new(byte_offset: u32, size: u8, signed: bool) -> Self {
        Self { byte_offset, size, signed }
    }
    
    /// Encode size as 2-bit code: 0=1, 1=2, 2=4, 3=8 bytes.
    pub fn size_code(&self) -> u8 {
        match self.size {
            1 => 0,
            2 => 1,
            4 => 2,
            8 => 3,
            _ => 3, // Default to 8
        }
    }
    
    /// Get flags for GetField/SetField instruction.
    /// flags[1:0] = size_code, flags[2] = signed
    pub fn flags(&self) -> u8 {
        self.size_code() | (if self.signed { 0b100 } else { 0 })
    }
}

/// Type metadata.
#[derive(Clone, Debug)]
pub struct TypeMeta {
    /// ValueKind for this type.
    pub value_kind: ValueKind,
    /// Runtime type ID (only for Struct/Interface, indexes into meta tables).
    pub type_id: RuntimeTypeId,
    /// Size in 8-byte slots (for GC allocation, backward compat).
    pub size_slots: usize,
    /// Size in bytes (compact layout).
    pub size_bytes: usize,
    /// Slot types for GC scanning. Each slot describes how GC should handle it.
    pub slot_types: Vec<SlotType>,
    pub name: String,
    
    // For struct/object: field layouts (compact)
    pub field_layouts: Vec<FieldLayout>,
    
    // For array/slice/channel: element type and size
    pub elem_type: Option<TypeId>,
    pub elem_size: Option<usize>,
    
    // For map: key and value types
    pub key_type: Option<TypeId>,
    pub value_type: Option<TypeId>,
}

impl TypeMeta {
    /// Check if this is a struct type.
    pub fn is_struct(&self) -> bool {
        self.value_kind == ValueKind::Struct
    }
    
    /// Check if this is an interface type.
    pub fn is_interface(&self) -> bool {
        self.value_kind == ValueKind::Interface
    }
    
    /// Check if this type needs GC scanning.
    pub fn needs_gc(&self) -> bool {
        self.value_kind.needs_gc()
    }
    
    /// Create a builtin type.
    pub fn builtin(value_kind: ValueKind, name: &str, size_slots: usize, slot_types: Vec<SlotType>) -> Self {
        Self {
            value_kind,
            type_id: 0,
            size_slots,
            size_bytes: size_slots * 8,
            slot_types,
            name: name.to_string(),
            field_layouts: vec![],
            elem_type: None,
            elem_size: None,
            key_type: None,
            value_type: None,
        }
    }
    
    pub fn nil() -> Self {
        Self::builtin(ValueKind::Nil, "nil", 0, vec![])
    }
    
    pub fn primitive(value_kind: ValueKind, name: &str) -> Self {
        Self::builtin(value_kind, name, 1, vec![SlotType::Value])
    }
    
    pub fn struct_(type_id: RuntimeTypeId, name: &str, size_slots: usize, slot_types: Vec<SlotType>) -> Self {
        Self {
            value_kind: ValueKind::Struct,
            type_id,
            size_slots,
            size_bytes: size_slots * 8,
            slot_types,
            name: name.to_string(),
            field_layouts: vec![],
            elem_type: None,
            elem_size: None,
            key_type: None,
            value_type: None,
        }
    }
    
    pub fn object(value_kind: ValueKind, name: &str, size_slots: usize, slot_types: Vec<SlotType>) -> Self {
        Self {
            value_kind,
            type_id: 0,
            size_slots,
            size_bytes: size_slots * 8,
            slot_types,
            name: name.to_string(),
            field_layouts: vec![],
            elem_type: None,
            elem_size: None,
            key_type: None,
            value_type: None,
        }
    }
    
    pub fn is_primitive(&self) -> bool {
        (self.value_kind as u8) <= (ValueKind::FuncPtr as u8)
    }
    
    /// Get field layout by index.
    pub fn get_field_layout(&self, idx: usize) -> Option<&FieldLayout> {
        self.field_layouts.get(idx)
    }
}

/// Type table with separate struct and interface metadata.
/// Struct and interface type_ids are independent (both start from 0).
#[derive(Clone, Debug, Default)]
pub struct TypeTable {
    /// Struct type metadata, indexed by struct type_id (0-based).
    struct_metas: Vec<TypeMeta>,
    /// Interface type metadata, indexed by interface type_id (0-based).
    interface_metas: Vec<TypeMeta>,
}

impl TypeTable {
    pub fn new() -> Self {
        Self {
            struct_metas: Vec::new(),
            interface_metas: Vec::new(),
        }
    }
    
    /// Load struct types from module.
    pub fn load_struct_types(&mut self, types: &[TypeMeta]) {
        for meta in types {
            let idx = meta.type_id as usize;
            if idx >= self.struct_metas.len() {
                self.struct_metas.resize(idx + 1, TypeMeta::nil());
            }
            self.struct_metas[idx] = meta.clone();
        }
    }
    
    /// Load interface types from module.
    pub fn load_interface_types(&mut self, types: &[TypeMeta]) {
        for meta in types {
            let idx = meta.type_id as usize;
            if idx >= self.interface_metas.len() {
                self.interface_metas.resize(idx + 1, TypeMeta::nil());
            }
            self.interface_metas[idx] = meta.clone();
        }
    }
    
    /// Get struct metadata by type_id.
    pub fn get_struct(&self, type_id: u32) -> Option<&TypeMeta> {
        self.struct_metas.get(type_id as usize)
    }
    
    /// Get struct metadata by type_id (unchecked).
    pub fn get_struct_unchecked(&self, type_id: u32) -> &TypeMeta {
        &self.struct_metas[type_id as usize]
    }
    
    /// Get interface metadata by type_id.
    pub fn get_interface(&self, type_id: u32) -> Option<&TypeMeta> {
        self.interface_metas.get(type_id as usize)
    }
    
    /// Get interface metadata by type_id (unchecked).
    pub fn get_interface_unchecked(&self, type_id: u32) -> &TypeMeta {
        &self.interface_metas[type_id as usize]
    }
    
    /// Get number of struct types.
    pub fn struct_count(&self) -> usize {
        self.struct_metas.len()
    }
    
    /// Get number of interface types.
    pub fn interface_count(&self) -> usize {
        self.interface_metas.len()
    }
    
    pub fn is_empty(&self) -> bool {
        self.struct_metas.is_empty() && self.interface_metas.is_empty()
    }
}

