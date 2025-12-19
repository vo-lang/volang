//! Type metadata and type system.

use alloc::{string::{String, ToString}, vec, vec::Vec};
use hashbrown::HashMap;
use gox_common_core::{ValueKind, FIRST_USER_TYPE_ID};

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
    /// Type ID for user-defined types. None for builtin types (use kind as id).
    pub id: Option<TypeId>,
    pub kind: ValueKind,
    /// Size in 8-byte slots (for GC allocation, backward compat).
    pub size_slots: usize,
    /// Size in bytes (compact layout).
    pub size_bytes: usize,
    /// Pointer bitmap: one bit per 8-byte word, true if contains pointer.
    pub ptr_bitmap: Vec<bool>,
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
    /// Get the actual type ID. For builtin types, returns kind as TypeId.
    pub fn type_id(&self) -> TypeId {
        self.id.unwrap_or(self.kind as TypeId)
    }
    
    /// Create a builtin type (id = None, uses kind as id).
    pub fn builtin(kind: ValueKind, name: &str, size_slots: usize, ptr_bitmap: Vec<bool>) -> Self {
        Self {
            id: None,
            kind,
            size_slots,
            size_bytes: size_slots * 8,
            ptr_bitmap,
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
    
    pub fn primitive(kind: ValueKind, name: &str) -> Self {
        Self::builtin(kind, name, 1, vec![false])
    }
    
    pub fn struct_(id: TypeId, name: &str, size_slots: usize, ptr_bitmap: Vec<bool>) -> Self {
        Self {
            id: Some(id),
            kind: ValueKind::Struct,
            size_slots,
            size_bytes: size_slots * 8,
            ptr_bitmap,
            name: name.to_string(),
            field_layouts: vec![],
            elem_type: None,
            elem_size: None,
            key_type: None,
            value_type: None,
        }
    }
    
    pub fn object(id: TypeId, name: &str, size_slots: usize, ptr_bitmap: Vec<bool>) -> Self {
        Self {
            id: Some(id),
            kind: ValueKind::Pointer,
            size_slots,
            size_bytes: size_slots * 8,
            ptr_bitmap,
            name: name.to_string(),
            field_layouts: vec![],
            elem_type: None,
            elem_size: None,
            key_type: None,
            value_type: None,
        }
    }
    
    pub fn is_reference_type(&self) -> bool {
        self.kind.is_reference()
    }
    
    pub fn is_primitive(&self) -> bool {
        matches!(self.kind, ValueKind::Nil | ValueKind::Bool) || self.kind.is_numeric()
    }
    
    /// Get field layout by index.
    pub fn get_field_layout(&self, idx: usize) -> Option<&FieldLayout> {
        self.field_layouts.get(idx)
    }
}

/// Type table (compile-time generated, loaded into VM).
#[derive(Clone, Debug, Default)]
pub struct TypeTable {
    types: Vec<TypeMeta>,
    by_name: HashMap<String, TypeId>,
}

impl TypeTable {
    pub fn new() -> Self {
        let mut table = Self {
            types: Vec::new(),
            by_name: HashMap::new(),
        };
        table.init_builtins();
        table
    }
    
    fn init_builtins(&mut self) {
        // Reserve space for builtin types
        self.types.resize(FIRST_USER_TYPE_ID as usize, TypeMeta::nil());
        
        // Helper to set builtin type at its ValueKind index
        let mut set_builtin = |meta: TypeMeta| {
            self.set(meta.type_id(), meta);
        };
        
        // Primitives
        set_builtin(TypeMeta::nil());
        set_builtin(TypeMeta::primitive(ValueKind::Bool, "bool"));
        set_builtin(TypeMeta::primitive(ValueKind::Int, "int"));
        set_builtin(TypeMeta::primitive(ValueKind::Int8, "int8"));
        set_builtin(TypeMeta::primitive(ValueKind::Int16, "int16"));
        set_builtin(TypeMeta::primitive(ValueKind::Int32, "int32"));
        set_builtin(TypeMeta::primitive(ValueKind::Int64, "int64"));
        set_builtin(TypeMeta::primitive(ValueKind::Uint, "uint"));
        set_builtin(TypeMeta::primitive(ValueKind::Uint8, "uint8"));
        set_builtin(TypeMeta::primitive(ValueKind::Uint16, "uint16"));
        set_builtin(TypeMeta::primitive(ValueKind::Uint32, "uint32"));
        set_builtin(TypeMeta::primitive(ValueKind::Uint64, "uint64"));
        set_builtin(TypeMeta::primitive(ValueKind::Float32, "float32"));
        set_builtin(TypeMeta::primitive(ValueKind::Float64, "float64"));
        
        // String: GcRef (1 slot, is pointer)
        set_builtin(TypeMeta {
            id: None,
            kind: ValueKind::String,
            size_slots: 1,
            size_bytes: 8,
            ptr_bitmap: vec![true],
            name: "string".to_string(),
            field_layouts: vec![],
            elem_type: Some(ValueKind::Uint8 as TypeId),
            elem_size: Some(1),
            key_type: None,
            value_type: None,
        });
        
        // Array: GcRef (1 slot)
        set_builtin(TypeMeta::builtin(ValueKind::Array, "array", 1, vec![true]));
        
        // Slice: GcRef (1 slot)
        set_builtin(TypeMeta::builtin(ValueKind::Slice, "slice", 1, vec![true]));
        
        // Map: GcRef (1 slot)
        set_builtin(TypeMeta::builtin(ValueKind::Map, "map", 1, vec![true]));
        
        // Channel: GcRef (1 slot)
        set_builtin(TypeMeta::builtin(ValueKind::Channel, "channel", 1, vec![true]));
        
        // Closure: GcRef (1 slot)
        set_builtin(TypeMeta::builtin(ValueKind::Closure, "closure", 1, vec![true]));
        
        // Interface: 2 slots (type_id, data)
        set_builtin(TypeMeta::builtin(ValueKind::Interface, "interface{}", 2, vec![false, false]));
    }
    
    fn set(&mut self, id: TypeId, meta: TypeMeta) {
        let idx = id as usize;
        if idx >= self.types.len() {
            self.types.resize(idx + 1, TypeMeta::nil());
        }
        self.by_name.insert(meta.name.clone(), id);
        self.types[idx] = meta;
    }
    
    /// Register a new user-defined type.
    pub fn register(&mut self, mut meta: TypeMeta) -> TypeId {
        let id = self.types.len() as TypeId;
        meta.id = Some(id);
        self.by_name.insert(meta.name.clone(), id);
        self.types.push(meta);
        id
    }
    
    /// Get type metadata by ID.
    pub fn get(&self, id: TypeId) -> Option<&TypeMeta> {
        self.types.get(id as usize)
    }
    
    /// Get type metadata by ID (unchecked).
    pub fn get_unchecked(&self, id: TypeId) -> &TypeMeta {
        &self.types[id as usize]
    }
    
    /// Get type ID by name.
    pub fn get_by_name(&self, name: &str) -> Option<TypeId> {
        self.by_name.get(name).copied()
    }
    
    /// Get number of types.
    pub fn len(&self) -> usize {
        self.types.len()
    }
    
    pub fn is_empty(&self) -> bool {
        self.types.is_empty()
    }
}

// Re-export TypeKind as an alias for backward compatibility
#[deprecated(note = "Use gox_common_core::ValueKind instead")]
pub type TypeKind = ValueKind;
