//! Type metadata and type system.

use std::collections::HashMap;
use gox_common::ValueKind;

/// Type ID (index into type table).
pub type TypeId = u32;

/// Built-in type IDs.
pub mod builtin {
    use super::TypeId;
    
    pub const NIL: TypeId = 0;
    pub const BOOL: TypeId = 1;
    pub const INT: TypeId = 2;
    pub const INT8: TypeId = 3;
    pub const INT16: TypeId = 4;
    pub const INT32: TypeId = 5;
    pub const INT64: TypeId = 6;
    pub const UINT: TypeId = 7;
    pub const UINT8: TypeId = 8;
    pub const UINT16: TypeId = 9;
    pub const UINT32: TypeId = 10;
    pub const UINT64: TypeId = 11;
    pub const FLOAT32: TypeId = 12;
    pub const FLOAT64: TypeId = 13;
    pub const STRING: TypeId = 14;
    pub const ARRAY: TypeId = 15;
    pub const SLICE: TypeId = 16;
    pub const MAP: TypeId = 17;
    pub const CHANNEL: TypeId = 18;
    pub const CLOSURE: TypeId = 19;
    pub const INTERFACE: TypeId = 20;
    
    pub const FIRST_USER_TYPE: TypeId = 100;
}

/// Type metadata.
#[derive(Clone, Debug)]
pub struct TypeMeta {
    pub id: TypeId,
    pub kind: ValueKind,
    pub size_slots: usize,
    pub ptr_bitmap: Vec<bool>,
    pub name: String,
    
    // For struct/object: field offsets (in slots)
    pub field_offsets: Vec<usize>,
    
    // For array/slice/channel: element type and size
    pub elem_type: Option<TypeId>,
    pub elem_size: Option<usize>,
    
    // For map: key and value types
    pub key_type: Option<TypeId>,
    pub value_type: Option<TypeId>,
}

impl TypeMeta {
    pub fn nil() -> Self {
        Self {
            id: builtin::NIL,
            kind: ValueKind::Nil,
            size_slots: 0,
            ptr_bitmap: vec![],
            name: "nil".to_string(),
            field_offsets: vec![],
            elem_type: None,
            elem_size: None,
            key_type: None,
            value_type: None,
        }
    }
    
    pub fn primitive(id: TypeId, name: &str, kind: ValueKind) -> Self {
        Self {
            id,
            kind,
            size_slots: 1,
            ptr_bitmap: vec![false],
            name: name.to_string(),
            field_offsets: vec![],
            elem_type: None,
            elem_size: None,
            key_type: None,
            value_type: None,
        }
    }
    
    pub fn struct_(id: TypeId, name: &str, size_slots: usize, ptr_bitmap: Vec<bool>) -> Self {
        Self {
            id,
            kind: ValueKind::Struct,
            size_slots,
            ptr_bitmap,
            name: name.to_string(),
            field_offsets: vec![],
            elem_type: None,
            elem_size: None,
            key_type: None,
            value_type: None,
        }
    }
    
    pub fn object(id: TypeId, name: &str, size_slots: usize, ptr_bitmap: Vec<bool>) -> Self {
        Self {
            id,
            kind: ValueKind::Obx,
            size_slots,
            ptr_bitmap,
            name: name.to_string(),
            field_offsets: vec![],
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
        use builtin::*;
        
        // Reserve space for builtin types
        self.types.resize(FIRST_USER_TYPE as usize, TypeMeta::nil());
        
        self.set(NIL, TypeMeta::nil());
        self.set(BOOL, TypeMeta::primitive(BOOL, "bool", ValueKind::Bool));
        self.set(INT, TypeMeta::primitive(INT, "int", ValueKind::Int));
        self.set(INT8, TypeMeta::primitive(INT8, "int8", ValueKind::Int8));
        self.set(INT16, TypeMeta::primitive(INT16, "int16", ValueKind::Int16));
        self.set(INT32, TypeMeta::primitive(INT32, "int32", ValueKind::Int32));
        self.set(INT64, TypeMeta::primitive(INT64, "int64", ValueKind::Int64));
        self.set(UINT, TypeMeta::primitive(UINT, "uint", ValueKind::Uint));
        self.set(UINT8, TypeMeta::primitive(UINT8, "uint8", ValueKind::Uint8));
        self.set(UINT16, TypeMeta::primitive(UINT16, "uint16", ValueKind::Uint16));
        self.set(UINT32, TypeMeta::primitive(UINT32, "uint32", ValueKind::Uint32));
        self.set(UINT64, TypeMeta::primitive(UINT64, "uint64", ValueKind::Uint64));
        self.set(FLOAT32, TypeMeta::primitive(FLOAT32, "float32", ValueKind::Float32));
        self.set(FLOAT64, TypeMeta::primitive(FLOAT64, "float64", ValueKind::Float64));
        
        // String: GcRef (1 slot, is pointer)
        self.set(STRING, TypeMeta {
            id: STRING,
            kind: ValueKind::String,
            size_slots: 1,
            ptr_bitmap: vec![true],
            name: "string".to_string(),
            field_offsets: vec![],
            elem_type: Some(UINT8),
            elem_size: Some(1),
            key_type: None,
            value_type: None,
        });
        
        // Array: GcRef (1 slot)
        self.set(ARRAY, TypeMeta {
            id: ARRAY,
            kind: ValueKind::Array,
            size_slots: 1,
            ptr_bitmap: vec![true],
            name: "array".to_string(),
            field_offsets: vec![],
            elem_type: None,
            elem_size: None,
            key_type: None,
            value_type: None,
        });
        
        // Slice: GcRef (1 slot)
        self.set(SLICE, TypeMeta {
            id: SLICE,
            kind: ValueKind::Slice,
            size_slots: 1,
            ptr_bitmap: vec![true],
            name: "slice".to_string(),
            field_offsets: vec![],
            elem_type: None,
            elem_size: None,
            key_type: None,
            value_type: None,
        });
        
        // Map: GcRef (1 slot)
        self.set(MAP, TypeMeta {
            id: MAP,
            kind: ValueKind::Map,
            size_slots: 1,
            ptr_bitmap: vec![true],
            name: "map".to_string(),
            field_offsets: vec![],
            elem_type: None,
            elem_size: None,
            key_type: None,
            value_type: None,
        });
        
        // Channel: GcRef (1 slot)
        self.set(CHANNEL, TypeMeta {
            id: CHANNEL,
            kind: ValueKind::Channel,
            size_slots: 1,
            ptr_bitmap: vec![true],
            name: "channel".to_string(),
            field_offsets: vec![],
            elem_type: None,
            elem_size: None,
            key_type: None,
            value_type: None,
        });
        
        // Closure: GcRef (1 slot)
        self.set(CLOSURE, TypeMeta {
            id: CLOSURE,
            kind: ValueKind::Closure,
            size_slots: 1,
            ptr_bitmap: vec![true],
            name: "closure".to_string(),
            field_offsets: vec![],
            elem_type: None,
            elem_size: None,
            key_type: None,
            value_type: None,
        });
        
        // Interface: 2 slots (type_id, data)
        self.set(INTERFACE, TypeMeta {
            id: INTERFACE,
            kind: ValueKind::Interface,
            size_slots: 2,
            ptr_bitmap: vec![false, false], // data slot depends on actual type
            name: "interface{}".to_string(),
            field_offsets: vec![],
            elem_type: None,
            elem_size: None,
            key_type: None,
            value_type: None,
        });
    }
    
    fn set(&mut self, id: TypeId, meta: TypeMeta) {
        let idx = id as usize;
        if idx >= self.types.len() {
            self.types.resize(idx + 1, TypeMeta::nil());
        }
        self.by_name.insert(meta.name.clone(), id);
        self.types[idx] = meta;
    }
    
    /// Register a new type.
    pub fn register(&mut self, mut meta: TypeMeta) -> TypeId {
        let id = self.types.len() as TypeId;
        meta.id = id;
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
#[deprecated(note = "Use gox_common::ValueKind instead")]
pub type TypeKind = ValueKind;
