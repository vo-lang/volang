//! Typed value representation for extern function calls.
//!
//! Extern functions receive arguments as typed values (GoxValue),
//! allowing proper formatting and type-safe operations.

use alloc::{format, string::{String, ToString}};
use crate::gc::GcRef;
use crate::objects::string;

// Re-export ValueKind as TypeTag for backward compatibility
pub use gox_common_core::ValueKind as TypeTag;

/// A typed value for FFI.
#[derive(Debug, Clone)]
pub enum GoxValue {
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(GcRef),
    Slice(GcRef),
    Map(GcRef),
    Struct(GcRef),
    Pointer(GcRef),
    Interface { type_id: u32, value: u64 },
}

impl GoxValue {
    /// Create a GoxValue from a raw u64 and type tag.
    pub fn from_raw(raw: u64, tag: TypeTag) -> Self {
        match tag {
            TypeTag::Nil => GoxValue::Nil,
            TypeTag::Bool => GoxValue::Bool(raw != 0),
            TypeTag::Int
            | TypeTag::Int8
            | TypeTag::Int16
            | TypeTag::Int32
            | TypeTag::Int64
            | TypeTag::Uint
            | TypeTag::Uint8
            | TypeTag::Uint16
            | TypeTag::Uint32
            | TypeTag::Uint64 => GoxValue::Int(raw as i64),
            TypeTag::Float32 => GoxValue::Float(f32::from_bits(raw as u32) as f64),
            TypeTag::Float64 => GoxValue::Float(f64::from_bits(raw)),
            TypeTag::String => GoxValue::String(raw as GcRef),
            TypeTag::Slice => GoxValue::Slice(raw as GcRef),
            TypeTag::Map => GoxValue::Map(raw as GcRef),
            TypeTag::Struct => GoxValue::Struct(raw as GcRef),
            TypeTag::Pointer => GoxValue::Pointer(raw as GcRef),
            TypeTag::Interface => GoxValue::Interface {
                type_id: 0,
                value: raw,
            },
            // New types added to ValueKind
            TypeTag::Array => GoxValue::Slice(raw as GcRef), // Arrays are similar to slices at runtime
            TypeTag::Channel => GoxValue::Int(raw as i64),   // Channel handle
            TypeTag::Closure => GoxValue::Int(raw as i64),   // Closure reference
        }
    }

    /// Convert to raw u64 value for storing in registers.
    pub fn to_raw(&self) -> u64 {
        match self {
            GoxValue::Nil => 0,
            GoxValue::Bool(b) => {
                if *b {
                    1
                } else {
                    0
                }
            }
            GoxValue::Int(i) => *i as u64,
            GoxValue::Float(f) => f.to_bits(),
            GoxValue::String(ptr) => *ptr as u64,
            GoxValue::Slice(ptr) => *ptr as u64,
            GoxValue::Map(ptr) => *ptr as u64,
            GoxValue::Struct(ptr) => *ptr as u64,
            GoxValue::Pointer(ptr) => *ptr as u64,
            GoxValue::Interface { value, .. } => *value,
        }
    }

    /// Format this value as a string (for fmt.Println etc).
    pub fn format(&self) -> String {
        match self {
            GoxValue::Nil => "nil".to_string(),
            GoxValue::Bool(b) => if *b { "true" } else { "false" }.to_string(),
            GoxValue::Int(i) => format!("{}", i),
            GoxValue::Float(f) => format!("{}", f),
            GoxValue::String(ptr) => {
                if ptr.is_null() {
                    "".to_string()
                } else {
                    string::as_str(*ptr).to_string()
                }
            }
            GoxValue::Slice(_) => "[...]".to_string(),
            GoxValue::Map(_) => "map[...]".to_string(),
            GoxValue::Struct(_) => "{...}".to_string(),
            GoxValue::Pointer(_) => "*struct{...}".to_string(),
            GoxValue::Interface { .. } => "<interface>".to_string(),
        }
    }
}

/// Argument offset for extern calls.
/// Contains the register index and type tag for each argument.
#[derive(Debug, Clone, Copy)]
pub struct ArgOffset {
    pub reg: u16,
    pub type_tag: TypeTag,
}

/// Arguments wrapper for extern function calls.
pub struct GoxArgs<'a> {
    /// Raw register values.
    raw: &'a [u64],
    /// Argument offsets with type information.
    offsets: &'a [ArgOffset],
}

impl<'a> GoxArgs<'a> {
    pub fn new(raw: &'a [u64], offsets: &'a [ArgOffset]) -> Self {
        Self { raw, offsets }
    }

    pub fn len(&self) -> usize {
        self.offsets.len()
    }

    pub fn is_empty(&self) -> bool {
        self.offsets.is_empty()
    }

    pub fn get(&self, idx: usize) -> GoxValue {
        if idx >= self.offsets.len() {
            return GoxValue::Nil;
        }
        let offset = &self.offsets[idx];
        let raw = self.raw.get(offset.reg as usize).copied().unwrap_or(0);
        GoxValue::from_raw(raw, offset.type_tag)
    }

    /// Format all arguments with spaces between them (like fmt.Println).
    pub fn format_all(&self) -> String {
        let mut output = String::new();
        for i in 0..self.len() {
            if i > 0 {
                output.push(' ');
            }
            output.push_str(&self.get(i).format());
        }
        output
    }
}
