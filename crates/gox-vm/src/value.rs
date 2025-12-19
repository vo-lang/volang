//! Typed value representation for extern function calls.
//!
//! Extern functions receive arguments as typed values (GoxValue),
//! allowing proper formatting and type-safe operations.

use alloc::{format, string::{String, ToString}};
use crate::gc::GcRef;
use crate::objects::string;

pub use gox_common_core::ValueKind;

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
    pub fn from_raw(raw: u64, tag: ValueKind) -> Self {
        match tag {
            ValueKind::Nil => GoxValue::Nil,
            ValueKind::Bool => GoxValue::Bool(raw != 0),
            ValueKind::Int
            | ValueKind::Int8
            | ValueKind::Int16
            | ValueKind::Int32
            | ValueKind::Int64
            | ValueKind::Uint
            | ValueKind::Uint8
            | ValueKind::Uint16
            | ValueKind::Uint32
            | ValueKind::Uint64 => GoxValue::Int(raw as i64),
            ValueKind::Float32 => GoxValue::Float(f32::from_bits(raw as u32) as f64),
            ValueKind::Float64 => GoxValue::Float(f64::from_bits(raw)),
            ValueKind::String => GoxValue::String(raw as GcRef),
            ValueKind::Slice => GoxValue::Slice(raw as GcRef),
            ValueKind::Map => GoxValue::Map(raw as GcRef),
            ValueKind::Struct => GoxValue::Struct(raw as GcRef),
            ValueKind::Pointer => GoxValue::Pointer(raw as GcRef),
            ValueKind::Interface => GoxValue::Interface {
                type_id: 0,
                value: raw,
            },
            // New types added to ValueKind
            ValueKind::Array => GoxValue::Slice(raw as GcRef), // Arrays are similar to slices at runtime
            ValueKind::Channel => GoxValue::Int(raw as i64),   // Channel handle
            ValueKind::Closure => GoxValue::Int(raw as i64),   // Closure reference
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
    pub type_tag: ValueKind,
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
