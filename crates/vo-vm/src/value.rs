//! Typed value representation for extern function calls.
//!
//! Extern functions receive arguments as typed values (VoValue),
//! allowing proper formatting and type-safe operations.

use alloc::{format, string::{String, ToString}};
use crate::gc::GcRef;
use crate::objects::string;

pub use vo_common_core::ValueKind;

/// A typed value for FFI.
#[derive(Debug, Clone)]
pub enum VoValue {
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

impl VoValue {
    /// Create a VoValue from a raw u64 and type tag.
    pub fn from_raw(raw: u64, tag: ValueKind) -> Self {
        match tag {
            ValueKind::Nil => VoValue::Nil,
            ValueKind::Bool => VoValue::Bool(raw != 0),
            ValueKind::Int
            | ValueKind::Int8
            | ValueKind::Int16
            | ValueKind::Int32
            | ValueKind::Int64
            | ValueKind::Uint
            | ValueKind::Uint8
            | ValueKind::Uint16
            | ValueKind::Uint32
            | ValueKind::Uint64 => VoValue::Int(raw as i64),
            ValueKind::Float32 => VoValue::Float(f32::from_bits(raw as u32) as f64),
            ValueKind::Float64 => VoValue::Float(f64::from_bits(raw)),
            ValueKind::String => VoValue::String(raw as GcRef),
            ValueKind::Slice => VoValue::Slice(raw as GcRef),
            ValueKind::Map => VoValue::Map(raw as GcRef),
            ValueKind::Struct => VoValue::Struct(raw as GcRef),
            ValueKind::Pointer => VoValue::Pointer(raw as GcRef),
            ValueKind::Interface => VoValue::Interface {
                type_id: 0,
                value: raw,
            },
            // New types added to ValueKind
            ValueKind::Array => VoValue::Slice(raw as GcRef), // Arrays are similar to slices at runtime
            ValueKind::Channel => VoValue::Int(raw as i64),   // Channel handle
            ValueKind::Closure => VoValue::Int(raw as i64),   // Closure reference
            ValueKind::FuncPtr => VoValue::Int(raw as i64),   // Function pointer
        }
    }

    /// Convert to raw u64 value for storing in registers.
    pub fn to_raw(&self) -> u64 {
        match self {
            VoValue::Nil => 0,
            VoValue::Bool(b) => {
                if *b {
                    1
                } else {
                    0
                }
            }
            VoValue::Int(i) => *i as u64,
            VoValue::Float(f) => f.to_bits(),
            VoValue::String(ptr) => *ptr as u64,
            VoValue::Slice(ptr) => *ptr as u64,
            VoValue::Map(ptr) => *ptr as u64,
            VoValue::Struct(ptr) => *ptr as u64,
            VoValue::Pointer(ptr) => *ptr as u64,
            VoValue::Interface { value, .. } => *value,
        }
    }

    /// Format this value as a string (for fmt.Println etc).
    pub fn format(&self) -> String {
        match self {
            VoValue::Nil => "nil".to_string(),
            VoValue::Bool(b) => if *b { "true" } else { "false" }.to_string(),
            VoValue::Int(i) => format!("{}", i),
            VoValue::Float(f) => format!("{}", f),
            VoValue::String(ptr) => {
                if ptr.is_null() {
                    "".to_string()
                } else {
                    string::as_str(*ptr).to_string()
                }
            }
            VoValue::Slice(_) => "[...]".to_string(),
            VoValue::Map(_) => "map[...]".to_string(),
            VoValue::Struct(_) => "{...}".to_string(),
            VoValue::Pointer(_) => "*struct{...}".to_string(),
            VoValue::Interface { .. } => "<interface>".to_string(),
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
pub struct VoArgs<'a> {
    /// Raw register values.
    raw: &'a [u64],
    /// Argument offsets with type information.
    offsets: &'a [ArgOffset],
}

impl<'a> VoArgs<'a> {
    pub fn new(raw: &'a [u64], offsets: &'a [ArgOffset]) -> Self {
        Self { raw, offsets }
    }

    pub fn len(&self) -> usize {
        self.offsets.len()
    }

    pub fn is_empty(&self) -> bool {
        self.offsets.is_empty()
    }

    pub fn get(&self, idx: usize) -> VoValue {
        if idx >= self.offsets.len() {
            return VoValue::Nil;
        }
        let offset = &self.offsets[idx];
        let raw = self.raw.get(offset.reg as usize).copied().unwrap_or(0);
        VoValue::from_raw(raw, offset.type_tag)
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
