//! Value formatting utilities.
//!
//! Used by builtin print/println and fmt package.

use crate::gc::GcRef;
use crate::objects::string;
use vo_common_core::types::ValueKind;

#[cfg(not(feature = "std"))]
extern crate alloc;

#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};

#[cfg(feature = "std")]
use std::string::{String, ToString};

/// Format a value based on its kind.
pub fn format_value(val: u64, kind: ValueKind) -> String {
    match kind {
        ValueKind::Void => "nil".to_string(),
        ValueKind::Bool => if val != 0 { "true" } else { "false" }.to_string(),
        ValueKind::Int | ValueKind::Int64 => (val as i64).to_string(),
        ValueKind::Int8 => (val as i8).to_string(),
        ValueKind::Int16 => (val as i16).to_string(),
        ValueKind::Int32 => (val as i32).to_string(),
        ValueKind::Uint | ValueKind::Uint64 => val.to_string(),
        ValueKind::Uint8 => (val as u8).to_string(),
        ValueKind::Uint16 => (val as u16).to_string(),
        ValueKind::Uint32 => (val as u32).to_string(),
        ValueKind::Float32 => f32::from_bits(val as u32).to_string(),
        ValueKind::Float64 => f64::from_bits(val).to_string(),
        ValueKind::String => {
            let ptr = val as GcRef;
            if ptr.is_null() {
                String::new()
            } else {
                string::as_str(ptr).to_string()
            }
        }
        ValueKind::Slice => "[...]".to_string(),
        ValueKind::Array => "[...]".to_string(),
        ValueKind::Map => "map[...]".to_string(),
        ValueKind::Struct => "{...}".to_string(),
        ValueKind::Pointer => {
            #[cfg(feature = "std")]
            { format!("0x{:x}", val) }
            #[cfg(not(feature = "std"))]
            { alloc::format!("0x{:x}", val) }
        }
        ValueKind::Interface => "<interface>".to_string(),
        ValueKind::Channel => "<chan>".to_string(),
        ValueKind::Closure => "<func>".to_string(),
        ValueKind::FuncPtr => "<func>".to_string(),
    }
}
