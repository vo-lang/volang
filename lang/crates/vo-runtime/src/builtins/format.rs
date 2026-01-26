//! Value formatting utilities.
//!
//! Used by builtin print/println and fmt package.

use crate::gc::{Gc, GcRef};
use crate::objects::{interface, slice, string as str_obj};
use crate::ffi::ExternCallContext;
use vo_common_core::types::ValueKind;

#[cfg(not(feature = "std"))]
extern crate alloc;

#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::format;

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
                str_obj::as_str(ptr).to_string()
            }
        }
        ValueKind::Slice => "[...]".to_string(),
        ValueKind::Array => "[...]".to_string(),
        ValueKind::Map => "map[...]".to_string(),
        ValueKind::Struct => "{...}".to_string(),
        ValueKind::Pointer => format!("0x{:x}", val),
        ValueKind::Interface => "<interface>".to_string(),
        ValueKind::Channel => "<chan>".to_string(),
        ValueKind::Closure => "<func>".to_string(),
    }
}

/// Format a single interface{} value (2 slots) to string.
/// For error interface, extracts the error message.
pub fn format_interface(slot0: u64, slot1: u64) -> String {
    format_interface_with_ctx(slot0, slot1, None)
}

/// Format interface with optional ExternCallContext for error message extraction.
pub fn format_interface_with_ctx(slot0: u64, slot1: u64, call: Option<&ExternCallContext>) -> String {
    let vk = interface::unpack_value_kind(slot0);
    
    match vk {
        ValueKind::Void => "<nil>".to_string(),
        ValueKind::Bool => if slot1 != 0 { "true" } else { "false" }.to_string(),
        ValueKind::Int | ValueKind::Int64 => (slot1 as i64).to_string(),
        ValueKind::Int8 => (slot1 as i8).to_string(),
        ValueKind::Int16 => (slot1 as i16).to_string(),
        ValueKind::Int32 => (slot1 as i32).to_string(),
        ValueKind::Uint | ValueKind::Uint64 => slot1.to_string(),
        ValueKind::Uint8 => (slot1 as u8).to_string(),
        ValueKind::Uint16 => (slot1 as u16).to_string(),
        ValueKind::Uint32 => (slot1 as u32).to_string(),
        ValueKind::Float32 => f32::from_bits(slot1 as u32).to_string(),
        ValueKind::Float64 => f64::from_bits(slot1).to_string(),
        ValueKind::String => {
            str_obj::as_str(slot1 as GcRef).to_string()
        }
        ValueKind::Pointer => {
            // Check if this is an error interface
            if let Some(ctx) = call {
                let rttid = interface::unpack_rttid(slot0);
                let wk = ctx.well_known();
                if wk.error_ptr_rttid.map_or(false, |err_rttid| rttid == err_rttid) {
                    if let Some(field_offsets) = wk.error_field_offsets {
                        let ptr = slot1 as GcRef;
                        if !ptr.is_null() {
                            return format_error_chain(ptr, field_offsets, ctx);
                        }
                    }
                }
            }
            format!("0x{:x}", slot1)
        }
        ValueKind::Slice => format_slice_value(slot1 as GcRef),
        ValueKind::Map => format!("map[...]"),
        ValueKind::Channel => format!("0x{:x}", slot1),
        ValueKind::Closure => format!("0x{:x}", slot1),
        ValueKind::Array => format!("[...]"),
        ValueKind::Struct => format!("{{...}}"),
        ValueKind::Interface => format!("0x{:x}", slot1),
    }
}

/// Format error chain recursively: "msg: cause_msg: cause_cause_msg..."
/// field_offsets: [msg, cause]
fn format_error_chain(ptr: GcRef, field_offsets: [u16; 2], ctx: &ExternCallContext) -> String {
    // Read msg field
    let msg_ref = unsafe { Gc::read_slot(ptr, field_offsets[0] as usize) } as GcRef;
    let msg = if !msg_ref.is_null() {
        str_obj::as_str(msg_ref).to_string()
    } else {
        String::new()
    };
    
    // Read cause field (interface: 2 slots)
    let cause_slot0 = unsafe { Gc::read_slot(ptr, field_offsets[1] as usize) };
    let cause_slot1 = unsafe { Gc::read_slot(ptr, field_offsets[1] as usize + 1) };
    
    // Check if cause is nil (slot0 == 0 means nil interface)
    if cause_slot0 == 0 {
        return msg;
    }
    
    // Recursively format cause
    let cause_msg = format_interface_with_ctx(cause_slot0, cause_slot1, Some(ctx));
    if cause_msg.is_empty() || cause_msg == "<nil>" {
        return msg;
    }
    
    format!("{}: {}", msg, cause_msg)
}

/// Format a slice value for %v output.
fn format_slice_value(slice_ref: GcRef) -> String {
    if slice_ref.is_null() {
        return "[]".to_string();
    }
    let len = slice::len(slice_ref);
    if len == 0 {
        return "[]".to_string();
    }
    // Show first few elements for byte slices
    let elem_kind = slice::elem_kind(slice_ref);
    if elem_kind == ValueKind::Uint8 {
        let data = slice::data_ptr(slice_ref);
        let bytes = unsafe { core::slice::from_raw_parts(data, len.min(8)) };
        if len <= 8 {
            return format!("{:?}", bytes);
        } else {
            return format!("{:?}...", bytes);
        }
    }
    format!("[len={}]", len)
}
