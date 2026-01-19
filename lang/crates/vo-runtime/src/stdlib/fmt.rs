//! fmt package native function implementations.
//!
//! Provides print and format functions for the fmt standard library package.
//!
//! Native layer handles:
//! - nativeSprint: format []interface{} with default format (space-separated)
//! - nativeSprintln: format []interface{} with newline
//! - nativeSprintf: format with format string
//! - nativeWrite: output string to stdout/buffer
//!
//! Vo layer (fmt.vo) provides Print, Println, Printf, Sprint, Sprintln, Sprintf
//! which call these native functions.

#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::{format, vec::Vec};

use vo_common_core::types::ValueKind;
use vo_ffi_macro::vostd_extern_ctx_nostd;
use crate::ffi::{ExternCallContext, ExternResult};
use crate::gc::GcRef;
use crate::objects::{interface, slice, string};

// =============================================================================
// Format interface{} values
// =============================================================================

/// Format a single interface{} value (2 slots) to string.
fn format_interface(slot0: u64, slot1: u64) -> String {
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
            string::as_str(slot1 as GcRef).to_string()
        }
        ValueKind::Pointer => format!("0x{:x}", slot1),
        ValueKind::Slice => format_slice_value(slot1 as GcRef),
        ValueKind::Map => format!("map[...]"),
        ValueKind::Channel => format!("0x{:x}", slot1),
        ValueKind::Closure => format!("0x{:x}", slot1),
        ValueKind::Array => format!("[...]"),
        ValueKind::Struct => format!("{{...}}"),
        ValueKind::Interface => format!("0x{:x}", slot1),
    }
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

/// Format all elements in a []interface{} slice with space separator.
fn format_args_slice(slice_ref: GcRef) -> String {
    if slice_ref.is_null() {
        return String::new();
    }
    
    let len = slice::len(slice_ref);
    if len == 0 {
        return String::new();
    }
    
    let data_ptr = slice::data_ptr(slice_ref) as *const u64;
    let mut result = String::new();
    
    for i in 0..len {
        if i > 0 {
            result.push(' ');
        }
        // Each interface{} is 2 slots (16 bytes)
        let slot0 = unsafe { *data_ptr.add(i * 2) };
        let slot1 = unsafe { *data_ptr.add(i * 2 + 1) };
        result.push_str(&format_interface(slot0, slot1));
    }
    
    result
}

// =============================================================================
// Printf format string parsing and formatting
// =============================================================================

/// Format with printf-style format string.
fn sprintf_impl(format_str: &str, args_ref: GcRef) -> String {
    let args_len = if args_ref.is_null() { 0 } else { slice::len(args_ref) };
    let data_ptr = if args_ref.is_null() { 
        core::ptr::null() 
    } else { 
        slice::data_ptr(args_ref) as *const u64 
    };
    
    let mut result = String::new();
    let mut chars = format_str.chars().peekable();
    let mut arg_idx = 0usize;
    
    while let Some(c) = chars.next() {
        if c != '%' {
            result.push(c);
            continue;
        }
        
        // Parse format specifier
        match chars.next() {
            None => {
                result.push('%');
                break;
            }
            Some('%') => {
                result.push('%');
            }
            Some(spec) => {
                if arg_idx >= args_len {
                    result.push_str("%!");
                    result.push(spec);
                    result.push_str("(MISSING)");
                } else {
                    let slot0 = unsafe { *data_ptr.add(arg_idx * 2) };
                    let slot1 = unsafe { *data_ptr.add(arg_idx * 2 + 1) };
                    result.push_str(&format_with_verb(spec, slot0, slot1));
                    arg_idx += 1;
                }
            }
        }
    }
    
    result
}

/// Format a value with a specific format verb.
fn format_with_verb(verb: char, slot0: u64, slot1: u64) -> String {
    let vk = interface::unpack_value_kind(slot0);
    
    match verb {
        'v' => format_interface(slot0, slot1),
        'd' => {
            if vk.is_integer() {
                (slot1 as i64).to_string()
            } else {
                format!("%!d({})", format_interface(slot0, slot1))
            }
        }
        's' => {
            match vk {
                ValueKind::String => string::as_str(slot1 as GcRef).to_string(),
                _ => format_interface(slot0, slot1),
            }
        }
        'f' => {
            match vk {
                ValueKind::Float32 => format!("{}", f32::from_bits(slot1 as u32)),
                ValueKind::Float64 => format!("{}", f64::from_bits(slot1)),
                _ => format!("%!f({})", format_interface(slot0, slot1)),
            }
        }
        't' => {
            match vk {
                ValueKind::Bool => if slot1 != 0 { "true" } else { "false" }.to_string(),
                _ => format!("%!t({})", format_interface(slot0, slot1)),
            }
        }
        'x' => {
            match vk {
                ValueKind::Int | ValueKind::Int64 | ValueKind::Uint | ValueKind::Uint64 => {
                    format!("{:x}", slot1)
                }
                ValueKind::Int8 | ValueKind::Uint8 => format!("{:x}", slot1 as u8),
                ValueKind::Int16 | ValueKind::Uint16 => format!("{:x}", slot1 as u16),
                ValueKind::Int32 | ValueKind::Uint32 => format!("{:x}", slot1 as u32),
                ValueKind::String => {
                    let s = string::as_str(slot1 as GcRef);
                    s.bytes().map(|b| format!("{:02x}", b)).collect()
                }
                _ => format!("%!x({})", format_interface(slot0, slot1)),
            }
        }
        'X' => {
            match vk {
                ValueKind::Int | ValueKind::Int64 | ValueKind::Uint | ValueKind::Uint64 => {
                    format!("{:X}", slot1)
                }
                ValueKind::Int8 | ValueKind::Uint8 => format!("{:X}", slot1 as u8),
                ValueKind::Int16 | ValueKind::Uint16 => format!("{:X}", slot1 as u16),
                ValueKind::Int32 | ValueKind::Uint32 => format!("{:X}", slot1 as u32),
                ValueKind::String => {
                    let s = string::as_str(slot1 as GcRef);
                    s.bytes().map(|b| format!("{:02X}", b)).collect()
                }
                _ => format!("%!X({})", format_interface(slot0, slot1)),
            }
        }
        'p' => format!("0x{:x}", slot1),
        'q' => {
            match vk {
                ValueKind::String => {
                    let s = string::as_str(slot1 as GcRef);
                    format!("{:?}", s)
                }
                _ => format!("%!q({})", format_interface(slot0, slot1)),
            }
        }
        _ => format!("%!{}({})", verb, format_interface(slot0, slot1)),
    }
}

// =============================================================================
// Native extern functions
// =============================================================================

/// nativeWrite - write string to output (uses output.rs for std/no_std)
#[vostd_extern_ctx_nostd("fmt", "nativeWrite")]
fn native_write(call: &mut ExternCallContext) -> ExternResult {
    let s = call.arg_str(slots::ARG_S);
    crate::output::write(s);
    ExternResult::Ok
}

/// nativeSprint - format []interface{} with default format
#[vostd_extern_ctx_nostd("fmt", "nativeSprint")]
fn native_sprint(call: &mut ExternCallContext) -> ExternResult {
    let args_ref = call.arg_ref(slots::ARG_A);
    let formatted = format_args_slice(args_ref);
    let gc = call.gc();
    let s = string::from_rust_str(gc, &formatted);
    call.ret_ref(0, s);
    ExternResult::Ok
}

/// nativeSprintln - format []interface{} with newline
#[vostd_extern_ctx_nostd("fmt", "nativeSprintln")]
fn native_sprintln(call: &mut ExternCallContext) -> ExternResult {
    let args_ref = call.arg_ref(slots::ARG_A);
    let mut formatted = format_args_slice(args_ref);
    formatted.push('\n');
    let gc = call.gc();
    let s = string::from_rust_str(gc, &formatted);
    call.ret_ref(0, s);
    ExternResult::Ok
}

/// nativeSprintf - format with format string
#[vostd_extern_ctx_nostd("fmt", "nativeSprintf")]
fn native_sprintf(call: &mut ExternCallContext) -> ExternResult {
    let format_str = call.arg_str(slots::ARG_FORMAT);
    let args_ref = call.arg_ref(slots::ARG_A);
    let formatted = sprintf_impl(format_str, args_ref);
    let gc = call.gc();
    let s = string::from_rust_str(gc, &formatted);
    call.ret_ref(0, s);
    ExternResult::Ok
}

crate::stdlib_register!(fmt: nativeWrite, nativeSprint, nativeSprintln, nativeSprintf);
