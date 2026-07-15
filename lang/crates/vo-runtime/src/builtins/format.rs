//! Value formatting utilities.
//!
//! Used by builtin print/println and fmt package.

use crate::ffi::ExternCallContext;
use crate::gc::{Gc, GcRef};
use crate::objects::{interface, slice, string as str_obj};
use vo_common_core::types::ValueKind;

#[cfg(not(feature = "std"))]
extern crate alloc;

#[cfg(not(feature = "std"))]
use alloc::collections::BTreeSet;
#[cfg(not(feature = "std"))]
use alloc::format;
#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

#[cfg(feature = "std")]
use std::collections::BTreeSet;
#[cfg(feature = "std")]
use std::string::{String, ToString};
#[cfg(feature = "std")]
use std::vec::Vec;

/// Format a value based on its kind while preserving arbitrary string bytes.
pub fn format_value_bytes(val: u64, kind: ValueKind) -> Vec<u8> {
    match kind {
        ValueKind::Void => b"nil".to_vec(),
        ValueKind::Bool => {
            if val != 0 {
                b"true".to_vec()
            } else {
                b"false".to_vec()
            }
        }
        ValueKind::Int | ValueKind::Int64 => (val as i64).to_string().into_bytes(),
        ValueKind::Int8 => (val as i8).to_string().into_bytes(),
        ValueKind::Int16 => (val as i16).to_string().into_bytes(),
        ValueKind::Int32 => (val as i32).to_string().into_bytes(),
        ValueKind::Uint | ValueKind::Uint64 => val.to_string().into_bytes(),
        ValueKind::Uint8 => (val as u8).to_string().into_bytes(),
        ValueKind::Uint16 => (val as u16).to_string().into_bytes(),
        ValueKind::Uint32 => (val as u32).to_string().into_bytes(),
        ValueKind::Float32 => f32::from_bits(val as u32).to_string().into_bytes(),
        ValueKind::Float64 => f64::from_bits(val).to_string().into_bytes(),
        ValueKind::String => {
            let ptr = val as GcRef;
            if ptr.is_null() {
                Vec::new()
            } else {
                // Safety: `kind` identifies `val` as a live VM string.
                unsafe { str_obj::to_bytes(ptr) }
            }
        }
        ValueKind::Slice => b"[...]".to_vec(),
        ValueKind::Array => b"[...]".to_vec(),
        ValueKind::Map => b"map[...]".to_vec(),
        ValueKind::Struct => b"{...}".to_vec(),
        ValueKind::Pointer => format!("0x{:x}", val).into_bytes(),
        ValueKind::Interface => b"<interface>".to_vec(),
        ValueKind::Channel => b"<chan>".to_vec(),
        ValueKind::Port => b"<port>".to_vec(),
        ValueKind::Closure => b"<func>".to_vec(),
        ValueKind::Island => b"<island>".to_vec(),
    }
}

/// Diagnostic text view of [`format_value_bytes`]. Malformed bytes are escaped.
pub fn format_value(val: u64, kind: ValueKind) -> String {
    crate::output::render_output_text(&format_value_bytes(val, kind))
}

/// Format a single interface{} value (2 slots) to string.
/// For error interface, extracts the error message.
pub fn format_interface(slot0: u64, slot1: u64) -> String {
    crate::output::render_output_text(&format_interface_bytes(slot0, slot1))
}

/// Format a single interface value while preserving arbitrary string bytes.
pub fn format_interface_bytes(slot0: u64, slot1: u64) -> Vec<u8> {
    format_interface_bytes_with_ctx(slot0, slot1, None)
}

/// Format interface bytes with optional context for error message extraction.
pub fn format_interface_bytes_with_ctx(
    slot0: u64,
    slot1: u64,
    call: Option<&ExternCallContext>,
) -> Vec<u8> {
    let Some(vk) = interface::try_unpack_value_kind(slot0) else {
        return format!("<invalid-interface-kind:{}>", slot0 as u8).into_bytes();
    };

    match vk {
        ValueKind::Void => b"<nil>".to_vec(),
        ValueKind::Bool => {
            if slot1 != 0 {
                b"true".to_vec()
            } else {
                b"false".to_vec()
            }
        }
        ValueKind::Int | ValueKind::Int64 => (slot1 as i64).to_string().into_bytes(),
        ValueKind::Int8 => (slot1 as i8).to_string().into_bytes(),
        ValueKind::Int16 => (slot1 as i16).to_string().into_bytes(),
        ValueKind::Int32 => (slot1 as i32).to_string().into_bytes(),
        ValueKind::Uint | ValueKind::Uint64 => slot1.to_string().into_bytes(),
        ValueKind::Uint8 => (slot1 as u8).to_string().into_bytes(),
        ValueKind::Uint16 => (slot1 as u16).to_string().into_bytes(),
        ValueKind::Uint32 => (slot1 as u32).to_string().into_bytes(),
        ValueKind::Float32 => f32::from_bits(slot1 as u32).to_string().into_bytes(),
        ValueKind::Float64 => f64::from_bits(slot1).to_string().into_bytes(),
        ValueKind::String => {
            // Safety: the interface metadata identifies `slot1` as a live string.
            unsafe { str_obj::to_bytes(slot1 as GcRef) }
        }
        ValueKind::Pointer => {
            // Check if this is an error interface
            if let Some(ctx) = call {
                let rttid = interface::unpack_rttid(slot0);
                let wk = ctx.well_known();
                if wk
                    .error_ptr_rttid
                    .is_some_and(|err_rttid| rttid == err_rttid)
                {
                    if let Some(field_offsets) = wk.error_field_offsets {
                        let ptr = slot1 as GcRef;
                        if !ptr.is_null() {
                            return format_error_chain(ptr, field_offsets, ctx);
                        }
                    }
                }
            }
            format!("0x{:x}", slot1).into_bytes()
        }
        ValueKind::Slice => {
            // Safety: the interface tag identifies slot1 as a rooted slice.
            unsafe { format_slice_value(slot1 as GcRef) }
        }
        ValueKind::Map => b"map[...]".to_vec(),
        ValueKind::Channel => format!("0x{:x}", slot1).into_bytes(),
        ValueKind::Port => format!("0x{:x}", slot1).into_bytes(),
        ValueKind::Closure => format!("0x{:x}", slot1).into_bytes(),
        ValueKind::Array => b"[...]".to_vec(),
        ValueKind::Struct => b"{...}".to_vec(),
        ValueKind::Interface => format!("0x{:x}", slot1).into_bytes(),
        ValueKind::Island => b"<island>".to_vec(),
    }
}

/// Diagnostic text view of [`format_interface_bytes_with_ctx`].
pub fn format_interface_with_ctx(
    slot0: u64,
    slot1: u64,
    call: Option<&ExternCallContext>,
) -> String {
    crate::output::render_output_text(&format_interface_bytes_with_ctx(slot0, slot1, call))
}

/// Format an error chain with an explicit work loop.
/// field_offsets: [msg, cause]
fn format_error_chain(mut ptr: GcRef, field_offsets: [u16; 2], ctx: &ExternCallContext) -> Vec<u8> {
    let mut seen = BTreeSet::new();
    let mut parts = Vec::new();

    loop {
        if !seen.insert(ptr as usize) {
            parts.push(b"<cycle>".to_vec());
            break;
        }

        let msg_ref = unsafe { Gc::read_slot(ptr, field_offsets[0] as usize) } as GcRef;
        parts.push(if msg_ref.is_null() {
            Vec::new()
        } else {
            // Safety: the well-known error layout identifies this field as a string.
            unsafe { str_obj::to_bytes(msg_ref) }
        });

        let cause_offset = field_offsets[1] as usize;
        let cause_slot0 = unsafe { Gc::read_slot(ptr, cause_offset) };
        let cause_slot1 = unsafe { Gc::read_slot(ptr, cause_offset + 1) };
        if cause_slot0 == 0 {
            break;
        }

        let cause_is_error = interface::try_unpack_value_kind(cause_slot0)
            == Some(ValueKind::Pointer)
            && ctx
                .well_known()
                .error_ptr_rttid
                .is_some_and(|rttid| interface::unpack_rttid(cause_slot0) == rttid)
            && cause_slot1 != 0;
        if cause_is_error {
            ptr = cause_slot1 as GcRef;
            continue;
        }

        let cause_msg = format_interface_bytes_with_ctx(cause_slot0, cause_slot1, Some(ctx));
        if !cause_msg.is_empty() && cause_msg != b"<nil>" {
            parts.push(cause_msg);
        }
        break;
    }

    let capacity = parts.iter().map(Vec::len).sum::<usize>() + parts.len().saturating_sub(1) * 2;
    let mut result = Vec::with_capacity(capacity);
    for (index, part) in parts.into_iter().enumerate() {
        if index > 0 {
            result.extend_from_slice(b": ");
        }
        result.extend_from_slice(&part);
    }
    result
}

/// Format a slice value for %v output.
unsafe fn format_slice_value(slice_ref: GcRef) -> Vec<u8> {
    if slice_ref.is_null() {
        return b"[]".to_vec();
    }
    let len = slice::len(slice_ref);
    if len == 0 {
        return b"[]".to_vec();
    }
    // Show first few elements for byte slices
    let elem_kind = slice::elem_kind(slice_ref);
    if elem_kind == ValueKind::Uint8 {
        let bytes = unsafe { slice::byte_vec(slice_ref) };
        let preview = &bytes[..len.min(8)];
        if len <= 8 {
            return format!("{:?}", preview).into_bytes();
        } else {
            return format!("{:?}...", preview).into_bytes();
        }
    }
    format!("[len={}]", len).into_bytes()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn invalid_interface_kind_is_rendered_without_panicking() {
        assert_eq!(format_interface(0xff, 0), "<invalid-interface-kind:255>");
    }

    #[test]
    fn string_formatting_preserves_invalid_utf8_bytes() {
        let mut gc = Gc::new();
        let string = str_obj::create(&mut gc, b"a\xffz");
        assert_eq!(
            format_value_bytes(string as u64, ValueKind::String),
            b"a\xffz"
        );
    }
}
