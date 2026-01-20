//! JSON struct serialization/deserialization native implementations.

#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;
#[cfg(not(feature = "std"))]
use alloc::borrow::Cow;
#[cfg(not(feature = "std"))]
use alloc::format;

#[cfg(feature = "std")]
use std::borrow::Cow;

use vo_common_core::types::ValueKind;
use vo_common_core::runtime_type::RuntimeType;

use crate::ffi::{ExternCallContext, ExternResult};
use crate::gc::GcRef;
use crate::objects::{interface, string as str_obj};
use crate::slot::SLOT_BYTES;
use super::error_helper::write_error_to;
use super::tag::{get_tag_value, parse_field_options};

use vo_ffi_macro::vostd_extern_ctx_nostd;

#[vostd_extern_ctx_nostd("json", "MarshalStruct")]
fn marshal_struct(call: &mut ExternCallContext) -> ExternResult {
    let v_slot0 = call.arg_u64(0);
    let v_slot1 = call.arg_u64(1);
    
    let vk = interface::unpack_value_kind(v_slot0);
    let rttid = interface::unpack_rttid(v_slot0);
    
    let mut buf = Vec::new();
    
    let result = match vk {
        ValueKind::Struct => marshal_struct_value(call, v_slot1 as GcRef, rttid, &mut buf),
        ValueKind::Pointer => {
            let ptr = v_slot1 as GcRef;
            let elem_rttid = call.get_elem_value_rttid_from_base(rttid).rttid();
            marshal_struct_value(call, ptr, elem_rttid, &mut buf)
        }
        _ => marshal_any_value(v_slot0, v_slot1, &mut buf),
    };
    
    match result {
        Ok(()) => {
            let slice_ref = call.alloc_bytes(&buf);
            call.ret_ref(0, slice_ref);  // []byte is 1 slot (GcRef)
            call.ret_nil(1);  // error slot0
            call.ret_nil(2);  // error slot1
            ExternResult::Ok
        }
        Err(msg) => {
            call.ret_nil(0);  // nil slice
            write_error_to(call, 1, call.dyn_err().type_mismatch, msg);
            ExternResult::Ok
        }
    }
}

fn get_pointed_type_rttid(call: &ExternCallContext, ptr_rttid: u32) -> u32 {
    call.get_elem_value_rttid_from_base(ptr_rttid).rttid()
}

const MAX_DEPTH: usize = 64;

fn marshal_struct_value(
    call: &ExternCallContext,
    ptr: GcRef,
    rttid: u32,
    buf: &mut Vec<u8>,
) -> Result<(), &'static str> {
    marshal_struct_value_depth(call, ptr, rttid, buf, 0)
}

fn marshal_struct_value_depth(
    call: &ExternCallContext,
    ptr: GcRef,
    rttid: u32,
    buf: &mut Vec<u8>,
    depth: usize,
) -> Result<(), &'static str> {
    if depth > MAX_DEPTH {
        return Err("max depth exceeded (possible cycle)");
    }
    
    buf.push(b'{');
    let mut first = true;
    marshal_fields_into(call, ptr, rttid, buf, &mut first, depth)?;
    buf.push(b'}');
    Ok(())
}

/// Marshal struct fields into buffer, handling embedded fields by flattening.
fn marshal_fields_into(
    call: &ExternCallContext,
    ptr: GcRef,
    rttid: u32,
    buf: &mut Vec<u8>,
    first: &mut bool,
    depth: usize,
) -> Result<(), &'static str> {
    let struct_meta_id = get_struct_meta_id(call, rttid)?;
    let struct_meta = call.struct_meta(struct_meta_id as usize).ok_or("struct meta not found")?;
    
    for field in &struct_meta.fields {
        // Skip unexported fields (lowercase first char)
        if field.name.chars().next().map(|c| c.is_lowercase()).unwrap_or(true) { continue; }
        
        let json_name = get_json_field_name(&field.name, field.tag.as_deref());
        if json_name == "-" { continue; }
        
        let field_ptr = unsafe { (ptr as *const u8).add(field.offset as usize * SLOT_BYTES) };
        
        if field.embedded {
            marshal_fields_into(call, field_ptr as GcRef, field.type_info.rttid(), buf, first, depth)?;
        } else {
            if !*first { buf.push(b','); }
            *first = false;
            
            buf.push(b'"');
            buf.extend_from_slice(json_name.as_bytes());
            buf.push(b'"');
            buf.push(b':');
            
            marshal_field_value_depth(call, ptr, field.offset as usize, field.type_info.value_kind(), field.type_info.rttid(), buf, depth)?;
        }
    }
    Ok(())
}

/// Get JSON field name from tag or use default conversion (lowercase first char).
/// Returns "-" if field should be skipped.
fn get_json_field_name<'a>(field_name: &'a str, tag: Option<&str>) -> Cow<'a, str> {
    if let Some(tag) = tag {
        if let Some(json_value) = get_tag_value(tag, "json") {
            let (name, _omitempty) = parse_field_options(json_value);
            if !name.is_empty() {
                return Cow::Owned(name.to_string());
            }
        }
    }
    // Default: lowercase first char
    let mut chars = field_name.chars();
    match chars.next() {
        Some(c) if c.is_uppercase() => Cow::Owned(c.to_lowercase().collect::<String>() + chars.as_str()),
        _ => Cow::Borrowed(field_name),
    }
}

fn marshal_field_value_depth(
    call: &ExternCallContext,
    struct_ptr: GcRef,
    field_offset: usize,
    vk: ValueKind,
    rttid: u32,
    buf: &mut Vec<u8>,
    depth: usize,
) -> Result<(), &'static str> {
    let base = struct_ptr as *const u8;
    let field_ptr = unsafe { base.add(field_offset * SLOT_BYTES) };
    
    match vk {
        ValueKind::Int | ValueKind::Int64 => {
            let val = unsafe { *(field_ptr as *const i64) };
            buf.extend_from_slice(format!("{}", val).as_bytes());
        }
        ValueKind::Int32 => {
            let val = unsafe { *(field_ptr as *const i32) };
            buf.extend_from_slice(format!("{}", val).as_bytes());
        }
        ValueKind::Float64 => {
            let val = unsafe { *(field_ptr as *const f64) };
            if val.is_nan() || val.is_infinite() { return Err("NaN/Infinity not supported"); }
            buf.extend_from_slice(format!("{}", val).as_bytes());
        }
        ValueKind::Bool => {
            let val = unsafe { *(field_ptr as *const u8) } != 0;
            buf.extend_from_slice(if val { b"true" } else { b"false" });
        }
        ValueKind::String => {
            let str_ref = unsafe { *(field_ptr as *const u64) } as GcRef;
            if str_ref.is_null() { buf.extend_from_slice(b"\"\""); }
            else { write_json_string(str_obj::as_str(str_ref), buf); }
        }
        ValueKind::Struct => {
            // Inline struct: field_ptr IS the struct data (not a pointer to it)
            marshal_struct_value_depth(call, field_ptr as GcRef, rttid, buf, depth + 1)?;
        }
        ValueKind::Pointer => {
            let ptr_val = unsafe { *(field_ptr as *const u64) } as GcRef;
            if ptr_val.is_null() { buf.extend_from_slice(b"null"); }
            else {
                let elem_rttid = get_pointed_type_rttid(call, rttid);
                marshal_struct_value_depth(call, ptr_val, elem_rttid, buf, depth + 1)?;
            }
        }
        ValueKind::Interface => {
            let s0 = unsafe { *(field_ptr as *const u64) };
            let s1 = unsafe { *((field_ptr as *const u64).add(1)) };
            marshal_any_value(s0, s1, buf)?;
        }
        _ => buf.extend_from_slice(b"null"),
    }
    Ok(())
}

fn marshal_any_value(slot0: u64, slot1: u64, buf: &mut Vec<u8>) -> Result<(), &'static str> {
    let vk = interface::unpack_value_kind(slot0);
    match vk {
        ValueKind::Void => buf.extend_from_slice(b"null"),
        ValueKind::Int | ValueKind::Int64 => buf.extend_from_slice(format!("{}", slot1 as i64).as_bytes()),
        ValueKind::Float64 => {
            let val = f64::from_bits(slot1);
            if val.is_nan() || val.is_infinite() { return Err("NaN/Infinity not supported"); }
            buf.extend_from_slice(format!("{}", val).as_bytes());
        }
        ValueKind::Bool => buf.extend_from_slice(if slot1 != 0 { b"true" } else { b"false" }),
        ValueKind::String => {
            let str_ref = slot1 as GcRef;
            if str_ref.is_null() { buf.extend_from_slice(b"\"\""); }
            else { write_json_string(str_obj::as_str(str_ref), buf); }
        }
        _ => buf.extend_from_slice(b"null"),
    }
    Ok(())
}

fn write_json_string(s: &str, buf: &mut Vec<u8>) {
    write_json_string_impl(s, buf, false);
}

fn write_json_string_impl(s: &str, buf: &mut Vec<u8>, escape_html: bool) {
    buf.push(b'"');
    for c in s.chars() {
        match c {
            '"' => buf.extend_from_slice(b"\\\""),
            '\\' => buf.extend_from_slice(b"\\\\"),
            '\n' => buf.extend_from_slice(b"\\n"),
            '\r' => buf.extend_from_slice(b"\\r"),
            '\t' => buf.extend_from_slice(b"\\t"),
            '\x08' => buf.extend_from_slice(b"\\b"),
            '\x0c' => buf.extend_from_slice(b"\\f"),
            '<' if escape_html => buf.extend_from_slice(b"\\u003c"),
            '>' if escape_html => buf.extend_from_slice(b"\\u003e"),
            '&' if escape_html => buf.extend_from_slice(b"\\u0026"),
            c if c < ' ' => buf.extend_from_slice(format!("\\u{:04x}", c as u32).as_bytes()),
            c => { let mut t = [0u8; 4]; buf.extend_from_slice(c.encode_utf8(&mut t).as_bytes()); }
        }
    }
    buf.push(b'"');
}

fn get_struct_meta_id(call: &ExternCallContext, rttid: u32) -> Result<u32, &'static str> {
    let rts = call.runtime_types();
    let rt = rts.get(rttid as usize).ok_or("type not found")?;
    match rt {
        RuntimeType::Struct { meta_id, .. } => Ok(*meta_id),
        RuntimeType::Named { struct_meta_id: Some(id), .. } => Ok(*id),
        _ => Err("not a struct type"),
    }
}

#[vostd_extern_ctx_nostd("json", "UnmarshalStruct")]
fn unmarshal_struct(call: &mut ExternCallContext) -> ExternResult {
    // Args: data []byte (1 slot), v any (2 slots)
    // Copy data to owned string to avoid borrow issues
    let json_str = {
        let data = call.arg_bytes(0);
        if data.is_empty() {
            write_error_to(call, 0, call.dyn_err().type_mismatch, "empty JSON");
            return ExternResult::Ok;
        }
        match core::str::from_utf8(data) {
            Ok(s) => s.to_string(),
            Err(_) => {
                write_error_to(call, 0, call.dyn_err().type_mismatch, "invalid UTF-8");
                return ExternResult::Ok;
            }
        }
    };
    
    let v_slot0 = call.arg_u64(1); // any slot0
    let v_slot1 = call.arg_u64(2); // any slot1
    
    let vk = interface::unpack_value_kind(v_slot0);
    let rttid = interface::unpack_rttid(v_slot0);
    
    if vk != ValueKind::Pointer {
        write_error_to(call, 0, call.dyn_err().type_mismatch, "target must be pointer");
        return ExternResult::Ok;
    }
    
    let ptr = v_slot1 as GcRef;
    if ptr.is_null() {
        write_error_to(call, 0, call.dyn_err().nil_base, "nil pointer");
        return ExternResult::Ok;
    }
    
    let pointed_rttid = get_pointed_type_rttid(call, rttid);
    
    match unmarshal_struct_impl(call, ptr, pointed_rttid, json_str.trim()) {
        Ok(()) => { call.ret_nil(0); call.ret_nil(1); }
        Err(msg) => write_error_to(call, 0, call.dyn_err().type_mismatch, msg),
    }
    ExternResult::Ok
}

fn unmarshal_struct_impl(call: &mut ExternCallContext, ptr: GcRef, rttid: u32, json: &str) -> Result<(), &'static str> {
    let struct_meta_id = get_struct_meta_id(call, rttid)?;
    
    let json = json.trim();
    if !json.starts_with('{') || !json.ends_with('}') { return Err("expected object"); }
    
    let inner = json[1..json.len()-1].trim();
    if inner.is_empty() { return Ok(()); }
    
    let mut pos = 0;
    let bytes = inner.as_bytes();
    
    while pos < bytes.len() {
        while pos < bytes.len() && is_ws(bytes[pos]) { pos += 1; }
        if pos >= bytes.len() { break; }
        
        if bytes[pos] != b'"' { return Err("expected key"); }
        pos += 1;
        let key_start = pos;
        while pos < bytes.len() && bytes[pos] != b'"' {
            if bytes[pos] == b'\\' { pos += 2; } else { pos += 1; }
        }
        if pos >= bytes.len() { return Err("unterminated key"); }
        let key = &inner[key_start..pos];
        pos += 1;
        
        while pos < bytes.len() && is_ws(bytes[pos]) { pos += 1; }
        if pos >= bytes.len() || bytes[pos] != b':' { return Err("expected colon"); }
        pos += 1;
        while pos < bytes.len() && is_ws(bytes[pos]) { pos += 1; }
        
        let val_start = pos;
        let val_end = find_val_end(bytes, pos)?;
        let value = inner[val_start..val_end].trim();
        pos = val_end;
        
        // Try to find field by JSON key (including in embedded structs)
        if let Some((field_ptr, fvk, field_rttid)) = find_field_by_json_key(call, ptr, struct_meta_id, key)? {
            unmarshal_field_value(call, field_ptr, fvk, field_rttid, value)?;
        }
        
        while pos < bytes.len() && is_ws(bytes[pos]) { pos += 1; }
        if pos < bytes.len() && bytes[pos] == b',' { pos += 1; }
    }
    Ok(())
}

/// Find a field by JSON key, recursively searching embedded structs.
/// Direct fields take precedence over embedded fields.
fn find_field_by_json_key(
    call: &ExternCallContext,
    ptr: GcRef,
    struct_meta_id: u32,
    json_key: &str,
) -> Result<Option<(GcRef, ValueKind, u32)>, &'static str> {
    let struct_meta = call.struct_meta(struct_meta_id as usize).ok_or("meta not found")?;
    
    // Collect embedded fields for later search (direct fields have priority)
    let mut embedded_fields = Vec::new();
    
    for field in &struct_meta.fields {
        let field_ptr = unsafe { (ptr as *const u8).add(field.offset as usize * SLOT_BYTES) };
        
        if field.embedded {
            embedded_fields.push((field_ptr as GcRef, field.type_info.rttid()));
        } else {
            let field_json_name = get_json_field_name(&field.name, field.tag.as_deref());
            if field_json_name == "-" { continue; }
            if field_json_name == json_key {
                return Ok(Some((field_ptr as GcRef, field.type_info.value_kind(), field.type_info.rttid())));
            }
        }
    }
    
    // Search in embedded fields
    for (embed_ptr, embed_rttid) in embedded_fields {
        let embed_meta_id = get_struct_meta_id(call, embed_rttid)?;
        if let Some(result) = find_field_by_json_key(call, embed_ptr, embed_meta_id, json_key)? {
            return Ok(Some(result));
        }
    }
    
    Ok(None)
}

fn find_val_end(bytes: &[u8], start: usize) -> Result<usize, &'static str> {
    let mut pos = start;
    if pos >= bytes.len() { return Err("unexpected end"); }
    match bytes[pos] {
        b'"' => {
            pos += 1;
            while pos < bytes.len() && bytes[pos] != b'"' {
                if bytes[pos] == b'\\' { pos += 2; } else { pos += 1; }
            }
            if pos >= bytes.len() { return Err("unterminated string"); }
            Ok(pos + 1)
        }
        b'{' | b'[' => {
            let (open, close) = if bytes[pos] == b'{' { (b'{', b'}') } else { (b'[', b']') };
            let mut depth = 1;
            pos += 1;
            while pos < bytes.len() && depth > 0 {
                if bytes[pos] == open { depth += 1; }
                else if bytes[pos] == close { depth -= 1; }
                else if bytes[pos] == b'"' {
                    pos += 1;
                    while pos < bytes.len() && bytes[pos] != b'"' {
                        if bytes[pos] == b'\\' { pos += 1; }
                        pos += 1;
                    }
                }
                pos += 1;
            }
            Ok(pos)
        }
        _ => {
            while pos < bytes.len() && !is_ws(bytes[pos]) && bytes[pos] != b',' && bytes[pos] != b'}' && bytes[pos] != b']' { pos += 1; }
            Ok(pos)
        }
    }
}

fn unmarshal_field_value(call: &mut ExternCallContext, field_ptr: GcRef, vk: ValueKind, rttid: u32, value: &str) -> Result<(), &'static str> {
    let field_ptr = field_ptr as *mut u8;
    match vk {
        ValueKind::Int | ValueKind::Int64 => {
            let val: i64 = value.parse().map_err(|_| "invalid int")?;
            unsafe { *(field_ptr as *mut i64) = val; }
        }
        ValueKind::Int32 => {
            let val: i32 = value.parse().map_err(|_| "invalid int")?;
            unsafe { *(field_ptr as *mut i32) = val; }
        }
        ValueKind::Float64 => {
            let val: f64 = value.parse().map_err(|_| "invalid float")?;
            unsafe { *(field_ptr as *mut f64) = val; }
        }
        ValueKind::Bool => {
            let val = match value { "true" => true, "false" => false, _ => return Err("invalid bool") };
            unsafe { *(field_ptr as *mut u8) = val as u8; }
        }
        ValueKind::String => {
            if value == "null" { unsafe { *(field_ptr as *mut u64) = 0; } }
            else if value.starts_with('"') && value.ends_with('"') {
                let s = parse_json_str(&value[1..value.len()-1])?;
                let str_ref = call.alloc_str(&s);
                unsafe { *(field_ptr as *mut u64) = str_ref as u64; }
            } else { return Err("expected string"); }
        }
        ValueKind::Struct => {
            // Inline struct: unmarshal directly into field_ptr
            if value == "null" { return Ok(()); }
            unmarshal_struct_impl(call, field_ptr as GcRef, rttid, value)?;
        }
        ValueKind::Pointer => {
            if value == "null" {
                unsafe { *(field_ptr as *mut u64) = 0; }
            } else {
                // Get pointed-to type info
                let elem_rttid = get_pointed_type_rttid(call, rttid);
                let elem_meta_id = get_struct_meta_id(call, elem_rttid)?;
                let elem_meta = call.struct_meta(elem_meta_id as usize).ok_or("elem meta not found")?;
                let slot_count = elem_meta.slot_count();
                
                // Allocate struct on heap
                let new_struct = call.gc_alloc(slot_count, &[]);
                
                // Unmarshal into newly allocated struct
                unmarshal_struct_impl(call, new_struct, elem_rttid, value)?;
                
                // Store pointer
                unsafe { *(field_ptr as *mut u64) = new_struct as u64; }
            }
        }
        _ => {}
    }
    Ok(())
}

fn parse_json_str(s: &str) -> Result<String, &'static str> {
    let mut result = String::new();
    let mut chars = s.chars().peekable();
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('"') => result.push('"'),
                Some('\\') => result.push('\\'),
                Some('n') => result.push('\n'),
                Some('r') => result.push('\r'),
                Some('t') => result.push('\t'),
                Some('u') => {
                    let mut hex = String::new();
                    for _ in 0..4 { hex.push(chars.next().ok_or("invalid escape")?); }
                    let code = u32::from_str_radix(&hex, 16).map_err(|_| "invalid escape")?;
                    result.push(char::from_u32(code).ok_or("invalid code")?);
                }
                _ => return Err("invalid escape"),
            }
        } else { result.push(c); }
    }
    Ok(result)
}

#[inline]
fn is_ws(c: u8) -> bool { c == b' ' || c == b'\t' || c == b'\n' || c == b'\r' }

// ==================== Shared String Functions ====================

/// writeJsonString(buf []byte, s string, escapeHTML bool) []byte
#[vostd_extern_ctx_nostd("json", "writeJsonString")]
fn write_json_string_extern(call: &mut ExternCallContext) -> ExternResult {
    // Args: buf []byte (1 slot), s string (1 slot), escapeHTML bool (1 slot)
    let buf_ref = call.arg_ref(0);
    let s = call.arg_str(1);
    let escape_html = call.arg_bool(2);
    
    // Read existing buffer content
    let mut buf: Vec<u8> = if buf_ref.is_null() {
        Vec::new()
    } else {
        let data = crate::objects::slice::data_ptr(buf_ref);
        let len = crate::objects::slice::len(buf_ref);
        unsafe { core::slice::from_raw_parts(data, len).to_vec() }
    };
    
    // Write JSON string using shared implementation
    write_json_string_impl(s, &mut buf, escape_html);
    
    // Return new slice
    let result = call.alloc_bytes(&buf);
    call.ret_ref(0, result);
    ExternResult::Ok
}

/// parseJsonString(data []byte, pos int) (string, int, error)
#[vostd_extern_ctx_nostd("json", "parseJsonString")]
fn parse_json_string_extern(call: &mut ExternCallContext) -> ExternResult {
    // Args: data []byte (1 slot), pos int (1 slot)
    let data = call.arg_bytes(0);
    let mut pos = call.arg_i64(1) as usize;
    
    if pos >= data.len() || data[pos] != b'"' {
        call.ret_str(0, "");
        call.ret_i64(1, pos as i64);
        write_error_to(call, 2, call.dyn_err().type_mismatch, "expected string");
        return ExternResult::Ok;
    }
    
    pos += 1; // skip opening quote
    let start = pos;
    let mut buf: Option<Vec<u8>> = None;
    
    loop {
        if pos >= data.len() {
            call.ret_str(0, "");
            call.ret_i64(1, pos as i64);
            write_error_to(call, 2, call.dyn_err().type_mismatch, "unterminated string");
            return ExternResult::Ok;
        }
        
        let c = data[pos];
        if c == b'"' {
            let result = if let Some(b) = buf {
                String::from_utf8_lossy(&b).to_string()
            } else {
                String::from_utf8_lossy(&data[start..pos]).to_string()
            };
            pos += 1; // skip closing quote
            call.ret_str(0, &result);
            call.ret_i64(1, pos as i64);
            call.ret_nil(2);
            call.ret_nil(3);
            return ExternResult::Ok;
        }
        
        if c == b'\\' {
            if buf.is_none() {
                buf = Some(data[start..pos].to_vec());
            }
            pos += 1;
            if pos >= data.len() {
                call.ret_str(0, "");
                call.ret_i64(1, pos as i64);
                write_error_to(call, 2, call.dyn_err().type_mismatch, "unterminated escape");
                return ExternResult::Ok;
            }
            let esc = data[pos];
            let b = buf.as_mut().unwrap();
            match esc {
                b'"' | b'\\' | b'/' => b.push(esc),
                b'b' => b.push(0x08),
                b'f' => b.push(0x0c),
                b'n' => b.push(b'\n'),
                b'r' => b.push(b'\r'),
                b't' => b.push(b'\t'),
                b'u' => {
                    if pos + 4 >= data.len() {
                        call.ret_str(0, "");
                        call.ret_i64(1, pos as i64);
                        write_error_to(call, 2, call.dyn_err().type_mismatch, "invalid unicode escape");
                        return ExternResult::Ok;
                    }
                    let hex = &data[pos+1..pos+5];
                    if let Ok(hex_str) = core::str::from_utf8(hex) {
                        if let Ok(code) = u32::from_str_radix(hex_str, 16) {
                            if let Some(ch) = char::from_u32(code) {
                                let mut tmp = [0u8; 4];
                                b.extend_from_slice(ch.encode_utf8(&mut tmp).as_bytes());
                            }
                        }
                    }
                    pos += 4;
                }
                _ => {
                    call.ret_str(0, "");
                    call.ret_i64(1, pos as i64);
                    write_error_to(call, 2, call.dyn_err().type_mismatch, "invalid escape");
                    return ExternResult::Ok;
                }
            }
            pos += 1;
        } else if c < 0x20 {
            call.ret_str(0, "");
            call.ret_i64(1, pos as i64);
            write_error_to(call, 2, call.dyn_err().type_mismatch, "control char in string");
            return ExternResult::Ok;
        } else {
            if let Some(b) = buf.as_mut() {
                b.push(c);
            }
            pos += 1;
        }
    }
}

crate::stdlib_register!(json: MarshalStruct, UnmarshalStruct, writeJsonString, parseJsonString);
