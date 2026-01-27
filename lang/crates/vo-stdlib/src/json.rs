//! JSON struct serialization/deserialization native implementations.
//! Uses the serde module for visitor-pattern based marshaling.

#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use vo_common_core::types::ValueKind;

use vo_runtime::ffi::{ExternCallContext, ExternResult};
use vo_runtime::gc::GcRef;
use vo_runtime::objects::interface;
use vo_runtime::builtins::error_helper::write_error_to;
use super::serde::{marshal_struct_value, marshal_any_value, unmarshal_struct, get_pointed_type_rttid, FormatWriter as _};
use super::serde_json::{JsonWriter, JsonReader, write_json_string_to_buf};

use vo_ffi_macro::vostd_extern_ctx_nostd;

fn marshal_impl(call: &mut ExternCallContext) -> ExternResult {
    let v_slot0 = call.arg_u64(0);
    let v_slot1 = call.arg_u64(1);
    
    let vk = interface::unpack_value_kind(v_slot0);
    let rttid = interface::unpack_rttid(v_slot0);
    
    let mut writer = JsonWriter::new();
    
    let result = match vk {
        ValueKind::Struct => marshal_struct_value(call, v_slot1 as GcRef, rttid, &mut writer),
        ValueKind::Pointer => {
            let ptr = v_slot1 as GcRef;
            let elem_rttid = get_pointed_type_rttid(call, rttid);
            marshal_struct_value(call, ptr, elem_rttid, &mut writer)
        }
        _ => marshal_any_value(call, v_slot0, v_slot1, &mut writer),
    };
    
    match result {
        Ok(()) => {
            let buf = writer.into_bytes();
            let slice_ref = call.alloc_bytes(&buf);
            call.ret_ref(0, slice_ref);
            call.ret_nil(1);
            call.ret_nil(2);
            ExternResult::Ok
        }
        Err(msg) => {
            call.ret_nil(0);
            write_error_to(call, 1, msg);
            ExternResult::Ok
        }
    }
}

#[vostd_extern_ctx_nostd("encoding/json", "marshalAny")]
fn marshal_any(call: &mut ExternCallContext) -> ExternResult {
    marshal_impl(call)
}

#[vostd_extern_ctx_nostd("encoding/json", "Unmarshal")]
fn unmarshal_extern(call: &mut ExternCallContext) -> ExternResult {
    let json_str = {
        let data = call.arg_bytes(0);
        if data.is_empty() {
            write_error_to(call, 0, "empty JSON");
            return ExternResult::Ok;
        }
        match core::str::from_utf8(data) {
            Ok(s) => s.to_string(),
            Err(_) => {
                write_error_to(call, 0, "invalid UTF-8");
                return ExternResult::Ok;
            }
        }
    };
    
    let v_slot0 = call.arg_u64(1);
    let v_slot1 = call.arg_u64(2);
    
    let vk = interface::unpack_value_kind(v_slot0);
    let rttid = interface::unpack_rttid(v_slot0);
    
    if vk != ValueKind::Pointer {
        write_error_to(call, 0, "target must be pointer");
        return ExternResult::Ok;
    }
    
    let ptr = v_slot1 as GcRef;
    if ptr.is_null() {
        write_error_to(call, 0, "nil pointer");
        return ExternResult::Ok;
    }
    
    let pointed_rttid = get_pointed_type_rttid(call, rttid);
    
    match unmarshal_struct::<JsonReader>(call, ptr, pointed_rttid, json_str.trim()) {
        Ok(()) => { call.ret_nil(0); call.ret_nil(1); }
        Err(msg) => write_error_to(call, 0, msg),
    }
    ExternResult::Ok
}

// ==================== Shared String Functions ====================

/// writeJsonString(buf []byte, s string, escapeHTML bool) []byte
#[vostd_extern_ctx_nostd("encoding/json", "writeJsonString")]
fn write_json_string_extern(call: &mut ExternCallContext) -> ExternResult {
    // Args: buf []byte (1 slot), s string (1 slot), escapeHTML bool (1 slot)
    let buf_ref = call.arg_ref(0);
    let s = call.arg_str(1);
    let escape_html = call.arg_bool(2);
    
    // Read existing buffer content
    let mut buf: Vec<u8> = if buf_ref.is_null() {
        Vec::new()
    } else {
        let data = vo_runtime::objects::slice::data_ptr(buf_ref);
        let len = vo_runtime::objects::slice::len(buf_ref);
        unsafe { core::slice::from_raw_parts(data, len).to_vec() }
    };
    
    // Write JSON string
    write_json_string_to_buf(s, &mut buf, escape_html);
    
    // Return new slice
    let result = call.alloc_bytes(&buf);
    call.ret_ref(0, result);
    ExternResult::Ok
}

/// parseJsonString(data []byte, pos int) (string, int, error)
#[vostd_extern_ctx_nostd("encoding/json", "parseJsonString")]
fn parse_json_string_extern(call: &mut ExternCallContext) -> ExternResult {
    // Args: data []byte (1 slot), pos int (1 slot)
    let data = call.arg_bytes(0);
    let mut pos = call.arg_i64(1) as usize;
    
    if pos >= data.len() || data[pos] != b'"' {
        call.ret_str(0, "");
        call.ret_i64(1, pos as i64);
        write_error_to(call, 2, "expected string");
        return ExternResult::Ok;
    }
    
    pos += 1; // skip opening quote
    let start = pos;
    let mut buf: Option<Vec<u8>> = None;
    
    loop {
        if pos >= data.len() {
            call.ret_str(0, "");
            call.ret_i64(1, pos as i64);
            write_error_to(call, 2, "unterminated string");
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
                write_error_to(call, 2, "unterminated escape");
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
                        write_error_to(call, 2, "invalid unicode escape");
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
                    write_error_to(call, 2, "invalid escape");
                    return ExternResult::Ok;
                }
            }
            pos += 1;
        } else if c < 0x20 {
            call.ret_str(0, "");
            call.ret_i64(1, pos as i64);
            write_error_to(call, 2, "control char in string");
            return ExternResult::Ok;
        } else {
            if let Some(b) = buf.as_mut() {
                b.push(c);
            }
            pos += 1;
        }
    }
}

vo_runtime::stdlib_register!(encoding_json: marshalAny, Unmarshal, writeJsonString, parseJsonString);
