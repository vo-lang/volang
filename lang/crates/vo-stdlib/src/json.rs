//! JSON struct serialization/deserialization native implementations.
//! Uses the serde module for visitor-pattern based marshaling.

#[cfg(not(feature = "std"))]
use alloc::string::ToString;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use vo_common_core::types::ValueKind;

use super::serde::{
    get_pointed_type_rttid, marshal_any_value, marshal_struct_value, unmarshal_struct,
    FormatWriter as _,
};
use super::serde_json::{
    parse_json_string_at, write_json_string_bytes_to_buf, JsonReader, JsonWriter,
};
use vo_runtime::builtins::error_helper::write_error_to;
use vo_runtime::ffi::{ExternCallContext, ExternResult};
use vo_runtime::gc::GcRef;
use vo_runtime::objects::interface;

use vo_ffi_macro::vostd_fn;

fn marshal_impl(call: &mut ExternCallContext) -> ExternResult {
    let v_slot0 = call.arg_u64(0);
    let v_slot1 = call.arg_u64(1);

    let vk = interface::unpack_value_kind(v_slot0);
    let rttid = interface::unpack_rttid(v_slot0);

    let mut writer = JsonWriter::new();

    let result = match vk {
        ValueKind::Struct => {
            let ptr = v_slot1 as GcRef;
            if ptr.is_null() {
                writer.write_null()
            } else {
                marshal_struct_value(call, ptr, rttid, &mut writer)
            }
        }
        ValueKind::Pointer => {
            let ptr = v_slot1 as GcRef;
            if ptr.is_null() {
                writer.write_null()
            } else {
                let elem_rttid = get_pointed_type_rttid(call, rttid);
                marshal_struct_value(call, ptr, elem_rttid, &mut writer)
            }
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

#[vostd_fn("encoding/json", "marshalAny")]
fn marshal_any(call: &mut ExternCallContext) -> ExternResult {
    marshal_impl(call)
}

#[vostd_fn("encoding/json", "unmarshalAny")]
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

    match unmarshal_struct::<JsonReader>(call, ptr, pointed_rttid, &json_str) {
        Ok(()) => {
            call.ret_nil(0);
            call.ret_nil(1);
        }
        Err(msg) => {
            write_error_to(call, 0, msg);
        }
    }
    ExternResult::Ok
}

// ==================== Shared String Functions ====================

/// writeJsonString(buf []byte, s string, escapeHTML bool) []byte
#[vostd_fn("encoding/json", "writeJsonString")]
fn write_json_string_extern(call: &mut ExternCallContext) -> ExternResult {
    // Args: buf []byte (1 slot), s string (1 slot), escapeHTML bool (1 slot)
    let buf_ref = call.arg_ref(0);
    let raw = call.arg_string_bytes(1);
    let escape_html = call.arg_bool(2);

    // Read existing buffer content
    let mut buf: Vec<u8> = if buf_ref.is_null() {
        Vec::new()
    } else {
        // Safety: `buf_ref` is a rooted []byte extern argument.
        unsafe { vo_runtime::objects::slice::byte_vec(buf_ref) }
    };

    // Write JSON string
    write_json_string_bytes_to_buf(&raw, &mut buf, escape_html);

    // Return new slice
    let result = call.alloc_bytes(&buf);
    call.ret_ref(0, result);
    ExternResult::Ok
}

/// parseJsonString(data []byte, pos int) (string, int, error)
#[vostd_fn("encoding/json", "parseJsonString")]
fn parse_json_string_extern(call: &mut ExternCallContext) -> ExternResult {
    // Args: data []byte (1 slot), pos int (1 slot)
    let data = call.arg_bytes(0);
    let raw_pos = call.arg_i64(1);
    let Ok(pos) = usize::try_from(raw_pos) else {
        call.ret_str(0, "");
        call.ret_i64(1, raw_pos);
        write_error_to(call, 2, "negative JSON string position");
        return ExternResult::Ok;
    };
    let input = match core::str::from_utf8(data) {
        Ok(input) => input,
        Err(_) => {
            call.ret_str(0, "");
            call.ret_i64(1, raw_pos);
            write_error_to(call, 2, "invalid UTF-8 in JSON string");
            return ExternResult::Ok;
        }
    };

    match parse_json_string_at(input, pos) {
        Ok((value, end)) => {
            let Ok(end) = i64::try_from(end) else {
                call.ret_str(0, "");
                call.ret_i64(1, raw_pos);
                write_error_to(call, 2, "JSON string position exceeds int range");
                return ExternResult::Ok;
            };
            call.ret_str(0, &value);
            call.ret_i64(1, end);
            call.ret_nil(2);
            call.ret_nil(3);
        }
        Err(error) => {
            call.ret_str(0, "");
            call.ret_i64(1, raw_pos);
            write_error_to(call, 2, error);
        }
    }
    ExternResult::Ok
}

vo_ffi_macro::vostd_register!("encoding/json": marshalAny, unmarshalAny, writeJsonString, parseJsonString);
