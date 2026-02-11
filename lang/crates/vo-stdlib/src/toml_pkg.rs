//! TOML struct serialization/deserialization native implementations.
//! Uses the serde module for visitor-pattern based marshaling.

#[cfg(not(feature = "std"))]
use alloc::string::ToString;

use vo_common_core::types::ValueKind;

use vo_runtime::ffi::{ExternCallContext, ExternResult};
use vo_runtime::gc::GcRef;
use vo_runtime::objects::interface;
use vo_runtime::builtins::error_helper::write_error_to;
use super::serde::{marshal_struct_value, marshal_any_value, unmarshal_struct, get_pointed_type_rttid, FormatWriter as _};
use super::serde_toml::{TomlWriter, TomlReader};

use vo_ffi_macro::vostd_fn;

#[vostd_fn("encoding/toml", "marshalAny")]
fn marshal_any(call: &mut ExternCallContext) -> ExternResult {
    let v_slot0 = call.arg_u64(0);
    let v_slot1 = call.arg_u64(1);
    
    let vk = interface::unpack_value_kind(v_slot0);
    let rttid = interface::unpack_rttid(v_slot0);
    
    let mut writer = TomlWriter::new();
    
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

#[vostd_fn("encoding/toml", "Unmarshal")]
fn unmarshal_extern(call: &mut ExternCallContext) -> ExternResult {
    let toml_str = {
        let data = call.arg_bytes(0);
        if data.is_empty() {
            write_error_to(call, 0, "empty TOML");
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
    
    match unmarshal_struct::<TomlReader>(call, ptr, pointed_rttid, toml_str.trim()) {
        Ok(()) => { call.ret_nil(0); call.ret_nil(1); }
        Err(msg) => write_error_to(call, 0, msg),
    }
    ExternResult::Ok
}

vo_runtime::stdlib_register!(encoding_toml: marshalAny, Unmarshal);
