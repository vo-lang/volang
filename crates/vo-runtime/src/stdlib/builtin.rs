//! Builtin native function implementations.
//!
//! These are low-level builtin functions called directly by runtime.
//! They don't have corresponding .vo declarations and skip signature validation.
//!
//! print/println receive (value, value_kind) pairs and format based on kind.

use linkme::distributed_slice;
use vo_common_core::types::ValueKind;

use crate::ffi::{ExternCallContext, ExternEntryWithContext, ExternResult, EXTERN_TABLE_WITH_CONTEXT};
use crate::objects::string;

/// Format a single (value, value_kind) pair to string.
fn format_value(call: &ExternCallContext, slot: u16) -> String {
    let value = call.arg_u64(slot);
    let kind = ValueKind::from_u8(call.arg_u64(slot + 1) as u8);
    
    match kind {
        ValueKind::Void => "nil".to_string(),
        ValueKind::Bool => if value != 0 { "true" } else { "false" }.to_string(),
        ValueKind::Int | ValueKind::Int64 => (value as i64).to_string(),
        ValueKind::Int8 => (value as i8).to_string(),
        ValueKind::Int16 => (value as i16).to_string(),
        ValueKind::Int32 => (value as i32).to_string(),
        ValueKind::Uint | ValueKind::Uint64 => value.to_string(),
        ValueKind::Uint8 => (value as u8).to_string(),
        ValueKind::Uint16 => (value as u16).to_string(),
        ValueKind::Uint32 => (value as u32).to_string(),
        ValueKind::Float32 => f32::from_bits(value as u32).to_string(),
        ValueKind::Float64 => f64::from_bits(value).to_string(),
        ValueKind::String => {
            let s = string::as_str(value as crate::gc::GcRef);
            s.to_string()
        }
        ValueKind::Pointer => format!("ptr@{:#x}", value),
        ValueKind::Slice => format!("slice@{:#x}", value),
        ValueKind::Map => format!("map@{:#x}", value),
        ValueKind::Channel => format!("chan@{:#x}", value),
        ValueKind::Closure => format!("closure@{:#x}", value),
        ValueKind::Array => format!("array"),
        ValueKind::Struct => format!("struct"),
        ValueKind::Interface => format!("interface"),
    }
}

/// Format all (value, kind) pairs starting from `start_slot` into a space-separated string.
fn format_args(call: &ExternCallContext, start_slot: u16) -> String {
    let arg_count = call.arg_count();
    let mut result = String::new();
    let mut slot = start_slot;
    
    while slot + 2 <= arg_count && slot < 32 {
        let kind_val = call.arg_u64(slot + 1) as u8;
        if kind_val == 0 && slot > start_slot {
            break;
        }
        
        if !result.is_empty() {
            result.push(' ');
        }
        result.push_str(&format_value(call, slot));
        
        slot += 2;
        
        if kind_val == ValueKind::Void as u8 {
            break;
        }
    }
    
    result
}

/// vo_print - print values without newline (Go builtin print semantics)
fn builtin_print(call: &mut ExternCallContext) -> ExternResult {
    print!("{}", format_args(call, 0));
    ExternResult::Ok
}

/// vo_println - print values with newline (Go builtin println semantics)
fn builtin_println(call: &mut ExternCallContext) -> ExternResult {
    println!("{}", format_args(call, 0));
    ExternResult::Ok
}

/// vo_assert - assert condition with optional message
/// Args: (cond bool, [(value, kind), ...])
fn builtin_assert(call: &mut ExternCallContext) -> ExternResult {
    let cond = call.arg_bool(0);
    if !cond {
        let msg_part = format_args(call, 2);
        let msg = if msg_part.is_empty() {
            "assertion failed".to_string()
        } else {
            format!("assertion failed: {}", msg_part)
        };
        return ExternResult::Panic(msg);
    }
    ExternResult::Ok
}

// Register builtins via linkme
#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_BUILTIN_PRINT: ExternEntryWithContext = ExternEntryWithContext {
    name: "vo_print",
    func: builtin_print,
};

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_BUILTIN_PRINTLN: ExternEntryWithContext = ExternEntryWithContext {
    name: "vo_println",
    func: builtin_println,
};

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_BUILTIN_ASSERT: ExternEntryWithContext = ExternEntryWithContext {
    name: "vo_assert",
    func: builtin_assert,
};

fn builtin_copy(call: &mut ExternCallContext) -> ExternResult {
    use crate::objects::{slice, array};
    
    let dst = call.arg_ref(0);
    let src = call.arg_ref(1);
    
    if dst.is_null() || src.is_null() {
        call.ret_i64(0, 0);
        return ExternResult::Ok;
    }
    
    let dst_len = slice::len(dst);
    let src_len = slice::len(src);
    let copy_len = dst_len.min(src_len);
    
    if copy_len == 0 {
        call.ret_i64(0, 0);
        return ExternResult::Ok;
    }
    
    let dst_arr = slice::array_ref(dst);
    let elem_bytes = array::elem_bytes(dst_arr);
    let dst_ptr = slice::data_ptr(dst);
    let src_ptr = slice::data_ptr(src);
    
    unsafe { core::ptr::copy_nonoverlapping(src_ptr, dst_ptr, copy_len * elem_bytes) };
    
    call.ret_i64(0, copy_len as i64);
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_BUILTIN_COPY: ExternEntryWithContext = ExternEntryWithContext {
    name: "vo_copy",
    func: builtin_copy,
};

// ==================== String Conversion Functions ====================

/// int -> string (unicode code point)
fn conv_int_str(call: &mut ExternCallContext) -> ExternResult {
    let code_point = call.arg_u64(0) as u32;
    let s = if let Some(c) = char::from_u32(code_point) {
        c.to_string()
    } else {
        "\u{FFFD}".to_string()
    };
    let gc_ref = crate::objects::string::new_from_string(call.gc(), s);
    call.ret_ref(0, gc_ref);
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_CONV_INT_STR: ExternEntryWithContext = ExternEntryWithContext {
    name: "vo_conv_int_str",
    func: conv_int_str,
};

/// []byte -> string (shares underlying array)
fn conv_bytes_str(call: &mut ExternCallContext) -> ExternResult {
    let slice_ref = call.arg_ref(0);
    let gc_ref = crate::objects::string::from_slice(call.gc(), slice_ref);
    call.ret_ref(0, gc_ref);
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_CONV_BYTES_STR: ExternEntryWithContext = ExternEntryWithContext {
    name: "vo_conv_bytes_str",
    func: conv_bytes_str,
};

/// string -> []byte (must copy)
fn conv_str_bytes(call: &mut ExternCallContext) -> ExternResult {
    let str_ref = call.arg_ref(0);
    let gc_ref = crate::objects::string::to_byte_slice_obj(call.gc(), str_ref);
    call.ret_ref(0, gc_ref);
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_CONV_STR_BYTES: ExternEntryWithContext = ExternEntryWithContext {
    name: "vo_conv_str_bytes",
    func: conv_str_bytes,
};

/// []rune -> string
fn conv_runes_str(call: &mut ExternCallContext) -> ExternResult {
    let slice_ref = call.arg_ref(0);
    let gc_ref = crate::objects::string::from_rune_slice_obj(call.gc(), slice_ref);
    call.ret_ref(0, gc_ref);
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_CONV_RUNES_STR: ExternEntryWithContext = ExternEntryWithContext {
    name: "vo_conv_runes_str",
    func: conv_runes_str,
};

/// string -> []rune
fn conv_str_runes(call: &mut ExternCallContext) -> ExternResult {
    let str_ref = call.arg_ref(0);
    let gc_ref = crate::objects::string::to_rune_slice_obj(call.gc(), str_ref);
    call.ret_ref(0, gc_ref);
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_CONV_STR_RUNES: ExternEntryWithContext = ExternEntryWithContext {
    name: "vo_conv_str_runes",
    func: conv_str_runes,
};
