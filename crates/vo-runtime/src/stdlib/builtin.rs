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
    use crate::objects::{slice, array, string};
    use crate::gc::Gc;
    use vo_common_core::types::ValueKind;
    
    let dst = call.arg_ref(0);
    let src = call.arg_ref(1);
    
    if dst.is_null() || src.is_null() {
        call.ret_i64(0, 0);
        return ExternResult::Ok;
    }
    
    let dst_len = slice::len(dst);
    
    // Check if src is a string (copy([]byte, string) case)
    let src_kind = Gc::header(src).value_meta.value_kind();
    let (src_len, src_ptr) = if src_kind == ValueKind::String {
        let len = string::len(src);
        let bytes = string::as_bytes(src);
        (len, bytes.as_ptr() as *mut u8)
    } else {
        (slice::len(src), slice::data_ptr(src))
    };
    
    let copy_len = dst_len.min(src_len);
    
    if copy_len == 0 {
        call.ret_i64(0, 0);
        return ExternResult::Ok;
    }
    
    let dst_arr = slice::array_ref(dst);
    let elem_bytes = array::elem_bytes(dst_arr);
    let dst_ptr = slice::data_ptr(dst);
    
    // Use copy (not copy_nonoverlapping) to support overlapping regions (Go semantics)
    unsafe { core::ptr::copy(src_ptr, dst_ptr, copy_len * elem_bytes) };
    
    call.ret_i64(0, copy_len as i64);
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_BUILTIN_COPY: ExternEntryWithContext = ExternEntryWithContext {
    name: "vo_copy",
    func: builtin_copy,
};

/// append(slice, other...) - append all elements from other slice
fn builtin_slice_append_slice(call: &mut ExternCallContext) -> ExternResult {
    use crate::objects::{slice, array};
    use vo_common_core::types::ValueMeta;
    
    let dst = call.arg_ref(0);
    let src = call.arg_ref(1);
    let elem_meta = ValueMeta::from_raw(call.arg_u64(2) as u32);
    
    // Handle nil src
    if src.is_null() {
        call.ret_ref(0, dst);
        return ExternResult::Ok;
    }
    
    let src_len = slice::len(src);
    if src_len == 0 {
        call.ret_ref(0, dst);
        return ExternResult::Ok;
    }
    
    let elem_bytes = if dst.is_null() {
        array::elem_bytes(slice::array_ref(src))
    } else {
        array::elem_bytes(slice::array_ref(dst))
    };
    
    // Handle nil dst
    if dst.is_null() {
        let new_cap = src_len.max(4);
        let new_arr = array::create(call.gc(), elem_meta, elem_bytes, new_cap);
        let src_ptr = slice::data_ptr(src);
        let dst_ptr = array::data_ptr_bytes(new_arr);
        unsafe { core::ptr::copy_nonoverlapping(src_ptr, dst_ptr, src_len * elem_bytes) };
        let result = slice::from_array_range(call.gc(), new_arr, 0, src_len, new_cap);
        call.ret_ref(0, result);
        return ExternResult::Ok;
    }
    
    let dst_len = slice::len(dst);
    let dst_cap = slice::cap(dst);
    let new_len = dst_len + src_len;
    
    if new_len <= dst_cap {
        // Enough capacity - write to existing backing array, return new slice header
        let dst_ptr = slice::data_ptr(dst);
        let src_ptr = slice::data_ptr(src);
        unsafe {
            let write_ptr = dst_ptr.add(dst_len * elem_bytes);
            core::ptr::copy_nonoverlapping(src_ptr, write_ptr, src_len * elem_bytes);
        }
        // Go semantics: append never modifies original slice header
        let new_s = slice::with_new_len(call.gc(), dst, new_len);
        call.ret_ref(0, new_s);
    } else {
        // Need to grow - allocate new array
        let new_cap = (new_len * 2).max(4);
        let new_arr = array::create(call.gc(), elem_meta, elem_bytes, new_cap);
        let new_arr_ptr = array::data_ptr_bytes(new_arr);
        let dst_ptr = slice::data_ptr(dst);
        let src_ptr = slice::data_ptr(src);
        unsafe {
            core::ptr::copy_nonoverlapping(dst_ptr, new_arr_ptr, dst_len * elem_bytes);
            core::ptr::copy_nonoverlapping(src_ptr, new_arr_ptr.add(dst_len * elem_bytes), src_len * elem_bytes);
        }
        let result = slice::from_array_range(call.gc(), new_arr, 0, new_len, new_cap);
        call.ret_ref(0, result);
    }
    
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_BUILTIN_SLICE_APPEND_SLICE: ExternEntryWithContext = ExternEntryWithContext {
    name: "vo_slice_append_slice",
    func: builtin_slice_append_slice,
};

/// Interface equality comparison
/// Args: (left_slot0, left_slot1, right_slot0, right_slot1)
/// Returns: bool (1 if equal, 0 if not)
fn builtin_iface_eq(call: &mut ExternCallContext) -> ExternResult {
    use crate::objects::string as str_obj;
    
    let left_slot0 = call.arg_u64(0);
    let left_slot1 = call.arg_u64(1);
    let right_slot0 = call.arg_u64(2);
    let right_slot1 = call.arg_u64(3);
    
    // slot0 format: [itab_id:32 | rttid:24 | value_kind:8]
    let left_vk = ValueKind::from_u8((left_slot0 & 0xFF) as u8);
    let right_vk = ValueKind::from_u8((right_slot0 & 0xFF) as u8);
    
    // If value_kinds differ, not equal (different dynamic types)
    if left_vk != right_vk {
        call.ret_bool(0, false);
        return ExternResult::Ok;
    }
    
    // Compare based on value_kind
    let equal = match left_vk {
        ValueKind::Void => true, // both nil
        ValueKind::Bool | ValueKind::Int | ValueKind::Int8 | ValueKind::Int16 | 
        ValueKind::Int32 | ValueKind::Int64 | ValueKind::Uint | ValueKind::Uint8 | 
        ValueKind::Uint16 | ValueKind::Uint32 | ValueKind::Uint64 |
        ValueKind::Float32 | ValueKind::Float64 | ValueKind::Pointer | 
        ValueKind::Slice | ValueKind::Map | ValueKind::Channel | ValueKind::Closure => {
            // Immediate or reference identity comparison
            left_slot1 == right_slot1
        }
        ValueKind::String => {
            // String content comparison
            let left_ref = left_slot1 as crate::gc::GcRef;
            let right_ref = right_slot1 as crate::gc::GcRef;
            if left_ref == right_ref {
                true
            } else if left_ref.is_null() || right_ref.is_null() {
                false
            } else {
                str_obj::as_str(left_ref) == str_obj::as_str(right_ref)
            }
        }
        ValueKind::Struct | ValueKind::Array => {
            // For struct/array in interface, compare rttid first, then data
            // rttid is in bits 8-31 of slot0
            let left_rttid = (left_slot0 >> 8) & 0xFFFFFF;
            let right_rttid = (right_slot0 >> 8) & 0xFFFFFF;
            if left_rttid != right_rttid {
                false
            } else {
                // Same type - compare slot1 (GcRef to boxed data)
                // For now, just compare references (identity)
                // TODO: deep comparison for value equality
                left_slot1 == right_slot1
            }
        }
        ValueKind::Interface => {
            // Nested interface - compare both slots
            left_slot0 == right_slot0 && left_slot1 == right_slot1
        }
    };
    
    call.ret_bool(0, equal);
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_BUILTIN_IFACE_EQ: ExternEntryWithContext = ExternEntryWithContext {
    name: "vo_iface_eq",
    func: builtin_iface_eq,
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
