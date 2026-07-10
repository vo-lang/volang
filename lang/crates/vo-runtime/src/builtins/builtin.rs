//! Builtin native function implementations.
//!
//! These are low-level builtin functions called directly by runtime.
//! They don't have corresponding .vo declarations and skip signature validation.
//!
//! print/println receive interface{} values (each 2 slots).
//! All args are uniformly boxed as interface by codegen.

#[cfg(not(feature = "std"))]
use alloc::format;
#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};

use super::format::format_interface_with_ctx;
use crate::ffi::{ExternCallContext, ExternResult};

/// Format all interface{} args starting from `start_slot` into a space-separated string.
/// Each arg is 2 slots: [slot0 = packed_info, slot1 = data]
fn format_args(call: &ExternCallContext, start_slot: u16) -> String {
    let arg_count = call.arg_count();
    let mut result = String::new();
    let mut slot = start_slot;

    while slot + 2 <= arg_count {
        if !result.is_empty() {
            result.push(' ');
        }
        let slot0 = call.arg_u64(slot);
        let slot1 = call.arg_u64(slot + 1);
        result.push_str(&format_interface_with_ctx(slot0, slot1, Some(call)));
        slot += 2;
    }

    result
}

/// vo_print - print values without newline (Go builtin print semantics)
fn builtin_print(call: &mut ExternCallContext) -> ExternResult {
    call.write_output(&format_args(call, 0));
    ExternResult::Ok
}

/// vo_println - print values with newline (Go builtin println semantics)
fn builtin_println(call: &mut ExternCallContext) -> ExternResult {
    call.writeln_output(&format_args(call, 0));
    ExternResult::Ok
}

/// vo_assert - assert condition with optional message
/// Args are passed as interfaces (2 slots each): (cond_iface[0:1], msg_iface[2:3], ...)
/// cond_iface: slot 0 = metadata, slot 1 = bool data
fn builtin_assert(call: &mut ExternCallContext) -> ExternResult {
    // Read bool from interface data slot (slot 1), not metadata slot (slot 0)
    let cond = call.arg_bool(1);
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

fn builtin_copy(call: &mut ExternCallContext) -> ExternResult {
    use crate::gc::Gc;
    use crate::objects::{array, slice, string as str_obj};
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
        // Safety: the GC header above established that `src` is a live string.
        let len = unsafe { str_obj::len(src) };
        let bytes = unsafe { str_obj::bytes_unchecked(src) };
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

    let elem_meta = array::elem_meta(dst_arr);
    if elem_meta.value_kind().may_contain_gc_refs() {
        call.typed_write_barrier_range_by_meta(
            dst_arr,
            src_ptr as *const u8,
            copy_len,
            elem_bytes,
            elem_meta,
        );
    }
    // Use copy (not copy_nonoverlapping) to support overlapping regions (Go semantics)
    unsafe { core::ptr::copy(src_ptr, dst_ptr, copy_len * elem_bytes) };

    call.ret_i64(0, copy_len as i64);
    ExternResult::Ok
}

/// append(slice, other...) - append all elements from other slice/string
/// Works for both slice and string sources since they have identical memory layout.
fn builtin_slice_append_slice(call: &mut ExternCallContext) -> ExternResult {
    use crate::objects::{array, slice};

    let dst = call.arg_ref(0);
    let src = call.arg_ref(1);
    let _legacy_elem_meta = call.arg_u64(2);

    // Handle nil src
    if src.is_null() {
        call.ret_ref(0, dst);
        return ExternResult::Ok;
    }

    // String and slice have identical layout, so we can use slice:: functions for both
    let src_len = slice::len(src);
    if src_len == 0 {
        call.ret_ref(0, dst);
        return ExternResult::Ok;
    }

    let src_elem_meta = slice::elem_meta(src);
    let src_elem_bytes = array::elem_bytes(slice::array_ref(src));
    let (elem_meta, elem_bytes) = if dst.is_null() {
        (src_elem_meta, src_elem_bytes)
    } else {
        let dst_elem_meta = slice::elem_meta(dst);
        let dst_elem_bytes = array::elem_bytes(slice::array_ref(dst));
        if dst_elem_meta != src_elem_meta || dst_elem_bytes != src_elem_bytes {
            call.record_contract_violation(format!(
                "vo_slice_append_slice element layout mismatch: dst_meta={} dst_bytes={} src_meta={} src_bytes={}",
                dst_elem_meta.to_raw(),
                dst_elem_bytes,
                src_elem_meta.to_raw(),
                src_elem_bytes
            ));
            call.ret_ref(0, dst);
            return ExternResult::Ok;
        }
        (dst_elem_meta, dst_elem_bytes)
    };

    // Handle nil dst
    if dst.is_null() {
        let new_cap = src_len.max(4);
        let new_arr = array::create(call.gc(), elem_meta, elem_bytes, new_cap);
        let src_ptr = slice::data_ptr(src);
        let dst_ptr = array::data_ptr_bytes(new_arr);
        unsafe { core::ptr::copy_nonoverlapping(src_ptr, dst_ptr, src_len * elem_bytes) };
        if elem_meta.value_kind().may_contain_gc_refs() {
            call.gc().mark_allocated_for_scan(new_arr);
        }
        let result = slice::from_array_range(call.gc(), new_arr, 0, src_len);
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
        let write_ptr = unsafe { dst_ptr.add(dst_len * elem_bytes) };
        if elem_meta.value_kind().may_contain_gc_refs() {
            let arr_ref = slice::array_ref(dst);
            call.typed_write_barrier_range_by_meta(
                arr_ref,
                src_ptr as *const u8,
                src_len,
                elem_bytes,
                elem_meta,
            );
        }
        unsafe {
            core::ptr::copy(src_ptr, write_ptr, src_len * elem_bytes);
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
            core::ptr::copy_nonoverlapping(
                src_ptr,
                new_arr_ptr.add(dst_len * elem_bytes),
                src_len * elem_bytes,
            );
        }
        if elem_meta.value_kind().may_contain_gc_refs() {
            call.gc().mark_allocated_for_scan(new_arr);
        }
        let result = slice::from_array_range(call.gc(), new_arr, 0, new_len);
        call.ret_ref(0, result);
    }

    ExternResult::Ok
}

/// Interface equality comparison
/// Args: (left_slot0, left_slot1, right_slot0, right_slot1)
/// Returns: bool (1 if equal, 0 if not)
fn builtin_iface_eq(call: &mut ExternCallContext) -> ExternResult {
    let result = crate::objects::compare::iface_eq(
        call.arg_u64(0),
        call.arg_u64(1),
        call.arg_u64(2),
        call.arg_u64(3),
        call.module(),
    );
    match result {
        0 | 1 => {
            call.ret_bool(0, result == 1);
            ExternResult::Ok
        }
        2 => ExternResult::Panic(crate::objects::compare::UNCOMPARABLE_INTERFACE_ERROR.to_string()),
        code => ExternResult::Panic(format!(
            "internal error: invalid interface equality result {code}"
        )),
    }
}

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

/// []byte -> string (shares underlying array)
fn conv_bytes_str(call: &mut ExternCallContext) -> ExternResult {
    let slice_ref = call.arg_ref(0);
    // Safety: the builtin ABI supplies a live byte-slice argument.
    let gc_ref = unsafe { crate::objects::string::from_slice(call.gc(), slice_ref) };
    call.ret_ref(0, gc_ref);
    ExternResult::Ok
}

/// string -> []byte (must copy)
fn conv_str_bytes(call: &mut ExternCallContext) -> ExternResult {
    let str_ref = call.arg_ref(0);
    // Safety: the builtin ABI supplies a live string argument.
    let gc_ref = unsafe { crate::objects::string::to_byte_slice_obj(call.gc(), str_ref) };
    call.ret_ref(0, gc_ref);
    ExternResult::Ok
}

/// []rune -> string
fn conv_runes_str(call: &mut ExternCallContext) -> ExternResult {
    let slice_ref = call.arg_ref(0);
    // Safety: the builtin ABI supplies a live rune-slice argument.
    let gc_ref = unsafe { crate::objects::string::from_rune_slice_obj(call.gc(), slice_ref) };
    call.ret_ref(0, gc_ref);
    ExternResult::Ok
}

/// string -> []rune
fn conv_str_runes(call: &mut ExternCallContext) -> ExternResult {
    let str_ref = call.arg_ref(0);
    // Safety: the builtin ABI supplies a live string argument.
    let gc_ref = unsafe { crate::objects::string::to_rune_slice_obj(call.gc(), str_ref) };
    call.ret_ref(0, gc_ref);
    ExternResult::Ok
}

/// Panic with an error value.
/// Used by ? operator and dynamic write in functions without error return value.
/// Args: error interface (2 slots: slot0=meta, slot1=data)
fn panic_with_error(call: &mut ExternCallContext) -> ExternResult {
    let error_slot0 = call.arg_u64(0);
    let error_data = call.arg_u64(1);

    // Use format_interface_with_ctx to properly extract error message
    let error_str = format_interface_with_ctx(error_slot0, error_data, Some(call));
    let msg = format!("panic: {}", error_str);

    ExternResult::Panic(msg)
}

fn math_sqrt(call: &mut ExternCallContext) -> ExternResult {
    call.ret_f64(0, call.arg_f64(0).sqrt());
    ExternResult::Ok
}

fn math_floor(call: &mut ExternCallContext) -> ExternResult {
    call.ret_f64(0, call.arg_f64(0).floor());
    ExternResult::Ok
}

fn math_ceil(call: &mut ExternCallContext) -> ExternResult {
    call.ret_f64(0, call.arg_f64(0).ceil());
    ExternResult::Ok
}

fn math_trunc(call: &mut ExternCallContext) -> ExternResult {
    call.ret_f64(0, call.arg_f64(0).trunc());
    ExternResult::Ok
}

fn math_fma(call: &mut ExternCallContext) -> ExternResult {
    call.ret_f64(0, call.arg_f64(0).mul_add(call.arg_f64(1), call.arg_f64(2)));
    ExternResult::Ok
}

/// Register builtin extern functions (for no_std mode).
#[derive(Clone, Copy)]
struct BuiltinExternEntry {
    name: &'static str,
    func: crate::ffi::ExternFn,
    effects: crate::bytecode::ExternEffects,
}

const REGISTERED_EXTERNS: &[BuiltinExternEntry] = &[
    BuiltinExternEntry {
        name: "vo_print",
        func: builtin_print,
        effects: crate::bytecode::ExternEffects::NONE,
    },
    BuiltinExternEntry {
        name: "vo_println",
        func: builtin_println,
        effects: crate::bytecode::ExternEffects::NONE,
    },
    BuiltinExternEntry {
        name: "vo_assert",
        func: builtin_assert,
        effects: crate::bytecode::ExternEffects::NONE,
    },
    BuiltinExternEntry {
        name: "vo_copy",
        func: builtin_copy,
        effects: crate::bytecode::ExternEffects::NONE,
    },
    BuiltinExternEntry {
        name: "vo_slice_append_slice",
        func: builtin_slice_append_slice,
        effects: crate::bytecode::ExternEffects::NONE,
    },
    BuiltinExternEntry {
        name: "vo_iface_eq",
        func: builtin_iface_eq,
        effects: crate::bytecode::ExternEffects::NONE,
    },
    BuiltinExternEntry {
        name: "vo_conv_int_str",
        func: conv_int_str,
        effects: crate::bytecode::ExternEffects::NONE,
    },
    BuiltinExternEntry {
        name: "vo_conv_bytes_str",
        func: conv_bytes_str,
        effects: crate::bytecode::ExternEffects::NONE,
    },
    BuiltinExternEntry {
        name: "vo_conv_str_bytes",
        func: conv_str_bytes,
        effects: crate::bytecode::ExternEffects::NONE,
    },
    BuiltinExternEntry {
        name: "vo_conv_runes_str",
        func: conv_runes_str,
        effects: crate::bytecode::ExternEffects::NONE,
    },
    BuiltinExternEntry {
        name: "vo_conv_str_runes",
        func: conv_str_runes,
        effects: crate::bytecode::ExternEffects::NONE,
    },
    BuiltinExternEntry {
        name: "panic_with_error",
        func: panic_with_error,
        effects: crate::bytecode::ExternEffects::NONE,
    },
    BuiltinExternEntry {
        name: "math_Sqrt",
        func: math_sqrt,
        effects: crate::bytecode::ExternEffects::NONE,
    },
    BuiltinExternEntry {
        name: "math_Floor",
        func: math_floor,
        effects: crate::bytecode::ExternEffects::NONE,
    },
    BuiltinExternEntry {
        name: "math_Ceil",
        func: math_ceil,
        effects: crate::bytecode::ExternEffects::NONE,
    },
    BuiltinExternEntry {
        name: "math_Trunc",
        func: math_trunc,
        effects: crate::bytecode::ExternEffects::NONE,
    },
    BuiltinExternEntry {
        name: "math_FMA",
        func: math_fma,
        effects: crate::bytecode::ExternEffects::NONE,
    },
];

pub fn known_extern_allowed_effects(name: &str) -> Option<crate::bytecode::ExternEffects> {
    REGISTERED_EXTERNS
        .iter()
        .find(|entry| entry.name == name)
        .map(|entry| entry.effects)
}

pub fn register_externs(
    registry: &mut crate::ffi::ExternRegistry,
    externs: &[crate::bytecode::ExternDef],
) {
    for (id, def) in externs.iter().enumerate() {
        for entry in REGISTERED_EXTERNS {
            if def.name == entry.name {
                registry.register_builtin_with_effects(
                    id as u32,
                    entry.name,
                    entry.func,
                    entry.effects,
                );
                break;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    fn production_source() -> String {
        vo_source_contract::production_source_without_test_modules(include_str!("builtin.rs"))
    }

    #[test]
    fn builtin_copy_and_append_barrier_before_existing_array_mutation_052() {
        let source = production_source();
        let builtin_copy = source
            .split("fn builtin_copy(")
            .nth(1)
            .and_then(|rest| rest.split("/// append(slice, other...)").next())
            .expect("builtin_copy section");
        let copy_pos = builtin_copy
            .find("core::ptr::copy(src_ptr, dst_ptr")
            .expect("copy must preserve overlapping copy semantics");
        let barrier_pos = builtin_copy
            .find("typed_write_barrier_range_by_meta(")
            .expect("copy of root-bearing elements must use typed range barrier");
        assert!(
            barrier_pos < copy_pos,
            "copy must validate/barrier source elements before mutating the destination array"
        );

        let append_spare = source
            .split("if new_len <= dst_cap {")
            .nth(1)
            .and_then(|rest| {
                rest.split("// Go semantics: append never modifies original slice header")
                    .next()
            })
            .expect("append-with-spare-capacity section");
        let copy_pos = append_spare
            .find("core::ptr::copy(src_ptr, write_ptr")
            .expect("append-with-spare-capacity must preserve overlapping copy semantics");
        let barrier_pos = append_spare
            .find("typed_write_barrier_range_by_meta(")
            .expect("append-with-spare-capacity must use typed range barrier");
        assert!(
            barrier_pos < copy_pos,
            "append with spare capacity must validate/barrier source elements before mutating the existing backing array"
        );
    }

    #[test]
    fn builtin_spread_append_derives_elem_meta_from_containers_057() {
        let source = production_source();
        let append = source
            .split("fn builtin_slice_append_slice(")
            .nth(1)
            .and_then(|rest| rest.split("/// Interface equality comparison").next())
            .expect("builtin_slice_append_slice section");

        assert!(
            !append.contains("ValueMeta::from_raw(call.arg_u64(2)"),
            "spread append must not treat the legacy ABI metadata argument as the element layout source"
        );
        assert!(
            append.contains("let src_elem_meta = slice::elem_meta(src);"),
            "spread append must derive source element metadata from the source container"
        );
        assert!(
            append.contains("let dst_elem_meta = slice::elem_meta(dst);"),
            "spread append must derive destination element metadata from the destination container"
        );
        assert!(
            append.contains("record_contract_violation"),
            "spread append must reject source/destination layout drift before copying"
        );
    }
}
