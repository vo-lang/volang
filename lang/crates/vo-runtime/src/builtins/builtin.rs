//! Builtin native function implementations.
//!
//! These are low-level builtin functions called directly by runtime.
//! They don't have corresponding .vo declarations and skip signature validation.
//!
//! print/println receive interface{} values (each 2 slots).
//! All args are uniformly boxed as interface by codegen.

use super::format::{format_interface_bytes_with_ctx, format_interface_with_ctx};
use crate::ffi::{ExternCallContext, ExternResult};
#[cfg(not(feature = "std"))]
use alloc::format;
#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::vec;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

/// Format all interface{} args starting from `start_slot` into a space-separated string.
/// Each arg is 2 slots: [slot0 = packed_info, slot1 = data]
fn format_args(call: &ExternCallContext, start_slot: u16) -> Vec<u8> {
    let arg_count = call.arg_count();
    let mut result = Vec::new();
    let mut slot = start_slot;

    while slot + 2 <= arg_count {
        if !result.is_empty() {
            result.push(b' ');
        }
        let slot0 = call.arg_u64(slot);
        let slot1 = call.arg_u64(slot + 1);
        result.extend_from_slice(&format_interface_bytes_with_ctx(slot0, slot1, Some(call)));
        slot += 2;
    }

    result
}

/// vo_print - print values without newline (Go builtin print semantics)
fn builtin_print(call: &mut ExternCallContext) -> ExternResult {
    call.write_output_bytes(&format_args(call, 0));
    ExternResult::Ok
}

/// vo_println - print values with newline (Go builtin println semantics)
fn builtin_println(call: &mut ExternCallContext) -> ExternResult {
    call.writeln_output_bytes(&format_args(call, 0));
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
            format!(
                "assertion failed: {}",
                crate::output::render_output_text(&msg_part)
            )
        };
        return ExternResult::Panic(msg);
    }
    ExternResult::Ok
}

unsafe fn builtin_copy_raw(call: &mut ExternCallContext) -> ExternResult {
    use crate::objects::slice;

    let dst = call.arg_ref(0);
    let src = call.arg_ref(1);

    if dst.is_null() || src.is_null() {
        call.ret_i64(0, 0);
        return ExternResult::Ok;
    }

    let dst_len = slice::len(dst);

    // Strings share the slice descriptor ABI and expose Uint8 elements.
    let src_len = slice::len(src);

    let copy_len = dst_len.min(src_len);

    if copy_len == 0 {
        call.ret_i64(0, 0);
        return ExternResult::Ok;
    }

    let dst_owner = slice::owner_ref(dst);
    let elem_meta = slice::elem_meta(dst);
    if elem_meta.value_kind().may_contain_gc_refs() {
        let elem_slots = slice::logical_elem_slots(src);
        let mut value = vec![0u64; elem_slots];
        for index in 0..copy_len {
            slice::read_logical_slots(src, index, &mut value);
            call.typed_write_barrier_by_meta(dst_owner, &value, elem_meta);
        }
    }
    slice::copy_logical_elements(dst, src, copy_len);

    call.ret_i64(0, copy_len as i64);
    ExternResult::Ok
}

fn builtin_copy(call: &mut ExternCallContext) -> ExternResult {
    // Safety: builtin dispatch keeps verified slice/string arguments rooted
    // throughout the call.
    unsafe { builtin_copy_raw(call) }
}

/// append(slice, other...) - append all elements from other slice/string
/// Works for both slice and string sources since they have identical memory layout.
unsafe fn builtin_slice_append_slice_raw(call: &mut ExternCallContext) -> ExternResult {
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
    let src_elem_bytes = slice::elem_bytes(src);
    let (elem_meta, elem_bytes) = if dst.is_null() {
        (src_elem_meta, src_elem_bytes)
    } else {
        let dst_elem_meta = slice::elem_meta(dst);
        let dst_elem_bytes = slice::elem_bytes(dst);
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
        if new_arr.is_null() {
            return ExternResult::Ok;
        }
        let result = slice::from_array_range_with_cap(call.gc(), new_arr, 0, src_len, new_cap);
        if result.is_null() {
            return ExternResult::Ok;
        }
        slice::copy_logical_elements(result, src, src_len);
        if elem_meta.value_kind().may_contain_gc_refs() {
            call.gc().mark_allocated_for_scan(new_arr);
        }
        call.ret_ref(0, result);
        return ExternResult::Ok;
    }

    let dst_len = slice::len(dst);
    let dst_cap = slice::cap(dst);
    let new_len = dst_len + src_len;

    if new_len <= dst_cap {
        // Enough capacity - write to existing backing array, return new slice header
        if elem_meta.value_kind().may_contain_gc_refs() {
            let owner = slice::owner_ref(dst);
            let elem_slots = slice::logical_elem_slots(src);
            let mut value = vec![0u64; elem_slots];
            for index in 0..src_len {
                slice::read_logical_slots(src, index, &mut value);
                call.typed_write_barrier_by_meta(owner, &value, elem_meta);
            }
        }
        // Go semantics: append never modifies original slice header
        let new_s = slice::with_new_len(call.gc(), dst, new_len);
        if new_s.is_null() {
            return ExternResult::Ok;
        }
        slice::copy_logical_elements_at(new_s, dst_len, src, 0, src_len);
        call.ret_ref(0, new_s);
    } else {
        // Need to grow - allocate new array
        let new_cap = (new_len * 2).max(4);
        let new_arr = array::create(call.gc(), elem_meta, elem_bytes, new_cap);
        if new_arr.is_null() {
            return ExternResult::Ok;
        }
        let result = slice::from_array_range_with_cap(call.gc(), new_arr, 0, new_len, new_cap);
        if result.is_null() {
            return ExternResult::Ok;
        }
        slice::copy_logical_elements_at(result, 0, dst, 0, dst_len);
        slice::copy_logical_elements_at(result, dst_len, src, 0, src_len);
        if elem_meta.value_kind().may_contain_gc_refs() {
            call.gc().mark_allocated_for_scan(new_arr);
        }
        call.ret_ref(0, result);
    }

    ExternResult::Ok
}

fn builtin_slice_append_slice(call: &mut ExternCallContext) -> ExternResult {
    // Safety: builtin dispatch keeps both slice arguments rooted and codegen
    // supplies their verified element layout.
    unsafe { builtin_slice_append_slice_raw(call) }
}

/// Interface equality comparison
/// Args: (left_slot0, left_slot1, right_slot0, right_slot1)
/// Returns: bool (1 if equal, 0 if not)
unsafe fn builtin_iface_eq_raw(call: &mut ExternCallContext) -> ExternResult {
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

fn builtin_iface_eq(call: &mut ExternCallContext) -> ExternResult {
    // Safety: interface operands originate from verified VM slots and remain
    // rooted for comparison.
    unsafe { builtin_iface_eq_raw(call) }
}

// ==================== String Conversion Functions ====================

/// int -> string (unicode code point)
fn integer_to_string(value: u64) -> String {
    u32::try_from(value)
        .ok()
        .and_then(char::from_u32)
        .unwrap_or('\u{FFFD}')
        .to_string()
}

fn conv_int_str(call: &mut ExternCallContext) -> ExternResult {
    let s = integer_to_string(call.arg_u64(0));
    let gc_ref = crate::objects::string::new_from_string(call.gc(), s);
    call.ret_ref(0, gc_ref);
    ExternResult::Ok
}

/// []byte -> string (copies because strings are immutable)
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
    call.ret_f64(0, libm::sqrt(call.arg_f64(0)));
    ExternResult::Ok
}

fn math_floor(call: &mut ExternCallContext) -> ExternResult {
    call.ret_f64(0, libm::floor(call.arg_f64(0)));
    ExternResult::Ok
}

fn math_ceil(call: &mut ExternCallContext) -> ExternResult {
    call.ret_f64(0, libm::ceil(call.arg_f64(0)));
    ExternResult::Ok
}

fn math_trunc(call: &mut ExternCallContext) -> ExternResult {
    call.ret_f64(0, libm::trunc(call.arg_f64(0)));
    ExternResult::Ok
}

fn math_fma(call: &mut ExternCallContext) -> ExternResult {
    call.ret_f64(
        0,
        libm::fma(call.arg_f64(0), call.arg_f64(1), call.arg_f64(2)),
    );
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
        name: crate::ffi::MATH_SQRT_EXTERN_NAME,
        func: math_sqrt,
        effects: crate::bytecode::ExternEffects::NONE,
    },
    BuiltinExternEntry {
        name: crate::ffi::MATH_FLOOR_EXTERN_NAME,
        func: math_floor,
        effects: crate::bytecode::ExternEffects::NONE,
    },
    BuiltinExternEntry {
        name: crate::ffi::MATH_CEIL_EXTERN_NAME,
        func: math_ceil,
        effects: crate::bytecode::ExternEffects::NONE,
    },
    BuiltinExternEntry {
        name: crate::ffi::MATH_TRUNC_EXTERN_NAME,
        func: math_trunc,
        effects: crate::bytecode::ExternEffects::NONE,
    },
    BuiltinExternEntry {
        name: crate::ffi::MATH_FMA_EXTERN_NAME,
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
) -> Result<(), crate::ffi::ExternContractError> {
    for (id, def) in crate::ffi::unique_extern_providers(externs) {
        for entry in REGISTERED_EXTERNS {
            if def.name == entry.name {
                registry.try_register_builtin_with_effects(
                    id as u32,
                    entry.name,
                    entry.func,
                    entry.effects,
                )?;
                break;
            }
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::integer_to_string;

    #[cfg(feature = "std")]
    fn gc_with_object_limit(max_objects: usize) -> crate::gc::Gc {
        crate::gc::Gc::with_memory_config(crate::gc::VmMemoryConfig {
            max_objects: Some(max_objects),
            ..crate::gc::VmMemoryConfig::default()
        })
        .expect("bounded GC configuration")
    }

    #[cfg(feature = "std")]
    fn invoke_slice_append_slice(
        gc: &mut crate::gc::Gc,
        module: &crate::Module,
        dst: crate::gc::GcRef,
        src: crate::gc::GcRef,
        initial_return: u64,
    ) -> u64 {
        use crate::ffi::{
            ExternCallContext, ExternFiberInputs, ExternInvoke, ExternResult, ExternWorld,
            SentinelErrorCache,
        };

        let mut stack = [dst as u64, src as u64, 0, initial_return];
        let invoke = ExternInvoke {
            extern_id: 0,
            bp: 0,
            arg_start: 0,
            arg_slots: 3,
            ret_start: 3,
            ret_slots: 1,
        };
        let mut itab_cache = crate::itab::ItabCache::new();
        let program_args = Vec::new();
        let output = crate::output::CaptureSink::new();
        let mut sentinel_errors = SentinelErrorCache::new();
        let mut host_output = None;
        let world = ExternWorld::new(
            gc,
            module.into(),
            &mut itab_cache,
            &program_args,
            output.as_ref(),
            &mut sentinel_errors,
            &mut host_output,
        );
        let mut call =
            ExternCallContext::new(&mut stack, invoke, world, ExternFiberInputs::default());

        assert!(matches!(
            unsafe { super::builtin_slice_append_slice_raw(&mut call) },
            ExternResult::Ok
        ));
        drop(call);
        stack[3]
    }

    #[test]
    fn integer_to_string_rejects_negative_wrapped_large_and_surrogate_values() {
        assert_eq!(integer_to_string('A' as u64), "A");
        assert_eq!(integer_to_string((-1_i64) as u64), "\u{FFFD}");
        assert_eq!(integer_to_string(0x1_0000_0041), "\u{FFFD}");
        assert_eq!(integer_to_string(0xD800), "\u{FFFD}");
        assert_eq!(integer_to_string(0x10FFFF), "\u{10FFFF}");
    }

    #[cfg(feature = "std")]
    #[test]
    fn spread_append_nil_destination_propagates_header_allocation_failure() {
        use crate::gc::MemoryError;
        use crate::objects::slice;
        use crate::{ValueKind, ValueMeta};

        let mut gc = gc_with_object_limit(3);
        let module = crate::Module::new("spread-append-oom".to_string());
        let meta = ValueMeta::new(0, ValueKind::Int64);
        let src = slice::create(&mut gc, meta, crate::slot::SLOT_BYTES, 1, 1);
        unsafe { slice::set(src, 0, 7, crate::slot::SLOT_BYTES) };

        let returned =
            invoke_slice_append_slice(&mut gc, &module, core::ptr::null_mut(), src, 0xfeed);

        assert_eq!(returned, 0xfeed);
        assert_eq!(gc.last_memory_error(), Some(MemoryError::MetadataExhausted));
    }

    #[cfg(feature = "std")]
    #[test]
    fn spread_append_spare_capacity_does_not_mutate_on_header_oom() {
        use crate::gc::MemoryError;
        use crate::objects::{array, slice};
        use crate::{ValueKind, ValueMeta};

        let mut gc = gc_with_object_limit(4);
        let module = crate::Module::new("spread-append-oom".to_string());
        let meta = ValueMeta::new(0, ValueKind::Int64);
        let dst = slice::create(&mut gc, meta, crate::slot::SLOT_BYTES, 1, 2);
        let src = slice::create(&mut gc, meta, crate::slot::SLOT_BYTES, 1, 1);
        let backing = unsafe { slice::array_ref(dst) };
        unsafe {
            slice::set(dst, 0, 11, crate::slot::SLOT_BYTES);
            slice::set(dst, 1, 42, crate::slot::SLOT_BYTES);
            slice::set(src, 0, 7, crate::slot::SLOT_BYTES);
        }

        let returned = invoke_slice_append_slice(&mut gc, &module, dst, src, 0xfeed);

        assert_eq!(returned, 0xfeed);
        assert_eq!(
            unsafe { array::get(backing, 1, crate::slot::SLOT_BYTES) },
            42
        );
        assert_eq!(unsafe { slice::len(dst) }, 1);
        assert_eq!(gc.last_memory_error(), Some(MemoryError::MetadataExhausted));
    }

    #[cfg(feature = "std")]
    #[test]
    fn spread_append_growth_propagates_second_allocation_failure() {
        use crate::gc::MemoryError;
        use crate::objects::{array, slice};
        use crate::{ValueKind, ValueMeta};

        let mut gc = gc_with_object_limit(5);
        let module = crate::Module::new("spread-append-oom".to_string());
        let meta = ValueMeta::new(0, ValueKind::Int64);
        let dst = slice::create(&mut gc, meta, crate::slot::SLOT_BYTES, 1, 1);
        let src = slice::create(&mut gc, meta, crate::slot::SLOT_BYTES, 1, 1);
        let backing = unsafe { slice::array_ref(dst) };
        unsafe {
            slice::set(dst, 0, 11, crate::slot::SLOT_BYTES);
            slice::set(src, 0, 7, crate::slot::SLOT_BYTES);
        }

        let returned = invoke_slice_append_slice(&mut gc, &module, dst, src, 0xfeed);

        assert_eq!(returned, 0xfeed);
        assert_eq!(
            unsafe { array::get(backing, 0, crate::slot::SLOT_BYTES) },
            11
        );
        assert_eq!(gc.last_memory_error(), Some(MemoryError::MetadataExhausted));
    }
}
