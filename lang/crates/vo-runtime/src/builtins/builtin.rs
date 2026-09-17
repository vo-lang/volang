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
use alloc::vec::Vec;

mod sequence;
use sequence::SequenceSource;

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

    let src = SequenceSource::new(src);
    let src_len = src.len();

    let copy_len = dst_len.min(src_len);

    if copy_len == 0 {
        call.ret_i64(0, 0);
        return ExternResult::Ok;
    }

    let dst_owner = slice::owner_ref(dst);
    let elem_meta = slice::elem_meta(dst);
    src.barrier_to(call, dst_owner, copy_len, elem_meta);
    src.copy_to(dst, 0, copy_len);

    call.ret_i64(0, copy_len as i64);
    ExternResult::Ok
}

fn builtin_copy(call: &mut ExternCallContext) -> ExternResult {
    // Safety: builtin dispatch keeps verified slice/string arguments rooted
    // throughout the call.
    unsafe { builtin_copy_raw(call) }
}

/// append(slice, other...) - append all elements from other slice/string
/// Source access follows each kind's descriptor and logical element layout.
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

    let source = SequenceSource::new(src);
    let src_len = source.len();
    if src_len == 0 {
        call.ret_ref(0, dst);
        return ExternResult::Ok;
    }

    let (src_elem_meta, src_elem_bytes) = source.element_layout();
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
        source.copy_to(result, 0, src_len);
        if elem_meta.value_kind().may_contain_gc_refs() {
            call.gc().mark_allocated_for_scan(new_arr);
        }
        call.ret_ref(0, result);
        return ExternResult::Ok;
    }

    let dst_len = slice::len(dst);
    let dst_cap = slice::cap(dst);
    let Some(new_len) = dst_len.checked_add(src_len) else {
        call.gc()
            .record_allocation_failure(crate::gc::MemoryError::AllocationSizeOverflow);
        return ExternResult::Ok;
    };

    if new_len <= dst_cap {
        // Enough capacity - write to existing backing array, return new slice header
        source.barrier_to(call, slice::owner_ref(dst), src_len, elem_meta);
        // Go semantics: append never modifies original slice header
        let new_s = slice::with_new_len(call.gc(), dst, new_len);
        if new_s.is_null() {
            return ExternResult::Ok;
        }
        source.copy_to(new_s, dst_len, src_len);
        call.ret_ref(0, new_s);
    } else {
        // Need to grow - allocate new array
        let Some(new_cap) = new_len.checked_mul(2).map(|capacity| capacity.max(4)) else {
            call.gc()
                .record_allocation_failure(crate::gc::MemoryError::AllocationSizeOverflow);
            return ExternResult::Ok;
        };
        let new_arr = array::create(call.gc(), elem_meta, elem_bytes, new_cap);
        if new_arr.is_null() {
            return ExternResult::Ok;
        }
        let result = slice::from_array_range_with_cap(call.gc(), new_arr, 0, new_len, new_cap);
        if result.is_null() {
            return ExternResult::Ok;
        }
        slice::copy_logical_elements_at(result, 0, dst, 0, dst_len);
        source.copy_to(result, dst_len, src_len);
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

macro_rules! extern_name {
    (canonical($package:literal, $function:literal)) => {
        crate::vo_extern_name!($package, $function)
    };
    (internal($name:literal)) => {
        $name
    };
}
macro_rules! registered_externs {
    ($(($kind:ident($($name:literal),+), $function:ident, $effects:expr)),* $(,)?) => {
        const REGISTERED_EXTERNS: &[BuiltinExternEntry] = &[$(BuiltinExternEntry {
            name: extern_name!($kind($($name),+)),
            func: $function,
            effects: $effects,
        }),*];
    };
}
vo_common_core::vo_builtin_extern_contracts!(registered_externs);

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
        invoke_sequence_builtin(
            gc,
            module,
            dst,
            src,
            initial_return,
            super::builtin_slice_append_slice_raw,
        )
    }

    #[cfg(feature = "std")]
    fn invoke_sequence_builtin(
        gc: &mut crate::gc::Gc,
        module: &crate::Module,
        dst: crate::gc::GcRef,
        src: crate::gc::GcRef,
        initial_return: u64,
        operation: unsafe fn(&mut crate::ffi::ExternCallContext) -> crate::ffi::ExternResult,
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

        assert!(matches!(unsafe { operation(&mut call) }, ExternResult::Ok));
        drop(call);
        stack[3]
    }

    #[cfg(feature = "std")]
    #[test]
    fn compact_string_sources_copy_and_append_to_packed_and_flat_bytes() {
        use crate::gc::Gc;
        use crate::objects::{slice, string};
        use crate::{ValueKind, ValueMeta};

        for variant in 0..4 {
            let mut gc = Gc::new();
            let module = crate::Module::new("string-sequence".to_string());
            let source = string::create(&mut gc, b"x\x00\xffbcy");
            let source = unsafe { string::slice_of(&mut gc, source, 1, 5) }.unwrap();
            let byte_meta = ValueMeta::new(0, ValueKind::Uint8);
            let dst = match variant {
                0 => core::ptr::null_mut(),
                1 => slice::create(&mut gc, byte_meta, 1, 1, 1),
                2 => slice::create(&mut gc, byte_meta, 1, 1, 8),
                _ => {
                    let owner = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 8);
                    unsafe {
                        slice::from_inline_array_range_with_cap(
                            &mut gc,
                            owner,
                            owner.cast(),
                            8,
                            0,
                            1,
                            8,
                            byte_meta,
                            1,
                            8,
                        )
                    }
                }
            };
            if !dst.is_null() {
                unsafe { slice::set(dst, 0, u64::from(b'a'), 1) };
            }
            let result =
                invoke_slice_append_slice(&mut gc, &module, dst, source, 0) as crate::gc::GcRef;
            let expected: &[u8] = if variant == 0 {
                b"\x00\xffbc"
            } else {
                b"a\x00\xffbc"
            };
            assert_eq!(unsafe { slice::byte_vec(result) }, expected);
            assert_eq!(unsafe { string::to_bytes(source) }, b"\x00\xffbc");
            assert_eq!(unsafe { slice::len(dst) }, usize::from(variant != 0));
            if variant >= 2 {
                assert_eq!(unsafe { slice::owner_ref(result) }, unsafe {
                    slice::owner_ref(dst)
                });
            }
            let destination = unsafe { slice::with_new_len(&mut gc, result, 2) };
            let copied = invoke_sequence_builtin(
                &mut gc,
                &module,
                destination,
                source,
                u64::MAX,
                super::builtin_copy_raw,
            );
            assert_eq!(copied, 2);
            assert_eq!(unsafe { slice::byte_vec(destination) }, b"\x00\xff");
            assert_eq!(unsafe { string::to_bytes(source) }, b"\x00\xffbc");
        }
    }

    #[cfg(feature = "std")]
    #[test]
    fn spread_append_checks_length_and_capacity_overflow_before_allocation() {
        use crate::gc::{Gc, MemoryError};
        use crate::objects::slice;
        use crate::{ValueKind, ValueMeta};

        for length in [usize::MAX, usize::MAX / 2] {
            let mut gc = Gc::new();
            let module = crate::Module::new("spread-overflow".to_string());
            // Zero-width elements exercise large logical lengths without large
            // allocations or copying work. Both descriptors remain valid.
            let meta = ValueMeta::new(0, ValueKind::Struct);
            let dst = slice::create(&mut gc, meta, 0, length, length);
            let src = slice::create(&mut gc, meta, 0, 2, 2);
            assert!(!dst.is_null() && !src.is_null());
            let before = gc.memory_stats();
            let result = invoke_slice_append_slice(&mut gc, &module, dst, src, 0xfeed);
            assert_eq!(result, 0xfeed);
            assert_eq!(
                gc.last_memory_error(),
                Some(MemoryError::AllocationSizeOverflow)
            );
            assert_eq!(gc.memory_stats().object_count, before.object_count);
            assert_eq!(
                gc.memory_stats().allocation_failures,
                before.allocation_failures + 1
            );
            assert_eq!(unsafe { slice::len(dst) }, length);
        }
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
