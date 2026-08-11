//! Slice instructions: SliceNew, SliceSlice, SliceAppend

#[cfg(not(feature = "std"))]
use alloc::string::String;
#[cfg(not(feature = "std"))]
use alloc::string::ToString;
use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::objects::slice;
use vo_runtime::slot::Slot;
use vo_runtime::{ValueKind, ValueMeta};

use crate::instruction::Instruction;
use crate::vm::helpers::{makeslice_error_message, stack_get, stack_set};

/// Result of exec_slice_new: Ok(()) on success, Err(msg) on invalid parameters
pub type SliceNewResult = Result<(), String>;

#[inline]
pub fn exec_slice_new(
    stack: *mut Slot,
    bp: usize,
    inst: &Instruction,
    gc: &mut Gc,
    elem_bytes: usize,
) -> SliceNewResult {
    let meta_raw = stack_get(stack, bp + inst.b as usize) as u32;
    let len = stack_get(stack, bp + inst.c as usize) as i64;
    let cap = stack_get(stack, bp + inst.c as usize + 1) as i64;

    // Use unified validation logic from slice::create_checked
    match slice::create_checked(gc, meta_raw, elem_bytes, len, cap) {
        Ok(s) => {
            stack_set(stack, bp + inst.a as usize, s as u64);
            Ok(())
        }
        Err(code) => Err(makeslice_error_message(code).to_string()),
    }
}

/// SliceSlice: a[lo:hi] or a[lo:hi:max]
/// flags: bit0 = input is array, bit1 = has max (three-index slice),
/// bit2 = `b` is `[owner, data_ptr, elem_meta, elem_bytes, storage_stride, array_len]`.
/// Returns false on bounds error.
#[inline]
pub fn exec_slice_slice(stack: *mut Slot, bp: usize, inst: &Instruction, gc: &mut Gc) -> bool {
    let source = stack_get(stack, bp + inst.b as usize) as GcRef;
    let Ok(lo) = usize::try_from(stack_get(stack, bp + inst.c as usize)) else {
        return false;
    };
    let Ok(hi) = usize::try_from(stack_get(stack, bp + inst.c as usize + 1)) else {
        return false;
    };

    let is_array = (inst.flags & crate::instruction::SLICE_SLICE_FLAG_ARRAY) != 0;
    let has_max = (inst.flags & crate::instruction::SLICE_SLICE_FLAG_HAS_MAX) != 0;
    let inline_view = (inst.flags & crate::instruction::SLICE_SLICE_FLAG_INLINE_ARRAY_VIEW) != 0;

    // nil slice slicing returns nil only for all-zero bounds.
    if source.is_null() && !is_array {
        if has_max {
            let Ok(max) = usize::try_from(stack_get(stack, bp + inst.c as usize + 2)) else {
                return false;
            };
            if lo == 0 && hi == 0 && max == 0 {
                stack_set(stack, bp + inst.a as usize, 0);
                return true;
            }
        } else if lo == 0 && hi == 0 {
            stack_set(stack, bp + inst.a as usize, 0);
            return true;
        }
        return false;
    }

    // Safety: module verification fixes the operand kind and GC roots keep the
    // handle live; the runtime helpers still validate every slice bound.
    let result = if inline_view {
        let data_ptr = stack_get(stack, bp + inst.b as usize + 1) as *mut u8;
        let elem_meta = ValueMeta::from_raw(stack_get(stack, bp + inst.b as usize + 2) as u32);
        let Ok(elem_bytes) = usize::try_from(stack_get(stack, bp + inst.b as usize + 3)) else {
            return false;
        };
        let Ok(storage_stride) = usize::try_from(stack_get(stack, bp + inst.b as usize + 4)) else {
            return false;
        };
        let Ok(array_len) = usize::try_from(stack_get(stack, bp + inst.b as usize + 5)) else {
            return false;
        };
        if has_max {
            let Ok(max) = usize::try_from(stack_get(stack, bp + inst.c as usize + 2)) else {
                return false;
            };
            unsafe {
                slice::inline_array_slice_with_cap(
                    gc,
                    source,
                    data_ptr,
                    elem_meta,
                    elem_bytes,
                    storage_stride,
                    array_len,
                    lo,
                    hi,
                    max,
                )
            }
        } else {
            unsafe {
                slice::inline_array_slice(
                    gc,
                    source,
                    data_ptr,
                    elem_meta,
                    elem_bytes,
                    storage_stride,
                    array_len,
                    lo,
                    hi,
                )
            }
        }
    } else if is_array {
        if has_max {
            let Ok(max) = usize::try_from(stack_get(stack, bp + inst.c as usize + 2)) else {
                return false;
            };
            unsafe { slice::array_slice_with_cap(gc, source, lo, hi, max) }
        } else {
            unsafe { slice::array_slice(gc, source, lo, hi) }
        }
    } else {
        if has_max {
            let Ok(max) = usize::try_from(stack_get(stack, bp + inst.c as usize + 2)) else {
                return false;
            };
            unsafe { slice::slice_of_with_cap(gc, source, lo, hi, max) }
        } else {
            unsafe { slice::slice_of(gc, source, lo, hi) }
        }
    };

    match result {
        Some(r) => {
            stack_set(stack, bp + inst.a as usize, r as u64);
            true
        }
        None => false,
    }
}

#[inline]
pub fn exec_slice_append(
    stack: *mut Slot,
    bp: usize,
    inst: &Instruction,
    gc: &mut Gc,
    module: Option<vo_runtime::bytecode::ModuleRuntimeMetadata<'_>>,
    elem_bytes: usize,
) -> Result<(), String> {
    let s = stack_get(stack, bp + inst.b as usize) as GcRef;
    let elem_slots = elem_bytes.div_ceil(8);

    // Read elem_meta from c
    let elem_meta = ValueMeta::from_raw(stack_get(stack, bp + inst.c as usize) as u32);

    let src_start = bp + inst.c as usize + 1;

    // Safety: verified SliceAppend metadata keeps this range in the active
    // frame, and try_append consumes it before the destination slot is written.
    let val = unsafe { core::slice::from_raw_parts(stack.add(src_start), elem_slots) };
    let s = if s.is_null() {
        s
    } else {
        let Some(s) = gc.canonicalize_ref(s) else {
            return Err("SliceAppend: invalid slice handle".to_string());
        };
        if unsafe { Gc::header(s) }.kind() != ValueKind::Slice {
            return Err("SliceAppend: expected slice handle".to_string());
        }
        s
    };
    let result = unsafe { slice::try_append(gc, elem_meta, elem_bytes, s, val, module) }
        .map_err(|err| err.to_string())?;
    stack_set(stack, bp + inst.a as usize, result as u64);
    Ok(())
}
