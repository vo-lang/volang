//! Global variable instructions: GlobalGet, GlobalGetN, GlobalSet, GlobalSetN

#[cfg(not(feature = "std"))]
use alloc::{format, string::String};
#[cfg(feature = "std")]
use std::string::String;

use crate::instruction::Instruction;
use crate::vm::helpers::{stack_get, stack_set};
use vo_runtime::slot::Slot;

fn check_global_range(
    op: &'static str,
    globals_len: usize,
    start: usize,
    count: usize,
) -> Result<(), String> {
    let Some(end) = start.checked_add(count) else {
        return Err(format!(
            "{op} global range {start}..+{count} overflows global index space"
        ));
    };
    if end > globals_len {
        return Err(format!(
            "{op} global range {start}..{end} out of bounds for {globals_len} global slots"
        ));
    }
    Ok(())
}

#[inline]
pub fn exec_global_get(
    stack: *mut Slot,
    bp: usize,
    inst: &Instruction,
    globals: &[u64],
) -> Result<(), String> {
    let idx = inst.b as usize;
    check_global_range("GlobalGet", globals.len(), idx, 1)?;
    let Some(&val) = globals.get(idx) else {
        return Err(format!(
            "GlobalGet global index {idx} out of bounds for {} global slots",
            globals.len()
        ));
    };
    stack_set(stack, bp + inst.a as usize, val);
    Ok(())
}

#[inline]
pub fn exec_global_get_n(
    stack: *mut Slot,
    bp: usize,
    inst: &Instruction,
    globals: &[u64],
) -> Result<(), String> {
    let count = inst.flags as usize;
    let dst_start = bp + inst.a as usize;
    let src_start = inst.b as usize;
    check_global_range("GlobalGetN", globals.len(), src_start, count)?;

    let Some(src) = globals.get(src_start..src_start + count) else {
        return Err(format!(
            "GlobalGetN global range {}..{} out of bounds for {} global slots",
            src_start,
            src_start + count,
            globals.len()
        ));
    };
    for (i, &val) in src.iter().enumerate() {
        stack_set(stack, dst_start + i, val);
    }
    Ok(())
}

#[inline]
pub fn exec_global_set(
    stack: *const Slot,
    bp: usize,
    inst: &Instruction,
    globals: &mut [u64],
) -> Result<(), String> {
    let idx = inst.a as usize;
    check_global_range("GlobalSet", globals.len(), idx, 1)?;
    let val = stack_get(stack, bp + inst.b as usize);
    let globals_len = globals.len();
    let Some(slot) = globals.get_mut(idx) else {
        return Err(format!(
            "GlobalSet global index {idx} out of bounds for {globals_len} global slots"
        ));
    };
    *slot = val;
    Ok(())
}

#[inline]
pub fn exec_global_set_n(
    stack: *const Slot,
    bp: usize,
    inst: &Instruction,
    globals: &mut [u64],
) -> Result<(), String> {
    let count = inst.flags as usize;
    let src_start = bp + inst.b as usize;
    let dst_start = inst.a as usize;
    check_global_range("GlobalSetN", globals.len(), dst_start, count)?;

    let globals_len = globals.len();
    let Some(dst) = globals.get_mut(dst_start..dst_start + count) else {
        return Err(format!(
            "GlobalSetN global range {}..{} out of bounds for {globals_len} global slots",
            dst_start,
            dst_start + count
        ));
    };
    for (i, slot) in dst.iter_mut().enumerate() {
        *slot = stack_get(stack, src_start + i);
    }
    Ok(())
}
