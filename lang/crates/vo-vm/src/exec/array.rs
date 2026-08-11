//! Array instructions: ArrayNew

use vo_runtime::gc::Gc;
use vo_runtime::objects::array;
use vo_runtime::slot::Slot;
use vo_runtime::ValueMeta;

use crate::instruction::Instruction;
use crate::vm::helpers::{stack_get, stack_set};

#[inline]
pub fn exec_array_new(
    stack: *mut Slot,
    bp: usize,
    inst: &Instruction,
    gc: &mut Gc,
    elem_bytes: usize,
) -> Result<(), &'static str> {
    const SIZE_ERROR: &str = "runtime error: array size out of range";

    let meta_raw = stack_get(stack, bp + inst.b as usize) as u32;
    let elem_meta = ValueMeta::try_from_raw(meta_raw).ok_or(SIZE_ERROR)?;
    let len = usize::try_from(stack_get(stack, bp + inst.c as usize)).map_err(|_| SIZE_ERROR)?;
    u32::try_from(elem_bytes).map_err(|_| SIZE_ERROR)?;
    len.checked_mul(elem_bytes)
        .filter(|bytes| *bytes <= isize::MAX as usize)
        .ok_or(SIZE_ERROR)?;

    let arr = array::create(gc, elem_meta, elem_bytes, len);
    stack_set(stack, bp + inst.a as usize, arr as u64);
    Ok(())
}
