//! External function call: CallExtern
//!
//! Uses ExternRegistry from vo-runtime-core for extern function dispatch.

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use crate::bytecode::ExternDef;
use crate::instruction::Instruction;
use crate::vm::ExecResult;

pub use vo_runtime::ffi::ExternRegistry;
use vo_runtime::ffi::ExternResult;
use vo_runtime::gc::Gc;

pub fn exec_call_extern(
    stack: &mut Vec<u64>,
    bp: usize,
    inst: &Instruction,
    externs: &[ExternDef],
    registry: &ExternRegistry,
    gc: &mut Gc,
) -> ExecResult {
    // CallExtern: a=dst, b=extern_id, c=args_start, flags=arg_count
    let extern_id = inst.b as u32;
    let arg_start = inst.c;
    let arg_count = inst.flags as u16;

    if extern_id as usize >= externs.len() {
        return ExecResult::Panic;
    }
    let _extern_def = &externs[extern_id as usize];
    let dst = inst.a;

    // Call through ExternRegistry using ExternCall API
    // ret_start = dst so return value goes to the right register
    let result = registry.call(
        extern_id,
        stack,
        bp,
        arg_start,
        arg_count,
        dst,
        gc,
    );

    match result {
        ExternResult::Ok => ExecResult::Continue,
        ExternResult::Yield => ExecResult::Yield,
        ExternResult::Panic(_) => ExecResult::Panic,
    }
}
