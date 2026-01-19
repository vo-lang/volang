//! External function call: CallExtern
//!
//! Uses ExternRegistry from vo-runtime-core for extern function dispatch.

#[cfg(not(feature = "std"))]
use alloc::string::String;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use crate::bytecode::ExternDef;
use crate::instruction::Instruction;
use crate::vm::ExecResult;

pub use vo_runtime::ffi::ExternRegistry;
use vo_runtime::ffi::ExternResult;
use vo_runtime::gc::Gc;
use vo_common_core::bytecode::Module;

pub fn exec_call_extern(
    stack: &mut Vec<u64>,
    bp: usize,
    inst: &Instruction,
    externs: &[ExternDef],
    registry: &ExternRegistry,
    gc: &mut Gc,
    struct_metas: &[vo_common_core::bytecode::StructMeta],
    interface_metas: &[vo_common_core::bytecode::InterfaceMeta],
    named_type_metas: &[vo_common_core::bytecode::NamedTypeMeta],
    runtime_types: &[vo_runtime::RuntimeType],
    well_known: &vo_common_core::bytecode::WellKnownTypes,
    itab_cache: &mut vo_runtime::itab::ItabCache,
    func_defs: &[vo_common_core::bytecode::FunctionDef],
    module: &Module,
    vm: *mut core::ffi::c_void,
    fiber: *mut core::ffi::c_void,
    call_closure_fn: Option<vo_runtime::ffi::ClosureCallFn>,
    fiber_panic_msg: &mut Option<String>,
    program_args: &[String],
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
        struct_metas,
        interface_metas,
        named_type_metas,
        runtime_types,
        well_known,
        itab_cache,
        func_defs,
        module,
        vm,
        fiber,
        call_closure_fn,
        program_args,
    );

    match result {
        ExternResult::Ok => ExecResult::Continue,
        ExternResult::Yield => ExecResult::Yield,
        ExternResult::Panic(msg) => {
            *fiber_panic_msg = Some(msg);
            ExecResult::Panic
        }
    }
}
