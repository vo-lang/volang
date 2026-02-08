//! External function call: CallExtern
//!
//! Uses ExternRegistry from vo-runtime-core for extern function dispatch.

#[cfg(not(feature = "std"))]
use alloc::format;
#[cfg(not(feature = "std"))]
use alloc::string::String;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use crate::bytecode::ExternDef;
use crate::instruction::Instruction;
use crate::vm::ExecResult;

pub use vo_runtime::ffi::ExternRegistry;
use vo_runtime::ffi::ExternResult;
use vo_runtime::SentinelErrorCache;
use vo_common_core::bytecode::WellKnownTypes;
use vo_runtime::gc::Gc;
use vo_common_core::bytecode::Module;
#[cfg(feature = "std")]
use vo_runtime::io::IoRuntime;
#[cfg(feature = "std")]
use vo_runtime::io::IoToken;

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
    itab_cache: &mut vo_runtime::itab::ItabCache,
    func_defs: &[vo_common_core::bytecode::FunctionDef],
    module: &Module,
    vm: *mut core::ffi::c_void,
    fiber: *mut core::ffi::c_void,
    call_closure_fn: Option<vo_runtime::ffi::ClosureCallFn>,
    fiber_panic_msg: &mut Option<String>,
    well_known: &WellKnownTypes,
    program_args: &[String],
    sentinel_errors: &mut SentinelErrorCache,
    #[cfg(feature = "std")]
    io: &mut IoRuntime,
    #[cfg(feature = "std")]
    resume_io_token: Option<IoToken>,
    closure_replay_results: Vec<Vec<u64>>,
    closure_replay_panicked: bool,
) -> ExecResult {
    // CallExtern: a=dst, b=extern_id, c=args_start, flags=arg_count
    let extern_id = inst.b as u32;
    let arg_start = inst.c;
    let arg_count = inst.flags as u16;

    if extern_id as usize >= externs.len() {
        *fiber_panic_msg = Some(format!("extern function id {} out of range", extern_id));
        return ExecResult::Panic;
    }
    let extern_def = &externs[extern_id as usize];
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
        itab_cache,
        func_defs,
        module,
        vm,
        fiber,
        call_closure_fn,
        well_known,
        program_args,
        sentinel_errors,
        #[cfg(feature = "std")]
        io,
        #[cfg(feature = "std")]
        resume_io_token,
        closure_replay_results,
        closure_replay_panicked,
    );

    match result {
        ExternResult::Ok => ExecResult::FrameChanged,
        ExternResult::Yield => ExecResult::TimesliceExpired,
        ExternResult::Block => ExecResult::Block(crate::fiber::BlockReason::Queue),
        #[cfg(feature = "std")]
        ExternResult::WaitIo { token } => ExecResult::Block(crate::fiber::BlockReason::Io(token)),
        ExternResult::Panic(msg) => {
            *fiber_panic_msg = Some(msg);
            ExecResult::Panic
        }
        ExternResult::NotRegistered(id) => {
            *fiber_panic_msg = Some(format!(
                "extern function '{}' (id={}) not registered",
                extern_def.name, id
            ));
            ExecResult::Panic
        }
        ExternResult::CallClosure { closure_ref, args } => {
            ExecResult::CallClosure { closure_ref, args }
        }
    }
}
