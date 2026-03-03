//! os/exec package WASM stub — all operations return ERR_NOT_SUPPORTED.
//!
//! Process spawning is not available in the browser sandbox. The stubs return
//! proper errors so Vo handler programs that call os/exec in WASM fail
//! gracefully instead of panicking with NotRegistered.

use vo_runtime::bytecode::ExternDef;
use vo_runtime::ffi::{ExternCallContext, ExternRegistry, ExternResult};
use vo_runtime::builtins::error_helper::write_error_to;

const ERR: &str = "operation not supported on wasm";

// startProcess → (i64 pid, error)
fn exec_start_process(call: &mut ExternCallContext) -> ExternResult {
    call.ret_i64(0, -1);
    write_error_to(call, 1, ERR);
    ExternResult::Ok
}

// waitProcess → (*ProcessState, error)
fn exec_wait_process(call: &mut ExternCallContext) -> ExternResult {
    call.ret_ref(0, std::ptr::null_mut());
    write_error_to(call, 1, ERR);
    ExternResult::Ok
}

// runCaptureOutput → ([]byte output, i64 pid, i64 exitCode, error)
fn exec_run_capture_output(call: &mut ExternCallContext) -> ExternResult {
    call.ret_ref(0, std::ptr::null_mut());
    call.ret_i64(1, 0);
    call.ret_i64(2, -1);
    write_error_to(call, 3, ERR);
    ExternResult::Ok
}

// =============================================================================
// Registration
// =============================================================================

pub fn register_externs(registry: &mut ExternRegistry, externs: &[ExternDef]) {
    for (id, def) in externs.iter().enumerate() {
        match def.name.as_str() {
            "os_exec_startProcess"      => registry.register(id as u32, exec_start_process),
            "os_exec_waitProcess"       => registry.register(id as u32, exec_wait_process),
            "os_exec_runCaptureOutput"  => registry.register(id as u32, exec_run_capture_output),
            _ => {}
        }
    }
}
