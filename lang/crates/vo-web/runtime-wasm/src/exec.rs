//! os/exec package WASM stub — all operations return ERR_NOT_SUPPORTED.
//!
//! Process spawning is not available in the browser sandbox. The stubs return
//! proper errors so Vo handler programs that call os/exec in WASM fail
//! gracefully instead of panicking with NotRegistered.

use vo_runtime::builtins::error_helper::write_error_to;
use vo_runtime::bytecode::ExternDef;
use vo_runtime::ffi::{ExternCallContext, ExternContractError, ExternRegistry, ExternResult};

const ERR: &str = "operation not supported on wasm";

// startProcess → (i64 pid, i64 opaqueHandle, error)
fn exec_start_process(call: &mut ExternCallContext) -> ExternResult {
    call.ret_i64(0, -1);
    call.ret_i64(1, 0);
    write_error_to(call, 2, ERR);
    ExternResult::Ok
}

// isExecutable → (bool, error)
fn exec_is_executable(call: &mut ExternCallContext) -> ExternResult {
    call.ret_bool(0, false);
    write_error_to(call, 1, ERR);
    ExternResult::Ok
}

// killProcess → error
fn exec_kill_process(call: &mut ExternCallContext) -> ExternResult {
    write_error_to(call, 0, ERR);
    ExternResult::Ok
}

// waitProcess → (*ProcessState, error)
fn exec_wait_process(call: &mut ExternCallContext) -> ExternResult {
    call.ret_ref(0, std::ptr::null_mut());
    write_error_to(call, 1, ERR);
    ExternResult::Ok
}

// =============================================================================
// Registration
// =============================================================================

pub fn register_externs(
    registry: &mut ExternRegistry,
    externs: &[ExternDef],
) -> Result<(), ExternContractError> {
    let mut seen_names = std::collections::BTreeSet::new();
    for (id, def) in externs.iter().enumerate() {
        if !seen_names.insert(def.name.as_str()) {
            continue;
        }
        match def.name.as_str() {
            vo_runtime::vo_extern_name!("os/exec", "startProcess") => {
                crate::register_wasm_host(registry, id as u32, &def.name, exec_start_process)?
            }
            vo_runtime::vo_extern_name!("os/exec", "isExecutable") => {
                crate::register_wasm_host(registry, id as u32, &def.name, exec_is_executable)?
            }
            vo_runtime::vo_extern_name!("os/exec", "killProcess") => {
                crate::register_wasm_host(registry, id as u32, &def.name, exec_kill_process)?
            }
            vo_runtime::vo_extern_name!("os/exec", "waitProcess") => {
                crate::register_wasm_host(registry, id as u32, &def.name, exec_wait_process)?
            }
            _ => {}
        }
    }
    Ok(())
}
