//! os/exec package native function implementations.

#[cfg(feature = "std")]
use std::process::{Command, Stdio};
#[cfg(feature = "std")]
use std::collections::HashMap;
#[cfg(feature = "std")]
use std::sync::Mutex;

use vo_ffi_macro::vostd_fn;
use vo_runtime::ffi::{ExternCallContext, ExternResult};
#[cfg(feature = "std")]
use vo_runtime::gc::{Gc, GcRef};
#[cfg(feature = "std")]
use vo_runtime::objects::slice;
#[cfg(feature = "std")]
use vo_runtime::builtins::error_helper::{write_error_to, write_nil_error};

#[cfg(feature = "std")]
lazy_static::lazy_static! {
    static ref CHILD_PROCESSES: Mutex<HashMap<i32, std::process::Child>> = Mutex::new(HashMap::new());
    static ref NEXT_PID: Mutex<i32> = Mutex::new(1000);
}

#[cfg(feature = "std")]
fn register_child(child: std::process::Child) -> i32 {
    let mut processes = CHILD_PROCESSES.lock().unwrap();
    let mut next_pid = NEXT_PID.lock().unwrap();
    let pid = *next_pid;
    *next_pid += 1;
    processes.insert(pid, child);
    pid
}

#[cfg(feature = "std")]
fn take_child(pid: i32) -> Option<std::process::Child> {
    CHILD_PROCESSES.lock().unwrap().remove(&pid)
}

#[cfg(feature = "std")]
#[vostd_fn("os/exec", "startProcess", std)]
fn exec_start_process(call: &mut ExternCallContext) -> ExternResult {
    let path = call.arg_str(slots::ARG_PATH);
    let args_ref = call.arg_ref(slots::ARG_ARGS);
    let dir = call.arg_str(slots::ARG_DIR);
    let env_ref = call.arg_ref(slots::ARG_ENV);

    let args = read_string_slice(args_ref);
    let env = if env_ref.is_null() { None } else { Some(read_string_slice(env_ref)) };

    let mut cmd = Command::new(&path);
    if args.len() > 1 { cmd.args(&args[1..]); }
    if !dir.is_empty() { cmd.current_dir(&dir); }
    if let Some(env_vars) = env {
        cmd.env_clear();
        for var in env_vars {
            if let Some(pos) = var.find('=') {
                cmd.env(&var[..pos], &var[pos+1..]);
            }
        }
    }
    cmd.stdin(Stdio::inherit()).stdout(Stdio::inherit()).stderr(Stdio::inherit());

    match cmd.spawn() {
        Ok(child) => {
            call.ret_i64(0, register_child(child) as i64);
            write_nil_error(call, 1);
        }
        Err(e) => {
            call.ret_i64(0, -1);
            write_error_to(call, 1, &e.to_string());
        }
    }
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn read_string_slice(slice_ref: GcRef) -> Vec<String> {
    use vo_runtime::slot::slot_to_ptr;
    
    if slice_ref.is_null() {
        return Vec::new();
    }
    let len = slice::len(slice_ref);
    let mut result = Vec::with_capacity(len);
    let elem_bytes = 8; // string refs are 8 bytes (GcRef)
    for i in 0..len {
        let str_ref_raw = slice::get(slice_ref, i, elem_bytes);
        let str_ref: GcRef = slot_to_ptr(str_ref_raw);
        if !str_ref.is_null() {
            let s = vo_runtime::objects::string::as_str(str_ref).to_string();
            result.push(s);
        } else {
            result.push(String::new());
        }
    }
    result
}

#[cfg(feature = "std")]
#[vostd_fn("os/exec", "waitProcess", std)]
fn exec_wait_process(call: &mut ExternCallContext) -> ExternResult {
    let pid = call.arg_i64(slots::ARG_PID) as i32;

    let Some(mut child) = take_child(pid) else {
        call.ret_ref(0, std::ptr::null_mut());
        write_error_to(call, 1, "process not found");
        return ExternResult::Ok;
    };

    match child.wait() {
        Ok(status) => {
            let state = call.gc_alloc(5, &[]);
            unsafe {
                Gc::write_slot(state, 0, pid as u64);
                Gc::write_slot(state, 1, status.code().unwrap_or(-1) as u64);
                Gc::write_slot(state, 2, if status.code().is_some() { 1 } else { 0 });
                #[cfg(unix)]
                {
                    use std::os::unix::process::ExitStatusExt;
                    Gc::write_slot(state, 3, if status.signal().is_some() { 1 } else { 0 });
                    Gc::write_slot(state, 4, status.signal().unwrap_or(0) as u64);
                }
                #[cfg(not(unix))]
                {
                    Gc::write_slot(state, 3, 0);
                    Gc::write_slot(state, 4, 0);
                }
            }
            call.ret_ref(0, state);
            write_nil_error(call, 1);
        }
        Err(e) => {
            call.ret_ref(0, std::ptr::null_mut());
            write_error_to(call, 1, &e.to_string());
        }
    }
    ExternResult::Ok
}

#[cfg(feature = "std")]
#[vostd_fn("os/exec", "runCaptureOutput", std)]
fn exec_run_capture_output(call: &mut ExternCallContext) -> ExternResult {
    let path = call.arg_str(slots::ARG_PATH);
    let args_ref = call.arg_ref(slots::ARG_ARGS);
    let dir = call.arg_str(slots::ARG_DIR);
    let env_ref = call.arg_ref(slots::ARG_ENV);
    let combined = call.arg_i64(slots::ARG_COMBINED) != 0;

    let args = read_string_slice(args_ref);
    let env = if env_ref.is_null() { None } else { Some(read_string_slice(env_ref)) };

    let mut cmd = Command::new(&path);
    if args.len() > 1 { cmd.args(&args[1..]); }
    if !dir.is_empty() { cmd.current_dir(&dir); }
    if let Some(env_vars) = env {
        cmd.env_clear();
        for var in env_vars {
            if let Some(pos) = var.find('=') {
                cmd.env(&var[..pos], &var[pos+1..]);
            }
        }
    }

    let result = if combined { cmd.output() } else { cmd.stderr(Stdio::inherit()).output() };

    match result {
        Ok(output) => {
            let data = if combined {
                let mut out = output.stdout;
                out.extend(output.stderr);
                out
            } else {
                output.stdout
            };
            let slice_ref = call.alloc_bytes(&data);
            call.ret_ref(0, slice_ref);
            if output.status.success() {
                write_nil_error(call, 1);
            } else {
                write_error_to(call, 1, &format!("exit status {}", output.status.code().unwrap_or(-1)));
            }
        }
        Err(e) => {
            call.ret_ref(0, std::ptr::null_mut());
            write_error_to(call, 1, &e.to_string());
        }
    }
    ExternResult::Ok
}

#[cfg(feature = "std")]
vo_runtime::stdlib_register!(os_exec:
    startProcess,
    waitProcess,
    runCaptureOutput,
);
