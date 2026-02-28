//! os/exec package native function implementations.

#[cfg(feature = "std")]
use std::process::{Command, Stdio};
#[cfg(all(feature = "std", unix))]
use std::io::Read;
#[cfg(feature = "std")]
use std::collections::HashMap;
#[cfg(feature = "std")]
use std::sync::Mutex;
#[cfg(all(feature = "std", unix))]
use std::os::fd::{FromRawFd, IntoRawFd};
#[cfg(all(feature = "std", unix))]
use std::fs::File;

use vo_ffi_macro::vostd_fn;
use vo_runtime::ffi::{ExternCallContext, ExternResult};
#[cfg(feature = "std")]
use vo_runtime::gc::{Gc, GcRef};
#[cfg(feature = "std")]
use vo_runtime::objects::slice;
#[cfg(feature = "std")]
use vo_runtime::builtins::error_helper::{write_error_to, write_nil_error};
#[cfg(feature = "std")]
use vo_runtime::slot::slot_to_ptr;

#[cfg(feature = "std")]
lazy_static::lazy_static! {
    static ref CHILD_PROCESSES: Mutex<HashMap<i32, std::process::Child>> = Mutex::new(HashMap::new());
}

#[cfg(feature = "std")]
fn register_child(child: std::process::Child) -> i32 {
    let pid = child.id() as i32;
    let mut processes = CHILD_PROCESSES.lock().unwrap();
    let prev = processes.insert(pid, child);
    assert!(prev.is_none(), "duplicate child pid in process table: {pid}");
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
    let stdin_fd = call.arg_i64(slots::ARG_STDIN) as i32;
    let stdout_fd = call.arg_i64(slots::ARG_STDOUT) as i32;
    let stderr_fd = call.arg_i64(slots::ARG_STDERR) as i32;

    let args = read_string_slice(args_ref);
    let env = if env_ref.is_null() { None } else { Some(read_string_slice(env_ref)) };

    let mut cmd = Command::new(&path);
    if let Err(e) = configure_command(&mut cmd, &args, &dir, env.as_deref()) {
        call.ret_i64(0, -1);
        write_error_to(call, 1, &e.to_string());
        return ExternResult::Ok;
    }
    if let Err(e) = configure_stdio(&mut cmd, stdin_fd, stdout_fd, stderr_fd) {
        call.ret_i64(0, -1);
        write_error_to(call, 1, &e.to_string());
        return ExternResult::Ok;
    }

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
fn configure_command(
    cmd: &mut Command,
    args: &[String],
    dir: &str,
    env: Option<&[String]>,
) -> std::io::Result<()> {
    if args.len() > 1 {
        cmd.args(&args[1..]);
    }
    if !dir.is_empty() {
        cmd.current_dir(dir);
    }
    if let Some(env_vars) = env {
        cmd.env_clear();
        for var in env_vars {
            if let Some(pos) = var.find('=') {
                cmd.env(&var[..pos], &var[pos + 1..]);
            }
            // Go: entries without '=' are silently passed through (ignored by most OS)
        }
    }
    Ok(())
}

#[cfg(feature = "std")]
fn configure_stdio(cmd: &mut Command, stdin_fd: i32, stdout_fd: i32, stderr_fd: i32) -> std::io::Result<()> {
    cmd.stdin(fd_to_stdio(stdin_fd)?);
    cmd.stdout(fd_to_stdio(stdout_fd)?);
    cmd.stderr(fd_to_stdio(stderr_fd)?);
    Ok(())
}

#[cfg(feature = "std")]
fn fd_to_stdio(fd: i32) -> std::io::Result<Stdio> {
    if fd < 0 {
        return Ok(Stdio::inherit());
    }
    let raw_fd = crate::os::raw_fd_from_handle(fd).ok_or_else(|| {
        std::io::Error::new(std::io::ErrorKind::NotFound, "invalid file descriptor")
    })?;
    dup_fd_to_stdio(raw_fd)
}

#[cfg(all(feature = "std", unix))]
fn dup_fd_to_stdio(fd: i32) -> std::io::Result<Stdio> {
    let dup = unsafe { libc::dup(fd) };
    if dup < 0 {
        return Err(std::io::Error::last_os_error());
    }

    let flags = unsafe { libc::fcntl(dup, libc::F_GETFL) };
    if flags < 0 {
        let err = std::io::Error::last_os_error();
        unsafe { libc::close(dup) };
        return Err(err);
    }
    let ret = unsafe { libc::fcntl(dup, libc::F_SETFL, flags & !libc::O_NONBLOCK) };
    if ret < 0 {
        let err = std::io::Error::last_os_error();
        unsafe { libc::close(dup) };
        return Err(err);
    }

    Ok(unsafe { Stdio::from_raw_fd(dup) })
}

#[cfg(all(feature = "std", not(unix)))]
fn dup_fd_to_stdio(_fd: i32) -> std::io::Result<Stdio> {
    Err(std::io::Error::new(
        std::io::ErrorKind::Unsupported,
        "fd-based stdio redirection is only supported on unix",
    ))
}

#[cfg(feature = "std")]
fn read_string_slice(slice_ref: GcRef) -> Vec<String> {
    if slice_ref.is_null() {
        return Vec::new();
    }
    let len = slice::len(slice_ref);
    let mut result = Vec::with_capacity(len);
    for i in 0..len {
        let str_ref_raw = slice::get(slice_ref, i, std::mem::size_of::<GcRef>());
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

// ProcessState slot layout (must match exec.vo ProcessState struct field order)
#[cfg(feature = "std")]
const PS_PID: usize = 0;
#[cfg(feature = "std")]
const PS_EXIT_CODE: usize = 1;
#[cfg(feature = "std")]
const PS_EXITED: usize = 2;
#[cfg(feature = "std")]
const PS_SIGNALED: usize = 3;
#[cfg(feature = "std")]
const PS_SIGNAL: usize = 4;

#[cfg(feature = "std")]
fn write_process_state(
    call: &mut ExternCallContext,
    pid: i32,
    status: &std::process::ExitStatus,
) -> GcRef {
    let state = call.gc_alloc_struct("os/exec.ProcessState");
    unsafe {
        Gc::write_slot(state, PS_PID, pid as u64);
        Gc::write_slot(state, PS_EXIT_CODE, status.code().unwrap_or(-1) as u64);
        Gc::write_slot(state, PS_EXITED, if status.code().is_some() { 1 } else { 0 });
        #[cfg(unix)]
        {
            use std::os::unix::process::ExitStatusExt;
            Gc::write_slot(state, PS_SIGNALED, if status.signal().is_some() { 1 } else { 0 });
            Gc::write_slot(state, PS_SIGNAL, status.signal().unwrap_or(0) as u64);
        }
        #[cfg(not(unix))]
        {
            Gc::write_slot(state, PS_SIGNALED, 0);
            Gc::write_slot(state, PS_SIGNAL, 0);
        }
    }
    state
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
            let state = write_process_state(call, pid, &status);
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
    // Returns ([]byte, int, int, error) = (output, pid, exitCode, err)
    let path = call.arg_str(slots::ARG_PATH);
    let args_ref = call.arg_ref(slots::ARG_ARGS);
    let dir = call.arg_str(slots::ARG_DIR);
    let env_ref = call.arg_ref(slots::ARG_ENV);
    let combined = call.arg_i64(slots::ARG_COMBINED) != 0;

    let args = read_string_slice(args_ref);
    let env = if env_ref.is_null() { None } else { Some(read_string_slice(env_ref)) };

    let mut cmd = Command::new(&path);
    if let Err(e) = configure_command(&mut cmd, &args, &dir, env.as_deref()) {
        call.ret_ref(0, std::ptr::null_mut());
        call.ret_i64(1, 0);
        call.ret_i64(2, -1);
        write_error_to(call, 3, &e.to_string());
        return ExternResult::Ok;
    }

    let result = if combined {
        run_combined_output(cmd)
    } else {
        cmd.stderr(Stdio::inherit())
            .stdout(Stdio::piped())
            .spawn()
            .and_then(|child| {
                let pid = child.id();
                let output = child.wait_with_output()?;
                Ok((output.stdout, output.status, pid))
            })
    };

    match result {
        Ok((data, status, pid)) => {
            let slice_ref = call.alloc_bytes(&data);
            call.ret_ref(0, slice_ref);
            call.ret_i64(1, pid as i64);
            call.ret_i64(2, status.code().unwrap_or(-1) as i64);
            if status.success() {
                write_nil_error(call, 3);
            } else {
                write_error_to(call, 3, &format!("exit status {}", status.code().unwrap_or(-1)));
            }
        }
        Err(e) => {
            call.ret_ref(0, std::ptr::null_mut());
            call.ret_i64(1, 0);
            call.ret_i64(2, -1);
            write_error_to(call, 3, &e.to_string());
        }
    }
    ExternResult::Ok
}

#[cfg(all(feature = "std", unix))]
fn run_combined_output(mut cmd: Command) -> std::io::Result<(Vec<u8>, std::process::ExitStatus, u32)> {
    let (read_fd, write_fd) = nix::unistd::pipe()
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e.to_string()))?;

    let read_raw = read_fd.into_raw_fd();
    let write_raw = write_fd.into_raw_fd();
    let write_raw_dup = unsafe { libc::dup(write_raw) };
    if write_raw_dup < 0 {
        let err = std::io::Error::last_os_error();
        unsafe {
            libc::close(read_raw);
            libc::close(write_raw);
        }
        return Err(err);
    }

    cmd.stdout(unsafe { Stdio::from_raw_fd(write_raw) });
    cmd.stderr(unsafe { Stdio::from_raw_fd(write_raw_dup) });

    let child_result = cmd.spawn();
    // Drop cmd to close write-side fds held by its Stdio objects in the parent.
    // Without this, read_to_end below would block forever waiting for EOF.
    drop(cmd);
    let mut child = match child_result {
        Ok(c) => c,
        Err(e) => {
            unsafe { libc::close(read_raw) };
            return Err(e);
        }
    };
    let pid = child.id();
    let mut reader = unsafe { File::from_raw_fd(read_raw) };
    let mut data = Vec::new();
    reader.read_to_end(&mut data)?;
    let status = child.wait()?;

    Ok((data, status, pid))
}

#[cfg(all(feature = "std", not(unix)))]
fn run_combined_output(mut cmd: Command) -> std::io::Result<(Vec<u8>, std::process::ExitStatus, u32)> {
    cmd.stdout(Stdio::piped()).stderr(Stdio::piped()).spawn().and_then(|child| {
        let pid = child.id();
        let output = child.wait_with_output()?;
        let mut data = output.stdout;
        data.extend(output.stderr);
        Ok((data, output.status, pid))
    })
}

#[cfg(feature = "std")]
vo_runtime::stdlib_register!(os_exec:
    startProcess,
    waitProcess,
    runCaptureOutput,
);
