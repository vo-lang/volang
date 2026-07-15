//! os/exec package native function implementations.

#[cfg(feature = "std")]
use std::collections::HashMap;
#[cfg(feature = "std")]
use std::ffi::OsString;
#[cfg(all(feature = "std", unix))]
use std::fs::File;
#[cfg(all(feature = "std", unix))]
use std::os::fd::AsRawFd;
#[cfg(feature = "std")]
use std::process::{Command, Stdio};
#[cfg(feature = "std")]
use std::sync::{
    atomic::{AtomicU64, Ordering},
    Mutex,
};

#[cfg(feature = "std")]
use vo_ffi_macro::vostd_fn;
#[cfg(feature = "std")]
use vo_runtime::builtins::error_helper::{write_error_to, write_nil_error};
#[cfg(feature = "std")]
use vo_runtime::ffi::{ExternCallContext, ExternResult};
#[cfg(feature = "std")]
use vo_runtime::gc::{Gc, GcRef};
#[cfg(feature = "std")]
use vo_runtime::io::{CompletionData, IoResourceToken, IoRuntime};

#[cfg(feature = "std")]
struct ChildProcess {
    pid: u32,
    child: std::process::Child,
    cleanup_token: IoResourceToken,
}

#[cfg(feature = "std")]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct RegisteredChild {
    pid: u32,
    handle: u64,
}

#[cfg(feature = "std")]
static NEXT_CHILD_HANDLE: AtomicU64 = AtomicU64::new(1);

#[cfg(feature = "std")]
lazy_static::lazy_static! {
    static ref CHILD_PROCESSES: Mutex<HashMap<u64, ChildProcess>> = Mutex::new(HashMap::new());
}

#[cfg(feature = "std")]
fn checked_native_i32(raw: i64, label: &str) -> std::io::Result<i32> {
    i32::try_from(raw).map_err(|_| {
        std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            format!("{label} {raw} is outside the signed 32-bit native range"),
        )
    })
}

#[cfg(feature = "std")]
fn checked_process_handle(raw: i64) -> std::io::Result<u64> {
    u64::try_from(raw)
        .ok()
        .filter(|handle| *handle != 0)
        .ok_or_else(|| {
            std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                format!("process handle {raw} is outside the positive signed 64-bit range"),
            )
        })
}

#[cfg(feature = "std")]
fn try_allocate_child_handle() -> std::io::Result<u64> {
    NEXT_CHILD_HANDLE
        .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |current| {
            (current != 0 && current <= i64::MAX as u64).then(|| current + 1)
        })
        .map_err(|_| std::io::Error::other("process handle space exhausted"))
}

#[cfg(feature = "std")]
fn register_child(
    io: &mut IoRuntime,
    mut child: std::process::Child,
) -> std::io::Result<RegisteredChild> {
    let pid = child.id();
    if pid == 0 {
        let _ = child.kill();
        let _ = child.wait();
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            "operating system returned reserved child process id 0",
        ));
    }
    let handle = match try_allocate_child_handle() {
        Ok(handle) => handle,
        Err(error) => {
            terminate_and_reap(child);
            return Err(error);
        }
    };
    let cleanup_token = match io
        .register_resource_cleanup(move |token| move || cleanup_child_process(handle, token))
    {
        Ok(token) => token,
        Err(error) => {
            terminate_and_reap(child);
            return Err(error);
        }
    };
    let mut processes = CHILD_PROCESSES
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    if processes.contains_key(&handle) {
        drop(processes);
        let _ = io.disarm_resource_cleanup(cleanup_token);
        terminate_and_reap(child);
        return Err(std::io::Error::new(
            std::io::ErrorKind::AlreadyExists,
            format!("duplicate opaque process handle: {handle}"),
        ));
    }
    processes.insert(
        handle,
        ChildProcess {
            pid,
            child,
            cleanup_token,
        },
    );
    Ok(RegisteredChild { pid, handle })
}

#[cfg(feature = "std")]
fn cleanup_child_process(handle: u64, cleanup_token: IoResourceToken) {
    let process = {
        let mut processes = CHILD_PROCESSES
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if processes
            .get(&handle)
            .is_some_and(|process| process.cleanup_token == cleanup_token)
        {
            processes.remove(&handle)
        } else {
            None
        }
    };
    if let Some(process) = process {
        terminate_and_reap(process.child);
    }
}

#[cfg(feature = "std")]
fn terminate_and_reap(mut child: std::process::Child) {
    if !matches!(child.try_wait(), Ok(Some(_))) {
        let _ = child.kill();
        let _ = child.wait();
    }
}

#[cfg(feature = "std")]
fn write_start_error(call: &mut ExternCallContext, error: &str) {
    call.ret_i64(0, -1);
    call.ret_i64(1, 0);
    write_error_to(call, 2, error);
}

#[cfg(feature = "std")]
#[vostd_fn("os/exec", "startProcess", std)]
fn exec_start_process(call: &mut ExternCallContext) -> ExternResult {
    let stdio_handles = (|| {
        Ok::<_, std::io::Error>((
            checked_native_i32(call.arg_i64(slots::ARG_STDIN), "stdin handle")?,
            checked_native_i32(call.arg_i64(slots::ARG_STDOUT), "stdout handle")?,
            checked_native_i32(call.arg_i64(slots::ARG_STDERR), "stderr handle")?,
        ))
    })();
    let (stdin_fd, stdout_fd, stderr_fd) = match stdio_handles {
        Ok(handles) => handles,
        Err(error) => {
            write_start_error(call, &error.to_string());
            return ExternResult::Ok;
        }
    };

    let mut cmd = match command_from_call(
        call,
        slots::ARG_PATH,
        slots::ARG_ARGS,
        slots::ARG_DIR,
        slots::ARG_ENV,
    ) {
        Ok(cmd) => cmd,
        Err(error) => {
            write_start_error(call, &error.to_string());
            return ExternResult::Ok;
        }
    };
    if let Err(e) = configure_stdio(&mut cmd, stdin_fd, stdout_fd, stderr_fd) {
        write_start_error(call, &e.to_string());
        return ExternResult::Ok;
    }

    match cmd.spawn() {
        Ok(child) => match register_child(call.io_mut(), child) {
            Ok(process) => {
                call.ret_i64(0, i64::from(process.pid));
                call.ret_i64(1, process.handle as i64);
                write_nil_error(call, 2);
            }
            Err(error) => {
                write_start_error(call, &error.to_string());
            }
        },
        Err(e) => {
            write_start_error(call, &e.to_string());
        }
    }
    ExternResult::Ok
}

#[cfg(feature = "std")]
#[vostd_fn("os/exec", "isExecutable", std)]
fn exec_is_executable(call: &mut ExternCallContext) -> ExternResult {
    let path =
        match crate::host_bytes::path_buf_from_bytes(call.arg_string_bytes(0), "executable path") {
            Ok(path) => path,
            Err(error) => {
                call.ret_bool(0, false);
                write_error_to(call, 1, &error.to_string());
                return ExternResult::Ok;
            }
        };

    match is_executable_impl(&path) {
        Ok(()) => {
            call.ret_bool(0, true);
            write_nil_error(call, 1);
        }
        Err(error) => {
            call.ret_bool(0, false);
            write_error_to(call, 1, &error.to_string());
        }
    }
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn is_executable_impl(path: &std::path::Path) -> std::io::Result<()> {
    let metadata = std::fs::metadata(path)?;
    if metadata.is_dir() {
        return Err(std::io::Error::new(
            std::io::ErrorKind::PermissionDenied,
            format!("executable path is a directory: {path:?}"),
        ));
    }

    #[cfg(unix)]
    {
        use std::os::unix::ffi::OsStrExt;
        use std::os::unix::fs::PermissionsExt;

        let c_path = std::ffi::CString::new(path.as_os_str().as_bytes()).map_err(|_| {
            std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "executable path must not contain NUL",
            )
        })?;
        #[cfg(any(target_os = "linux", target_os = "android"))]
        let flags = libc::AT_EACCESS;
        #[cfg(not(any(target_os = "linux", target_os = "android")))]
        let flags = 0;
        if unsafe { libc::faccessat(libc::AT_FDCWD, c_path.as_ptr(), libc::X_OK, flags) } == 0 {
            return Ok(());
        }

        let error = std::io::Error::last_os_error();
        if matches!(error.raw_os_error(), Some(libc::ENOSYS) | Some(libc::EPERM)) {
            if metadata.permissions().mode() & 0o111 != 0 {
                return Ok(());
            }
            return Err(std::io::Error::new(
                std::io::ErrorKind::PermissionDenied,
                format!("executable permission is denied: {path:?}"),
            ));
        }
        Err(error)
    }

    #[cfg(not(unix))]
    Ok(())
}

#[cfg(feature = "std")]
fn configure_command(
    cmd: &mut Command,
    args: &[OsString],
    dir: Option<&std::path::Path>,
    env: Option<&[(OsString, OsString)]>,
) -> std::io::Result<()> {
    #[cfg(unix)]
    if let Some(arg0) = args.first() {
        use std::os::unix::process::CommandExt;
        cmd.arg0(arg0);
    }
    if args.len() > 1 {
        cmd.args(&args[1..]);
    }
    if let Some(dir) = dir {
        cmd.current_dir(dir);
    }
    if let Some(env_vars) = env {
        cmd.env_clear();
        for (key, value) in env_vars {
            cmd.env(key, value);
        }
    }
    Ok(())
}

#[cfg(feature = "std")]
fn command_from_call(
    call: &ExternCallContext,
    path_slot: u16,
    args_slot: u16,
    dir_slot: u16,
    env_slot: u16,
) -> std::io::Result<Command> {
    let program = crate::host_bytes::os_string_from_bytes(
        call.arg_string_bytes(path_slot),
        "executable path",
    )?;
    let args = crate::host_bytes::read_string_slice_bytes(call.arg_ref(args_slot))
        .into_iter()
        .map(|bytes| crate::host_bytes::os_string_from_bytes(bytes, "process argument"))
        .collect::<std::io::Result<Vec<_>>>()?;

    let dir_bytes = call.arg_string_bytes(dir_slot);
    let dir = if dir_bytes.is_empty() {
        None
    } else {
        Some(crate::host_bytes::path_buf_from_bytes(
            dir_bytes,
            "working directory",
        )?)
    };

    let env_ref = call.arg_ref(env_slot);
    let env = if env_ref.is_null() {
        None
    } else {
        Some(decode_environment(
            crate::host_bytes::read_string_slice_bytes(env_ref),
        )?)
    };

    let mut command = Command::new(program);
    configure_command(&mut command, &args, dir.as_deref(), env.as_deref())?;
    Ok(command)
}

#[cfg(feature = "std")]
fn decode_environment(entries: Vec<Vec<u8>>) -> std::io::Result<Vec<(OsString, OsString)>> {
    decode_environment_for_platform(entries, cfg!(windows))
}

#[cfg(feature = "std")]
fn decode_environment_for_platform(
    entries: Vec<Vec<u8>>,
    windows: bool,
) -> std::io::Result<Vec<(OsString, OsString)>> {
    let mut result = Vec::with_capacity(entries.len());
    for entry in entries {
        let Some(separator) = environment_separator(&entry, windows) else {
            // Go permits entries without '=' in the environment vector.
            // `std::process::Command` has no representation for them, so keep
            // the expressible entries and omit these opaque records.
            continue;
        };
        if separator == 0 {
            return Err(crate::host_bytes::invalid_input(
                "environment variable name must not be empty",
            ));
        }
        if entry.contains(&0) {
            return Err(crate::host_bytes::invalid_input(
                "environment entry must not contain NUL",
            ));
        }
        let key = crate::host_bytes::os_string_from_bytes(
            entry[..separator].to_vec(),
            "environment variable name",
        )?;
        let value = crate::host_bytes::os_string_from_bytes(
            entry[separator + 1..].to_vec(),
            "environment variable value",
        )?;
        result.push((key, value));
    }
    Ok(result)
}

#[cfg(feature = "std")]
fn environment_separator(entry: &[u8], windows: bool) -> Option<usize> {
    let search_start = usize::from(windows && entry.first() == Some(&b'='));
    entry[search_start..]
        .iter()
        .position(|byte| *byte == b'=')
        .map(|offset| search_start + offset)
}

#[cfg(feature = "std")]
fn configure_stdio(
    cmd: &mut Command,
    stdin_fd: i32,
    stdout_fd: i32,
    stderr_fd: i32,
) -> std::io::Result<()> {
    cmd.stdin(fd_to_stdio(stdin_fd)?);
    cmd.stdout(fd_to_stdio(stdout_fd)?);
    cmd.stderr(fd_to_stdio(stderr_fd)?);
    Ok(())
}

#[cfg(all(feature = "std", unix))]
fn fd_to_stdio(fd: i32) -> std::io::Result<Stdio> {
    if fd < 0 {
        return Ok(Stdio::null());
    }
    let file = crate::os::clone_file_from_handle(fd)?;
    file_to_stdio(file)
}

#[cfg(all(feature = "std", not(unix)))]
fn fd_to_stdio(fd: i32) -> std::io::Result<Stdio> {
    if fd < 0 {
        return Ok(Stdio::null());
    }
    crate::os::clone_file_from_handle(fd).map(Stdio::from)
}

#[cfg(all(feature = "std", unix))]
fn file_to_stdio(fd: File) -> std::io::Result<Stdio> {
    let flags = unsafe { libc::fcntl(fd.as_raw_fd(), libc::F_GETFL) };
    if flags < 0 {
        return Err(std::io::Error::last_os_error());
    }
    let ret = unsafe { libc::fcntl(fd.as_raw_fd(), libc::F_SETFL, flags & !libc::O_NONBLOCK) };
    if ret < 0 {
        return Err(std::io::Error::last_os_error());
    }

    Ok(Stdio::from(fd))
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
    pid: u32,
    status: &std::process::ExitStatus,
) -> GcRef {
    let state = call.gc_alloc_struct("os/exec.ProcessState");
    unsafe {
        Gc::write_slot(state, PS_PID, pid as u64);
        Gc::write_slot(state, PS_EXIT_CODE, status.code().unwrap_or(-1) as u64);
        Gc::write_slot(
            state,
            PS_EXITED,
            if status.code().is_some() { 1 } else { 0 },
        );
        #[cfg(unix)]
        {
            use std::os::unix::process::ExitStatusExt;
            Gc::write_slot(
                state,
                PS_SIGNALED,
                if status.signal().is_some() { 1 } else { 0 },
            );
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
fn owned_child_cleanup_token(io: &IoRuntime, handle: u64) -> std::io::Result<IoResourceToken> {
    let token = CHILD_PROCESSES
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .get(&handle)
        .map(|process| process.cleanup_token)
        .ok_or_else(|| std::io::Error::new(std::io::ErrorKind::NotFound, "process not found"))?;
    if !io.owns_resource_cleanup(token) {
        return Err(std::io::Error::new(
            std::io::ErrorKind::PermissionDenied,
            "process is owned by another runtime",
        ));
    }
    Ok(token)
}

#[cfg(feature = "std")]
fn kill_owned_child(io: &IoRuntime, handle: u64) -> std::io::Result<()> {
    let cleanup_token = owned_child_cleanup_token(io, handle)?;
    let mut processes = CHILD_PROCESSES
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let process = processes.get_mut(&handle).ok_or_else(|| {
        std::io::Error::new(
            std::io::ErrorKind::NotFound,
            "process ownership changed during kill",
        )
    })?;
    if process.cleanup_token != cleanup_token {
        return Err(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            "process ownership changed during kill",
        ));
    }
    process.child.kill()
}

#[cfg(feature = "std")]
#[vostd_fn("os/exec", "killProcess", std)]
fn exec_kill_process(call: &mut ExternCallContext) -> ExternResult {
    let handle = match checked_process_handle(call.arg_i64(slots::ARG_HANDLE)) {
        Ok(handle) => handle,
        Err(error) => {
            write_error_to(call, 0, &error.to_string());
            return ExternResult::Ok;
        }
    };
    match kill_owned_child(call.io_mut(), handle) {
        Ok(()) => write_nil_error(call, 0),
        Err(error) => write_error_to(call, 0, &error.to_string()),
    }
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn retire_owned_child(io: &mut IoRuntime, handle: u64) -> std::io::Result<ChildProcess> {
    let cleanup_token = owned_child_cleanup_token(io, handle)?;
    let process = {
        let mut processes = CHILD_PROCESSES
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if !processes
            .get(&handle)
            .is_some_and(|process| process.cleanup_token == cleanup_token)
        {
            return Err(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                "process ownership changed during wait",
            ));
        }
        processes
            .remove(&handle)
            .expect("owned child remained registered")
    };
    if !io.disarm_resource_cleanup(cleanup_token) {
        let pid = process.pid;
        terminate_and_reap(process.child);
        return Err(std::io::Error::other(format!(
            "process {pid} cleanup ownership disappeared during wait"
        )));
    }
    Ok(process)
}

#[cfg(feature = "std")]
fn write_wait_error(call: &mut ExternCallContext, error: &str) -> ExternResult {
    call.ret_ref(0, std::ptr::null_mut());
    write_error_to(call, 1, error);
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn abort_owned_wait(call: &mut ExternCallContext, handle: u64, error: String) -> ExternResult {
    if let Err(cleanup_error) = terminate_owned_child(call.io_mut(), handle) {
        write_wait_error(
            call,
            &format!("{error}; failed to retire child process: {cleanup_error}"),
        )
    } else {
        write_wait_error(call, &error)
    }
}

#[cfg(feature = "std")]
fn terminate_owned_child(io: &mut IoRuntime, handle: u64) -> std::io::Result<()> {
    let process = retire_owned_child(io, handle)?;
    terminate_and_reap(process.child);
    Ok(())
}

#[cfg(feature = "std")]
#[vostd_fn("os/exec", "waitProcess", std, effects(MAY_WAIT_IO_REPLAY))]
fn exec_wait_process(call: &mut ExternCallContext) -> ExternResult {
    let handle = match checked_process_handle(call.arg_i64(slots::ARG_HANDLE)) {
        Ok(handle) => handle,
        Err(error) => {
            return write_wait_error(call, &error.to_string());
        }
    };
    if let Err(error) = owned_child_cleanup_token(call.io_mut(), handle) {
        return write_wait_error(call, &error.to_string());
    }

    if let Some(token) = call.take_resume_io_token() {
        match call.io_mut().take_completion(token).result {
            Ok(CompletionData::Timer) => {}
            Ok(_) => {
                return abort_owned_wait(
                    call,
                    handle,
                    "process wait resumed with unexpected I/O data".to_string(),
                );
            }
            Err(error) => {
                return abort_owned_wait(
                    call,
                    handle,
                    format!("process wait timer failed: {error}"),
                );
            }
        }
    }

    enum WaitState {
        Exited(std::process::ExitStatus),
        Pending,
        Failed(std::io::Error),
        Missing,
    }

    let wait_state = {
        let mut processes = CHILD_PROCESSES
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let probe = processes
            .get_mut(&handle)
            .map(|process| process.child.try_wait());
        match probe {
            None => WaitState::Missing,
            Some(Ok(Some(status))) => WaitState::Exited(status),
            Some(Ok(None)) => WaitState::Pending,
            Some(Err(error)) => WaitState::Failed(error),
        }
    };

    match wait_state {
        WaitState::Exited(status) => {
            let process = match retire_owned_child(call.io_mut(), handle) {
                Ok(process) => process,
                Err(error) => return write_wait_error(call, &error.to_string()),
            };
            let state = write_process_state(call, process.pid, &status);
            call.ret_ref(0, state);
            write_nil_error(call, 1);
            ExternResult::Ok
        }
        WaitState::Pending => {
            const WAIT_POLL_NS: i64 = 1_000_000;
            let token = match call.io_mut().try_submit_timer(WAIT_POLL_NS) {
                Ok(token) => token,
                Err(error) => {
                    return abort_owned_wait(
                        call,
                        handle,
                        format!("process wait timer submission failed: {error}"),
                    );
                }
            };
            ExternResult::WaitIo { token }
        }
        WaitState::Failed(error) => abort_owned_wait(call, handle, error.to_string()),
        WaitState::Missing => write_wait_error(call, "process not found"),
    }
}

#[cfg(feature = "std")]
vo_ffi_macro::vostd_register!("os/exec":
    startProcess,
    isExecutable,
    killProcess,
    waitProcess,
);

#[cfg(all(test, feature = "std"))]
mod tests {
    use super::{
        checked_native_i32, checked_process_handle, configure_command, decode_environment,
        is_executable_impl,
    };

    #[cfg(unix)]
    use super::{
        kill_owned_child, owned_child_cleanup_token, register_child, terminate_owned_child,
        CHILD_PROCESSES,
    };
    #[cfg(unix)]
    use vo_runtime::io::IoRuntime;

    #[cfg(unix)]
    #[test]
    fn child_process_is_terminated_reaped_and_unregistered_on_vm_drop() {
        let child = std::process::Command::new("/bin/sleep")
            .arg("30")
            .spawn()
            .expect("spawn long-running child");
        let mut owner = IoRuntime::new().expect("owner I/O runtime");
        let registered = register_child(&mut owner, child).expect("register child process");
        assert!(CHILD_PROCESSES
            .lock()
            .unwrap()
            .contains_key(&registered.handle));

        drop(owner);
        assert!(!CHILD_PROCESSES
            .lock()
            .unwrap()
            .contains_key(&registered.handle));
        assert_eq!(
            unsafe { libc::kill(i32::try_from(registered.pid).unwrap(), 0) },
            -1
        );
        assert_eq!(
            std::io::Error::last_os_error().raw_os_error(),
            Some(libc::ESRCH),
            "VM-owned child must be gone and reaped"
        );
    }

    #[cfg(unix)]
    #[test]
    fn child_process_is_terminated_before_guest_exit_returns() {
        let child = std::process::Command::new("/bin/sleep")
            .arg("30")
            .spawn()
            .expect("spawn long-running child");
        let mut owner = IoRuntime::new().expect("owner I/O runtime");
        let registered = register_child(&mut owner, child).expect("register child process");

        owner.shutdown();

        assert!(!CHILD_PROCESSES
            .lock()
            .unwrap()
            .contains_key(&registered.handle));
        assert_eq!(
            unsafe { libc::kill(i32::try_from(registered.pid).unwrap(), 0) },
            -1
        );
        assert_eq!(
            std::io::Error::last_os_error().raw_os_error(),
            Some(libc::ESRCH),
            "guest exit must terminate and reap its child synchronously"
        );
        assert!(!owner.has_pending());
    }

    #[cfg(unix)]
    #[test]
    fn process_handle_is_private_to_the_runtime_that_started_it() {
        let child = std::process::Command::new("/bin/sleep")
            .arg("30")
            .spawn()
            .expect("spawn long-running child");
        let mut owner = IoRuntime::new().expect("owner I/O runtime");
        let registered = register_child(&mut owner, child).expect("register child process");
        let cleanup_token = owned_child_cleanup_token(&owner, registered.handle)
            .expect("owner should resolve its child");
        let mut other = IoRuntime::new().expect("second I/O runtime");

        let error = owned_child_cleanup_token(&other, registered.handle)
            .expect_err("second runtime must not resolve the child");
        assert_eq!(error.kind(), std::io::ErrorKind::PermissionDenied);
        let error = terminate_owned_child(&mut other, registered.handle)
            .expect_err("second runtime must not retire the child");
        assert_eq!(error.kind(), std::io::ErrorKind::PermissionDenied);
        let error = kill_owned_child(&other, registered.handle)
            .expect_err("second runtime must not signal the child");
        assert_eq!(error.kind(), std::io::ErrorKind::PermissionDenied);
        assert!(owner.owns_resource_cleanup(cleanup_token));
        assert!(CHILD_PROCESSES
            .lock()
            .unwrap()
            .contains_key(&registered.handle));

        kill_owned_child(&owner, registered.handle).expect("owner should signal its child");
        terminate_owned_child(&mut owner, registered.handle)
            .expect("owner should terminate its child");
        assert!(!owner.owns_resource_cleanup(cleanup_token));
        assert!(!CHILD_PROCESSES
            .lock()
            .unwrap()
            .contains_key(&registered.handle));
    }

    #[cfg(unix)]
    #[test]
    fn failed_wait_cleanup_terminates_reaps_and_disarms_the_child() {
        let child = std::process::Command::new("/bin/sleep")
            .arg("30")
            .spawn()
            .expect("spawn long-running child");
        let mut owner = IoRuntime::new().expect("owner I/O runtime");
        let registered = register_child(&mut owner, child).expect("register child process");
        let cleanup_token = owned_child_cleanup_token(&owner, registered.handle)
            .expect("owner should resolve its child");

        terminate_owned_child(&mut owner, registered.handle)
            .expect("failed wait should retire the child");

        assert!(!owner.owns_resource_cleanup(cleanup_token));
        assert!(!CHILD_PROCESSES
            .lock()
            .unwrap()
            .contains_key(&registered.handle));
        assert_eq!(
            unsafe { libc::kill(i32::try_from(registered.pid).unwrap(), 0) },
            -1
        );
        assert_eq!(
            std::io::Error::last_os_error().raw_os_error(),
            Some(libc::ESRCH),
            "failed wait cleanup must reap the child"
        );
    }

    #[cfg(unix)]
    #[test]
    fn retired_process_handles_never_alias_later_children() {
        let mut owner = IoRuntime::new().expect("owner I/O runtime");
        let first_child = std::process::Command::new("/bin/sleep")
            .arg("30")
            .spawn()
            .expect("spawn first child");
        let first = register_child(&mut owner, first_child).expect("register first child");
        terminate_owned_child(&mut owner, first.handle).expect("retire first child");

        let second_child = std::process::Command::new("/bin/sleep")
            .arg("30")
            .spawn()
            .expect("spawn second child");
        let second = register_child(&mut owner, second_child).expect("register second child");
        assert_ne!(first.handle, second.handle);
        assert_eq!(
            owned_child_cleanup_token(&owner, first.handle)
                .expect_err("retired handle must remain stale")
                .kind(),
            std::io::ErrorKind::NotFound
        );
        assert!(owned_child_cleanup_token(&owner, second.handle).is_ok());
        terminate_owned_child(&mut owner, second.handle).expect("retire second child");
    }

    #[test]
    fn process_and_stdio_handles_reject_native_width_aliases() {
        assert_eq!(
            checked_native_i32(i32::MIN as i64, "handle").unwrap(),
            i32::MIN
        );
        assert_eq!(
            checked_native_i32(i32::MAX as i64, "handle").unwrap(),
            i32::MAX
        );
        for raw in [i32::MIN as i64 - 1, i32::MAX as i64 + 1, i64::MIN, i64::MAX] {
            let error = checked_native_i32(raw, "handle").unwrap_err();
            assert_eq!(error.kind(), std::io::ErrorKind::InvalidInput);
            assert!(error.to_string().contains(&raw.to_string()));
        }
        assert_eq!(checked_process_handle(1).unwrap(), 1);
        assert_eq!(checked_process_handle(i64::MAX).unwrap(), i64::MAX as u64);
        for raw in [i64::MIN, -1, 0] {
            let error = checked_process_handle(raw).unwrap_err();
            assert_eq!(error.kind(), std::io::ErrorKind::InvalidInput);
            assert!(error.to_string().contains(&raw.to_string()));
        }
    }

    #[test]
    fn extern_inputs_never_narrow_i64_with_wrapping_casts() {
        let source = include_str!("exec.rs");
        let production = source
            .split("#[cfg(all(test, feature = \"std\"))]")
            .next()
            .expect("production exec implementation");
        assert!(!production.contains("arg_i64(slots::ARG_STDIN) as i32"));
        assert!(!production.contains("arg_i64(slots::ARG_STDOUT) as i32"));
        assert!(!production.contains("arg_i64(slots::ARG_STDERR) as i32"));
        assert!(!production.contains("arg_i64(slots::ARG_HANDLE) as u64"));
        assert!(!production.contains("arg_i64(slots::ARG_PID)"));
    }

    #[cfg(unix)]
    #[test]
    fn environment_decoder_preserves_non_utf8_unix_bytes() {
        use std::os::unix::ffi::OsStrExt;

        let decoded = decode_environment(vec![vec![b'K', b'=', 0xff, b'V']]).unwrap();
        assert_eq!(decoded.len(), 1);
        assert_eq!(decoded[0].0.as_bytes(), b"K");
        assert_eq!(decoded[0].1.as_bytes(), &[0xff, b'V']);
    }

    #[test]
    fn environment_decoder_skips_opaque_entries_and_rejects_invalid_pairs() {
        let decoded =
            decode_environment(vec![b"MISSING_SEPARATOR".to_vec(), b"KEY=value".to_vec()]).unwrap();
        assert_eq!(
            decoded,
            vec![(
                std::ffi::OsString::from("KEY"),
                std::ffi::OsString::from("value")
            )]
        );

        for entry in [
            b"=empty-key".to_vec(),
            b"KEY=bad\0value".to_vec(),
            b"bad\0key=value".to_vec(),
        ] {
            let error = decode_environment(vec![entry]).unwrap_err();
            assert_eq!(error.kind(), std::io::ErrorKind::InvalidInput);
        }
    }

    #[test]
    fn windows_environment_decoder_preserves_hidden_drive_entries() {
        let decoded = super::decode_environment_for_platform(
            vec![br"=C:=C:\work".to_vec(), b"KEY=value".to_vec()],
            true,
        )
        .unwrap();
        assert_eq!(decoded[0].0, std::ffi::OsStr::new("=C:"));
        assert_eq!(decoded[0].1, std::ffi::OsStr::new(r"C:\work"));
        assert_eq!(decoded[1].0, std::ffi::OsStr::new("KEY"));
        assert!(super::decode_environment_for_platform(vec![b"=bad".to_vec()], false).is_err());
    }

    #[test]
    fn non_unix_command_setup_does_not_reject_resolved_program_argv_zero() {
        let production = include_str!("exec.rs")
            .split("#[cfg(all(test, feature = \"std\"))]")
            .next()
            .unwrap();
        assert!(!production.contains("custom process argv[0] is unavailable"));
        assert!(!production.contains("arg0 != &program"));
    }

    #[cfg(unix)]
    #[test]
    fn executable_probe_uses_host_access_rules_and_rejects_directories() {
        use std::os::unix::fs::PermissionsExt;

        let root = std::env::temp_dir().join(format!(
            "vo-executable-probe-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        std::fs::create_dir(&root).unwrap();
        let file = root.join("program");
        std::fs::write(&file, b"#!/bin/sh\n").unwrap();
        std::fs::set_permissions(&file, std::fs::Permissions::from_mode(0o644)).unwrap();
        assert!(is_executable_impl(&file).is_err());

        std::fs::set_permissions(&file, std::fs::Permissions::from_mode(0o755)).unwrap();
        is_executable_impl(&file).unwrap();
        assert_eq!(
            is_executable_impl(&root).unwrap_err().kind(),
            std::io::ErrorKind::PermissionDenied
        );
        std::fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn configure_command_uses_last_duplicate_environment_entry_and_accepts_empty_args() {
        let environment = decode_environment(vec![
            b"K=old".to_vec(),
            b"EMPTY=".to_vec(),
            b"K=new".to_vec(),
        ])
        .unwrap();
        let mut command = std::process::Command::new("ignored");
        configure_command(&mut command, &[], None, Some(&environment)).unwrap();
        assert_eq!(command.get_args().count(), 0);
        let configured = command
            .get_envs()
            .find(|(key, _)| *key == std::ffi::OsStr::new("K"))
            .and_then(|(_, value)| value)
            .expect("configured K");
        assert_eq!(configured, std::ffi::OsStr::new("new"));
        let empty = command
            .get_envs()
            .find(|(key, _)| *key == std::ffi::OsStr::new("EMPTY"))
            .and_then(|(_, value)| value)
            .expect("configured empty value");
        assert_eq!(empty, std::ffi::OsStr::new(""));
    }

    #[cfg(unix)]
    #[test]
    fn configure_command_preserves_raw_argv_zero() {
        use std::os::unix::ffi::OsStringExt;

        let raw_arg0 = std::ffi::OsString::from_vec(b"vo-\xff".to_vec());
        let args = vec![
            raw_arg0,
            std::ffi::OsString::from("-c"),
            std::ffi::OsString::from("printf %s \"$0\""),
        ];
        let mut command = std::process::Command::new("/bin/sh");
        configure_command(&mut command, &args, None, None).unwrap();
        let output = command.output().unwrap();
        assert!(output.status.success());
        assert_eq!(output.stdout, b"vo-\xff");
    }
}
