//! os package WASM implementation backed by JS VirtualFS.

use vo_runtime::builtins::error_helper::{create_error, write_error_to, write_nil_error};
use vo_runtime::bytecode::ExternDef;
use vo_runtime::core_types::{ValueKind, ValueMeta};
use vo_runtime::ffi::{ExternCallContext, ExternRegistry, ExternResult};
use vo_runtime::objects::{array, slice, string};
use vo_runtime::slot::SLOT_BYTES;

use crate::vfs;

const ERR_NOT_SUPPORTED: &str = "operation not supported on wasm";
const OS_ERROR_MESSAGES: [&str; 8] = [
    "file does not exist",
    "file already exists",
    "permission denied",
    "invalid argument",
    "i/o timeout",
    "file already closed",
    "not a directory",
    "is a directory",
];

// Thread-local args injected by run_with_args() before running the VM.
thread_local! {
    pub static WASM_PROG_ARGS: std::cell::RefCell<Option<Vec<String>>> =
        const { std::cell::RefCell::new(None) };
}

fn write_not_supported_error(call: &mut ExternCallContext, slot: u16) {
    write_error_to(call, slot, ERR_NOT_SUPPORTED);
}

const MODE_DIR: u32 = 1 << 31;

// =============================================================================
// OS Errors & Constants
// =============================================================================

fn os_get_errors(call: &mut ExternCallContext) -> ExternResult {
    init_os_errors(call);
    let Some(errors) = call
        .sentinel_errors()
        .get("os")
        .map(|errors| errors.to_vec())
    else {
        return ExternResult::Panic("os sentinel error initialization failed".to_string());
    };
    for (i, pair) in errors.into_iter().enumerate() {
        call.ret_interface_pair((i * 2) as u16, pair);
    }
    ExternResult::Ok
}

fn init_os_errors(call: &mut ExternCallContext) {
    if call.sentinel_errors().contains("os") {
        return;
    }
    let errors = OS_ERROR_MESSAGES
        .iter()
        .map(|msg| create_error(call, msg))
        .collect();
    call.sentinel_errors_mut().insert("os", errors);
}

fn os_get_consts(call: &mut ExternCallContext) -> ExternResult {
    call.ret_i64(0, 0); // O_RDONLY
    call.ret_i64(1, 1); // O_WRONLY
    call.ret_i64(2, 2); // O_RDWR
    call.ret_i64(3, 8); // O_APPEND
    call.ret_i64(4, 16); // O_CREATE
    call.ret_i64(5, 32); // O_EXCL
    call.ret_i64(6, 64); // O_SYNC
    call.ret_i64(7, 128); // O_TRUNC
    ExternResult::Ok
}

// =============================================================================
// File Operations
// =============================================================================

fn open_file(call: &mut ExternCallContext) -> ExternResult {
    let name = call.arg_str(0);
    let flag = call.arg_i64(1) as i32;
    let perm = call.arg_u64(2) as u32;

    let (fd, err) = vfs::open_file(name, flag, perm);

    if let Some(msg) = err {
        call.ret_i64(0, -1);
        write_error_to(call, 1, &msg);
    } else {
        call.ret_i64(0, fd as i64);
        write_nil_error(call, 1);
    }
    ExternResult::Ok
}

fn file_read(call: &mut ExternCallContext) -> ExternResult {
    let fd = call.arg_i64(0) as i32;
    let buf_ref = call.arg_ref(1);
    // Safety: the resolved os.File.Read ABI supplies a rooted []byte argument.
    let buf_len = unsafe { slice::len(buf_ref) };

    let (data, err) = vfs::read(fd, buf_len as u32);

    if let Some(msg) = err {
        call.ret_i64(0, 0);
        write_error_to(call, 1, &msg);
    } else {
        let n = data.len().min(buf_len);
        // Safety: the verified []byte remains rooted for this extern call and
        // `n` is bounded by its visible length.
        let buf_ptr = unsafe { slice::data_ptr(buf_ref) };
        unsafe {
            std::ptr::copy_nonoverlapping(data.as_ptr(), buf_ptr, n);
        }
        call.ret_i64(0, n as i64);
        if n == 0 && buf_len > 0 {
            write_error_to(call, 1, "EOF");
        } else {
            write_nil_error(call, 1);
        }
    }
    ExternResult::Ok
}

fn file_write(call: &mut ExternCallContext) -> ExternResult {
    let fd = call.arg_i64(0) as i32;
    let buf_ref = call.arg_ref(1);
    // Safety: the resolved os.File.Write ABI supplies a rooted []byte argument.
    let buf_len = unsafe { slice::len(buf_ref) };
    // Safety: the slice remains rooted and readable for this synchronous call.
    let buf_ptr = unsafe { slice::data_ptr(buf_ref) };
    let data = unsafe { std::slice::from_raw_parts(buf_ptr, buf_len) };

    let (n, err) = vfs::write(fd, data);

    if let Some(msg) = err {
        call.ret_i64(0, 0);
        write_error_to(call, 1, &msg);
    } else {
        call.ret_i64(0, n as i64);
        write_nil_error(call, 1);
    }
    ExternResult::Ok
}

fn file_read_at(call: &mut ExternCallContext) -> ExternResult {
    let fd = call.arg_i64(0) as i32;
    let buf_ref = call.arg_ref(1);
    let offset = call.arg_i64(2);
    // Safety: the resolved os.File.ReadAt ABI supplies a rooted []byte argument.
    let buf_len = unsafe { slice::len(buf_ref) };

    let (data, err) = vfs::read_at(fd, buf_len as u32, offset);

    if let Some(msg) = err {
        call.ret_i64(0, 0);
        write_error_to(call, 1, &msg);
    } else {
        let n = data.len().min(buf_len);
        // Safety: the verified []byte remains rooted for this extern call and
        // `n` is bounded by its visible length.
        let buf_ptr = unsafe { slice::data_ptr(buf_ref) };
        unsafe {
            std::ptr::copy_nonoverlapping(data.as_ptr(), buf_ptr, n);
        }
        call.ret_i64(0, n as i64);
        if n == 0 && buf_len > 0 {
            write_error_to(call, 1, "EOF");
        } else {
            write_nil_error(call, 1);
        }
    }
    ExternResult::Ok
}

fn file_write_at(call: &mut ExternCallContext) -> ExternResult {
    let fd = call.arg_i64(0) as i32;
    let buf_ref = call.arg_ref(1);
    let offset = call.arg_i64(2);
    // Safety: the resolved os.File.WriteAt ABI supplies a rooted []byte argument.
    let buf_len = unsafe { slice::len(buf_ref) };
    // Safety: the slice remains rooted and readable for this synchronous call.
    let buf_ptr = unsafe { slice::data_ptr(buf_ref) };
    let data = unsafe { std::slice::from_raw_parts(buf_ptr, buf_len) };

    let (n, err) = vfs::write_at(fd, data, offset);

    if let Some(msg) = err {
        call.ret_i64(0, 0);
        write_error_to(call, 1, &msg);
    } else {
        call.ret_i64(0, n as i64);
        write_nil_error(call, 1);
    }
    ExternResult::Ok
}

fn file_seek(call: &mut ExternCallContext) -> ExternResult {
    let fd = call.arg_i64(0) as i32;
    let offset = call.arg_i64(1);
    let whence = call.arg_i64(2) as i32;

    let (pos, err) = vfs::seek(fd, offset, whence);

    if let Some(msg) = err {
        call.ret_i64(0, 0);
        write_error_to(call, 1, &msg);
    } else {
        call.ret_i64(0, pos);
        write_nil_error(call, 1);
    }
    ExternResult::Ok
}

fn file_close(call: &mut ExternCallContext) -> ExternResult {
    let fd = call.arg_i64(0) as i32;

    // Allow closing stdin/stdout/stderr without error
    if fd <= 2 {
        write_nil_error(call, 0);
        return ExternResult::Ok;
    }

    if let Some(msg) = vfs::close(fd) {
        write_error_to(call, 0, &msg);
    } else {
        write_nil_error(call, 0);
    }
    ExternResult::Ok
}

fn file_sync(call: &mut ExternCallContext) -> ExternResult {
    let fd = call.arg_i64(0) as i32;

    if let Some(msg) = vfs::sync(fd) {
        write_error_to(call, 0, &msg);
    } else {
        write_nil_error(call, 0);
    }
    ExternResult::Ok
}

fn file_stat(call: &mut ExternCallContext) -> ExternResult {
    let fd = call.arg_i64(0) as i32;

    let (size, mode, mod_time, is_dir, err) = vfs::fstat(fd);

    if let Some(msg) = err {
        for i in 0..5 {
            call.ret_u64(i, 0);
        }
        write_error_to(call, 5, &msg);
    } else {
        let name_ref = string::from_rust_str(call.gc(), "");
        let mut mode_val = mode;
        if is_dir {
            mode_val |= MODE_DIR;
        }

        call.ret_ref(0, name_ref);
        call.ret_i64(1, size);
        call.ret_u64(2, mode_val as u64);
        call.ret_i64(3, mod_time / 1000); // JS ms -> seconds
        call.ret_u64(4, if is_dir { 1 } else { 0 });
        write_nil_error(call, 5);
    }
    ExternResult::Ok
}

fn file_truncate(call: &mut ExternCallContext) -> ExternResult {
    let fd = call.arg_i64(0) as i32;
    let size = call.arg_i64(1);

    if let Some(msg) = vfs::ftruncate(fd, size) {
        write_error_to(call, 0, &msg);
    } else {
        write_nil_error(call, 0);
    }
    ExternResult::Ok
}

// =============================================================================
// Directory Operations
// =============================================================================

fn native_mkdir(call: &mut ExternCallContext) -> ExternResult {
    let path = call.arg_str(0);
    let perm = call.arg_u64(1) as u32;

    if let Some(msg) = vfs::mkdir(path, perm) {
        write_error_to(call, 0, &msg);
    } else {
        write_nil_error(call, 0);
    }
    ExternResult::Ok
}

fn native_mkdir_all(call: &mut ExternCallContext) -> ExternResult {
    let path = call.arg_str(0);
    let perm = call.arg_u64(1) as u32;

    if let Some(msg) = vfs::mkdir_all(path, perm) {
        write_error_to(call, 0, &msg);
    } else {
        write_nil_error(call, 0);
    }
    ExternResult::Ok
}

fn native_remove(call: &mut ExternCallContext) -> ExternResult {
    let name = call.arg_str(0);

    if let Some(msg) = vfs::remove(name) {
        write_error_to(call, 0, &msg);
    } else {
        write_nil_error(call, 0);
    }
    ExternResult::Ok
}

fn native_remove_all(call: &mut ExternCallContext) -> ExternResult {
    let path = call.arg_str(0);

    if let Some(msg) = vfs::remove_all(path) {
        write_error_to(call, 0, &msg);
    } else {
        write_nil_error(call, 0);
    }
    ExternResult::Ok
}

fn native_rename(call: &mut ExternCallContext) -> ExternResult {
    let oldpath = call.arg_str(0);
    let newpath = call.arg_str(1);

    if let Some(msg) = vfs::rename(oldpath, newpath) {
        write_error_to(call, 0, &msg);
    } else {
        write_nil_error(call, 0);
    }
    ExternResult::Ok
}

fn native_stat(call: &mut ExternCallContext) -> ExternResult {
    let name = call.arg_str(0);

    let (basename, size, mode, mod_time, is_dir, err) = vfs::stat(name);

    if let Some(msg) = err {
        for i in 0..5 {
            call.ret_u64(i, 0);
        }
        write_error_to(call, 5, &msg);
    } else {
        let name_ref = string::from_rust_str(call.gc(), &basename);
        let mut mode_val = mode;
        if is_dir {
            mode_val |= MODE_DIR;
        }

        call.ret_ref(0, name_ref);
        call.ret_i64(1, size);
        call.ret_u64(2, mode_val as u64);
        call.ret_i64(3, mod_time / 1000); // JS ms -> seconds
        call.ret_u64(4, if is_dir { 1 } else { 0 });
        write_nil_error(call, 5);
    }
    ExternResult::Ok
}

fn native_lstat(call: &mut ExternCallContext) -> ExternResult {
    // No symlinks in VFS, same as stat
    native_stat(call)
}

fn native_read_dir(call: &mut ExternCallContext) -> ExternResult {
    let name = call.arg_str(0);

    let (entries, err) = vfs::read_dir(name);

    if let Some(msg) = err {
        call.ret_nil(0);
        write_error_to(call, 1, &msg);
    } else {
        let gc = call.gc();
        let len = entries.len();
        let elem_slots = 3;
        let elem_meta = vo_runtime::ValueMeta::new(0, ValueKind::Struct);
        let result = slice::create(gc, elem_meta, elem_slots * SLOT_BYTES, len, len);

        for (i, (entry_name, is_dir, mode)) in entries.iter().enumerate() {
            let name_ref = string::from_rust_str(gc, entry_name);
            let mut mode_val = *mode;
            if *is_dir {
                mode_val |= MODE_DIR;
            }
            let base = i * elem_slots;
            unsafe {
                slice::set(result, base, name_ref as u64, SLOT_BYTES);
                slice::set(result, base + 1, if *is_dir { 1 } else { 0 }, SLOT_BYTES);
                slice::set(result, base + 2, mode_val as u64, SLOT_BYTES);
            }
        }
        call.ret_ref(0, result);
        write_nil_error(call, 1);
    }
    ExternResult::Ok
}

fn native_chmod(call: &mut ExternCallContext) -> ExternResult {
    let name = call.arg_str(0);
    let mode = call.arg_u64(1) as u32;

    if let Some(msg) = vfs::chmod(name, mode) {
        write_error_to(call, 0, &msg);
    } else {
        write_nil_error(call, 0);
    }
    ExternResult::Ok
}

fn native_truncate(call: &mut ExternCallContext) -> ExternResult {
    let name = call.arg_str(0);
    let size = call.arg_i64(1);

    if let Some(msg) = vfs::truncate(name, size) {
        write_error_to(call, 0, &msg);
    } else {
        write_nil_error(call, 0);
    }
    ExternResult::Ok
}

fn native_read_file(call: &mut ExternCallContext) -> ExternResult {
    let name = call.arg_str(0);

    let (data, err) = vfs::read_file(name);

    if let Some(msg) = err {
        let gc = call.gc();
        let elem_meta = ValueMeta::new(0, ValueKind::Uint8);
        let arr = array::create(gc, elem_meta, 1, 0);
        // Safety: `arr` was allocated above as a live byte array in this GC.
        let slice_ref = unsafe { slice::from_array(gc, arr) };
        call.ret_ref(0, slice_ref);
        write_error_to(call, 1, &msg);
    } else {
        call.ret_bytes(0, &data);
        write_nil_error(call, 1);
    }
    ExternResult::Ok
}

fn native_write_file(call: &mut ExternCallContext) -> ExternResult {
    let name = call.arg_str(0);
    let data = call.arg_bytes(1);
    let perm = call.arg_u64(2) as u32;

    if let Some(msg) = vfs::write_file(name, data, perm) {
        write_error_to(call, 0, &msg);
    } else {
        write_nil_error(call, 0);
    }
    ExternResult::Ok
}

// =============================================================================
// Not Supported Operations
// =============================================================================

fn native_chown(call: &mut ExternCallContext) -> ExternResult {
    // chown not meaningful in browser
    write_nil_error(call, 0);
    ExternResult::Ok
}

fn native_symlink(call: &mut ExternCallContext) -> ExternResult {
    write_not_supported_error(call, 0);
    ExternResult::Ok
}

fn native_readlink(call: &mut ExternCallContext) -> ExternResult {
    let str_ref = string::from_rust_str(call.gc(), "");
    call.ret_ref(0, str_ref);
    write_not_supported_error(call, 1);
    ExternResult::Ok
}

fn native_link(call: &mut ExternCallContext) -> ExternResult {
    write_not_supported_error(call, 0);
    ExternResult::Ok
}

// =============================================================================
// Environment - return empty/defaults (no real env in browser)
// =============================================================================

fn native_getenv(call: &mut ExternCallContext) -> ExternResult {
    let str_ref = string::from_rust_str(call.gc(), "");
    call.ret_ref(0, str_ref);
    ExternResult::Ok
}

fn native_setenv(call: &mut ExternCallContext) -> ExternResult {
    write_nil_error(call, 0);
    ExternResult::Ok
}

fn native_unsetenv(call: &mut ExternCallContext) -> ExternResult {
    write_nil_error(call, 0);
    ExternResult::Ok
}

fn native_environ(call: &mut ExternCallContext) -> ExternResult {
    let gc = call.gc();
    let elem_meta = ValueMeta::new(0, ValueKind::String);
    let arr = array::create(gc, elem_meta, 8, 0);
    // Safety: `arr` was allocated above as a live string-reference array.
    let slice_ref = unsafe { slice::from_array(gc, arr) };
    call.ret_ref(0, slice_ref);
    ExternResult::Ok
}

fn native_lookup_env(call: &mut ExternCallContext) -> ExternResult {
    let str_ref = string::from_rust_str(call.gc(), "");
    call.ret_ref(0, str_ref);
    call.ret_bool(1, false);
    ExternResult::Ok
}

fn native_clearenv(_call: &mut ExternCallContext) -> ExternResult {
    ExternResult::Ok
}

fn native_expand_env(call: &mut ExternCallContext) -> ExternResult {
    let s = call.arg_str(0).to_string();
    let str_ref = string::from_rust_str(call.gc(), &s);
    call.ret_ref(0, str_ref);
    ExternResult::Ok
}

// =============================================================================
// Working Directory
// =============================================================================

fn native_getwd(call: &mut ExternCallContext) -> ExternResult {
    let str_ref = string::from_rust_str(call.gc(), "/");
    call.ret_ref(0, str_ref);
    write_nil_error(call, 1);
    ExternResult::Ok
}

fn native_chdir(call: &mut ExternCallContext) -> ExternResult {
    // TODO: could implement cwd tracking in VFS
    write_nil_error(call, 0);
    ExternResult::Ok
}

fn native_user_home_dir(call: &mut ExternCallContext) -> ExternResult {
    let str_ref = string::from_rust_str(call.gc(), "/home");
    call.ret_ref(0, str_ref);
    write_nil_error(call, 1);
    ExternResult::Ok
}

fn native_user_cache_dir(call: &mut ExternCallContext) -> ExternResult {
    let str_ref = string::from_rust_str(call.gc(), "/tmp/cache");
    call.ret_ref(0, str_ref);
    write_nil_error(call, 1);
    ExternResult::Ok
}

fn native_user_config_dir(call: &mut ExternCallContext) -> ExternResult {
    let str_ref = string::from_rust_str(call.gc(), "/home/config");
    call.ret_ref(0, str_ref);
    write_nil_error(call, 1);
    ExternResult::Ok
}

fn native_temp_dir(call: &mut ExternCallContext) -> ExternResult {
    let str_ref = string::from_rust_str(call.gc(), "/tmp");
    call.ret_ref(0, str_ref);
    ExternResult::Ok
}

// =============================================================================
// Process Info - return defaults
// =============================================================================

fn native_getpid(call: &mut ExternCallContext) -> ExternResult {
    call.ret_i64(0, 1);
    ExternResult::Ok
}

fn native_getppid(call: &mut ExternCallContext) -> ExternResult {
    call.ret_i64(0, 0);
    ExternResult::Ok
}

fn native_getuid(call: &mut ExternCallContext) -> ExternResult {
    call.ret_i64(0, 1000);
    ExternResult::Ok
}

fn native_geteuid(call: &mut ExternCallContext) -> ExternResult {
    call.ret_i64(0, 1000);
    ExternResult::Ok
}

fn native_getgid(call: &mut ExternCallContext) -> ExternResult {
    call.ret_i64(0, 1000);
    ExternResult::Ok
}

fn native_getegid(call: &mut ExternCallContext) -> ExternResult {
    call.ret_i64(0, 1000);
    ExternResult::Ok
}

fn native_exit(_call: &mut ExternCallContext) -> ExternResult {
    // Can't really exit in browser
    ExternResult::Ok
}

fn native_get_args(call: &mut ExternCallContext) -> ExternResult {
    let gc = call.gc();
    let elem_meta = ValueMeta::new(0, ValueKind::String);

    // Use injected args if present (set by run_with_args), else ["wasm"]
    let args: Vec<String> = WASM_PROG_ARGS.with(|cell| {
        cell.borrow()
            .as_ref()
            .cloned()
            .unwrap_or_else(|| vec!["wasm".to_string()])
    });

    let arr = array::create(gc, elem_meta, 8, args.len());
    for (i, arg) in args.iter().enumerate() {
        let s = string::from_rust_str(gc, arg);
        unsafe { array::set(arr, i, s as u64, 8) };
    }
    // Safety: `arr` remains a live string-reference array rooted by the return.
    let slice_ref = unsafe { slice::from_array(gc, arr) };
    call.ret_ref(0, slice_ref);
    ExternResult::Ok
}

fn native_is_terminal(call: &mut ExternCallContext) -> ExternResult {
    call.ret_i64(0, 0);
    ExternResult::Ok
}

fn native_hostname(call: &mut ExternCallContext) -> ExternResult {
    let str_ref = string::from_rust_str(call.gc(), "wasm");
    call.ret_ref(0, str_ref);
    write_nil_error(call, 1);
    ExternResult::Ok
}

fn native_executable(call: &mut ExternCallContext) -> ExternResult {
    let str_ref = string::from_rust_str(call.gc(), "/wasm");
    call.ret_ref(0, str_ref);
    write_nil_error(call, 1);
    ExternResult::Ok
}

fn native_create_temp(call: &mut ExternCallContext) -> ExternResult {
    let dir = call.arg_str(0);
    let pattern = call.arg_str(1);

    let dir = if dir.is_empty() { "/tmp" } else { dir };
    let name = format!(
        "{}{:016x}",
        pattern.replace('*', ""),
        js_sys::Math::random().to_bits()
    );
    let path = format!("{}/{}", dir, name);

    // Create parent if needed
    let _ = vfs::mkdir_all(dir, 0o755);

    let (fd, err) = vfs::open_file(&path, 16 | 32, 0o600); // O_CREATE | O_EXCL

    if let Some(msg) = err {
        call.ret_i64(0, -1);
        let str_ref = string::from_rust_str(call.gc(), "");
        call.ret_ref(1, str_ref);
        write_error_to(call, 2, &msg);
    } else {
        call.ret_i64(0, fd as i64);
        let str_ref = string::from_rust_str(call.gc(), &path);
        call.ret_ref(1, str_ref);
        write_nil_error(call, 2);
    }
    ExternResult::Ok
}

fn native_mkdir_temp(call: &mut ExternCallContext) -> ExternResult {
    let dir = call.arg_str(0);
    let pattern = call.arg_str(1);

    let dir = if dir.is_empty() { "/tmp" } else { dir };
    let name = format!(
        "{}{:016x}",
        pattern.replace('*', ""),
        js_sys::Math::random().to_bits()
    );
    let path = format!("{}/{}", dir, name);

    // Create parent if needed
    let _ = vfs::mkdir_all(dir, 0o755);

    if let Some(msg) = vfs::mkdir(&path, 0o755) {
        let str_ref = string::from_rust_str(call.gc(), "");
        call.ret_ref(0, str_ref);
        write_error_to(call, 1, &msg);
    } else {
        let str_ref = string::from_rust_str(call.gc(), &path);
        call.ret_ref(0, str_ref);
        write_nil_error(call, 1);
    }
    ExternResult::Ok
}

fn native_pipe(call: &mut ExternCallContext) -> ExternResult {
    call.ret_i64(0, -1);
    call.ret_i64(1, -1);
    write_not_supported_error(call, 2);
    ExternResult::Ok
}

fn native_chtimes(call: &mut ExternCallContext) -> ExternResult {
    write_not_supported_error(call, 0);
    ExternResult::Ok
}

fn native_find_process(call: &mut ExternCallContext) -> ExternResult {
    write_not_supported_error(call, 0);
    ExternResult::Ok
}

fn native_kill_process(call: &mut ExternCallContext) -> ExternResult {
    write_not_supported_error(call, 0);
    ExternResult::Ok
}

// =============================================================================
// Registration
// =============================================================================

pub fn register_externs(registry: &mut ExternRegistry, externs: &[ExternDef]) {
    for (id, def) in externs.iter().enumerate() {
        match def.name.as_str() {
            "os_getOsErrors" => {
                crate::register_wasm_host(registry, id as u32, &def.name, os_get_errors)
            }
            "os_getOsConsts" => {
                crate::register_wasm_host(registry, id as u32, &def.name, os_get_consts)
            }
            "os_fileRead" | "os_blocking_fileRead" => {
                crate::register_wasm_host(registry, id as u32, &def.name, file_read)
            }
            "os_fileWrite" | "os_blocking_fileWrite" => {
                crate::register_wasm_host(registry, id as u32, &def.name, file_write)
            }
            "os_fileReadAt" | "os_blocking_fileReadAt" => {
                crate::register_wasm_host(registry, id as u32, &def.name, file_read_at)
            }
            "os_fileWriteAt" | "os_blocking_fileWriteAt" => {
                crate::register_wasm_host(registry, id as u32, &def.name, file_write_at)
            }
            "os_fileSeek" => crate::register_wasm_host(registry, id as u32, &def.name, file_seek),
            "os_fileClose" => crate::register_wasm_host(registry, id as u32, &def.name, file_close),
            "os_fileSync" => crate::register_wasm_host(registry, id as u32, &def.name, file_sync),
            "os_fileStat" => crate::register_wasm_host(registry, id as u32, &def.name, file_stat),
            "os_fileTruncate" => {
                crate::register_wasm_host(registry, id as u32, &def.name, file_truncate)
            }
            "os_openFile" => crate::register_wasm_host(registry, id as u32, &def.name, open_file),
            "os_nativeMkdir" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_mkdir)
            }
            "os_nativeMkdirAll" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_mkdir_all)
            }
            "os_nativeRemove" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_remove)
            }
            "os_nativeRemoveAll" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_remove_all)
            }
            "os_nativeRename" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_rename)
            }
            "os_nativeStat" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_stat)
            }
            "os_nativeLstat" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_lstat)
            }
            "os_nativeReadDir" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_read_dir)
            }
            "os_nativeChmod" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_chmod)
            }
            "os_nativeChown" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_chown)
            }
            "os_nativeSymlink" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_symlink)
            }
            "os_nativeReadlink" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_readlink)
            }
            "os_nativeLink" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_link)
            }
            "os_nativeTruncate" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_truncate)
            }
            "os_nativeReadFile" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_read_file)
            }
            "os_nativeWriteFile" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_write_file)
            }
            "os_nativeGetenv" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_getenv)
            }
            "os_nativeSetenv" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_setenv)
            }
            "os_nativeUnsetenv" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_unsetenv)
            }
            "os_nativeEnviron" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_environ)
            }
            "os_nativeLookupEnv" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_lookup_env)
            }
            "os_nativeClearenv" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_clearenv)
            }
            "os_nativeExpandEnv" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_expand_env)
            }
            "os_nativeGetwd" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_getwd)
            }
            "os_nativeChdir" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_chdir)
            }
            "os_nativeUserHomeDir" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_user_home_dir)
            }
            "os_nativeUserCacheDir" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_user_cache_dir)
            }
            "os_nativeUserConfigDir" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_user_config_dir)
            }
            "os_nativeTempDir" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_temp_dir)
            }
            "os_nativeGetpid" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_getpid)
            }
            "os_nativeGetppid" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_getppid)
            }
            "os_nativeGetuid" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_getuid)
            }
            "os_nativeGeteuid" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_geteuid)
            }
            "os_nativeGetgid" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_getgid)
            }
            "os_nativeGetegid" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_getegid)
            }
            "os_nativeExit" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_exit)
            }
            "os_nativeGetArgs" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_get_args)
            }
            "os_nativeIsTerminal" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_is_terminal)
            }
            "os_nativeHostname" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_hostname)
            }
            "os_nativeExecutable" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_executable)
            }
            "os_nativeCreateTemp" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_create_temp)
            }
            "os_nativeMkdirTemp" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_mkdir_temp)
            }
            "os_nativePipe" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_pipe)
            }
            "os_nativeChtimes" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_chtimes)
            }
            "os_nativeFindProcess" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_find_process)
            }
            "os_nativeKillProcess" => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_kill_process)
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn wasm_os_error_interfaces_use_runtime_boxing_contract_061() {
        let source = include_str!("os.rs");
        assert!(
            source.contains("create_error(call, msg)")
                && source.contains("sentinel_errors_mut().insert(\"os\", errors)"),
            "WASM os error constants must use runtime sentinel error construction"
        );
        assert!(
            !source.contains(concat!("ValueKind::", "String as u64")),
            "WASM host shims must not hand-write string interface slot0 metadata"
        );
    }
}
