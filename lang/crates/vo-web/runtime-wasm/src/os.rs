//! os package WASM implementation backed by JS VirtualFS.

use vo_runtime::builtins::error_helper::{
    create_error, create_error_with_cause, write_error_to, write_nil_error,
};
use vo_runtime::bytecode::ExternDef;
use vo_runtime::core_types::{ValueKind, ValueMeta};
use vo_runtime::ffi::{ExternCallContext, ExternContractError, ExternRegistry, ExternResult};
use vo_runtime::objects::{slice, string};
use vo_runtime::slot::SLOT_BYTES;

use crate::{text::utf8_arg, vfs};

const ERR_NOT_SUPPORTED: &str = "operation not supported on wasm";
const MAX_ENVIRONMENT_VARIABLES: usize = 4_096;
const MAX_ENVIRONMENT_KEY_BYTES: usize = 1_024;
const MAX_ENVIRONMENT_VALUE_BYTES: usize = 1024 * 1024;
const MAX_ENVIRONMENT_BYTES: usize = 16 * 1024 * 1024;
const MAX_EXPANDED_ENV_BYTES: usize = 64 * 1024 * 1024;
const OS_ERROR_MESSAGES: [&str; 8] = [
    "file does not exist",
    "file already exists",
    "permission denied",
    "invalid argument",
    "operation timed out",
    "file already closed",
    "not a directory",
    "is a directory",
];

// Thread-local args injected by run_with_args() before running the VM.
thread_local! {
    pub static WASM_PROG_ARGS: std::cell::RefCell<Option<Vec<String>>> =
        const { std::cell::RefCell::new(None) };
    static WASM_ENV: std::cell::RefCell<std::collections::BTreeMap<Vec<u8>, Vec<u8>>> =
        const { std::cell::RefCell::new(std::collections::BTreeMap::new()) };
    static TEMP_COUNTER: std::cell::Cell<u64> = const { std::cell::Cell::new(0) };
}

fn write_not_supported_error(call: &mut ExternCallContext, slot: u16) {
    write_error_to(call, slot, ERR_NOT_SUPPORTED);
}

const MODE_DIR: u32 = 1 << 31;
const MODE_DEVICE: u32 = 1 << 26;
const MODE_CHAR_DEVICE: u32 = 1 << 21;

fn os_error_index(message: &str) -> Option<usize> {
    OS_ERROR_MESSAGES
        .iter()
        .position(|candidate| *candidate == message)
        .or_else(|| {
            (message == "i/o timeout").then_some(4).or_else(|| {
                (message.starts_with("invalid argument:")
                    || (message.starts_with("os:") && message.ends_with("contains invalid UTF-8")))
                .then_some(3)
            })
        })
}

fn io_error_index(message: &str) -> Option<usize> {
    match message {
        "EOF" => Some(0),
        "unexpected EOF" => Some(1),
        "short write" => Some(2),
        _ => None,
    }
}

fn sentinel_pair(call: &ExternCallContext<'_>, package: &str, index: usize) -> Option<(u64, u64)> {
    call.sentinel_errors()
        .get(package)
        .and_then(|errors| errors.get(index))
        .copied()
}

fn write_host_error(call: &mut ExternCallContext<'_>, slot: u16, message: &str) {
    if let Some(index) = io_error_index(message) {
        if let Some(pair) = sentinel_pair(call, "io", index) {
            call.ret_interface_pair(slot, pair);
            return;
        }
    }

    if let Some(index) = os_error_index(message) {
        init_os_errors(call);
        if let Some(pair) = sentinel_pair(call, "os", index) {
            if message == OS_ERROR_MESSAGES[index] {
                call.ret_interface_pair(slot, pair);
            } else {
                let wrapped = create_error_with_cause(call, message, pair.0, pair.1);
                call.ret_interface_pair(slot, wrapped);
            }
            return;
        }
    }

    write_error_to(call, slot, message);
}

fn os_utf8_arg(call: &ExternCallContext<'_>, slot: u16, field: &str) -> Result<String, String> {
    utf8_arg(call, slot).map_err(|_| format!("os: {field} contains invalid UTF-8"))
}

fn checked_i32(value: i64, field: &str) -> Result<i32, String> {
    i32::try_from(value).map_err(|_| invalid_argument(format!("{field} is out of range")))
}

fn invalid_argument(detail: impl std::fmt::Display) -> String {
    format!("invalid argument: {detail}")
}

fn vfs_base_name(path: &[u8]) -> &[u8] {
    let mut end = path.len();
    while end > 0 && path[end - 1] == b'/' {
        end -= 1;
    }
    if end == 0 {
        return if path.is_empty() { b"." } else { b"/" };
    }
    let start = path[..end]
        .iter()
        .rposition(|byte| *byte == b'/')
        .map_or(0, |slash| slash + 1);
    &path[start..end]
}

fn write_file_info_error(call: &mut ExternCallContext, message: &str) {
    for slot in 0..5 {
        call.ret_u64(slot, 0);
    }
    write_host_error(call, 5, message);
}

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

fn os_get_path_separators(call: &mut ExternCallContext) -> ExternResult {
    call.ret_i64(0, i64::from(b'/'));
    call.ret_i64(1, i64::from(b':'));
    ExternResult::Ok
}

// =============================================================================
// File Operations
// =============================================================================

fn open_file(call: &mut ExternCallContext) -> ExternResult {
    let name = match os_utf8_arg(call, 0, "path") {
        Ok(name) => name,
        Err(error) => {
            call.ret_i64(0, 0);
            write_host_error(call, 1, &error);
            return ExternResult::Ok;
        }
    };
    let flag = match checked_i32(call.arg_i64(1), "OpenFile flags") {
        Ok(flag) => flag,
        Err(error) => {
            call.ret_i64(0, 0);
            write_host_error(call, 1, &error);
            return ExternResult::Ok;
        }
    };
    let perm = call.arg_u64(2) as u32;

    let (fd, err) = vfs::open_file(&name, flag, perm);

    if let Some(msg) = err {
        call.ret_i64(0, 0);
        write_host_error(call, 1, &msg);
    } else {
        call.ret_i64(0, fd as i64);
        write_nil_error(call, 1);
    }
    ExternResult::Ok
}

fn write_read_completion(
    call: &mut ExternCallContext<'_>,
    buf_ref: vo_runtime::gc::GcRef,
    requested: usize,
    data: &[u8],
    error: Option<&str>,
    require_full: bool,
) {
    if data.len() > requested {
        call.ret_i64(0, 0);
        write_host_error(call, 1, "invalid browser VFS host response");
        return;
    }
    if !data.is_empty() {
        // Safety: the verified []byte remains rooted for this extern call and
        // `data` is bounded by its visible length.
        unsafe { slice::write_bytes(buf_ref, data) };
    }
    call.ret_i64(0, data.len() as i64);
    if let Some(message) = error {
        write_host_error(call, 1, message);
    } else if requested > 0 && (data.is_empty() || (require_full && data.len() < requested)) {
        write_host_error(call, 1, "EOF");
    } else {
        write_nil_error(call, 1);
    }
}

fn write_write_completion(
    call: &mut ExternCallContext<'_>,
    requested: usize,
    written: i32,
    error: Option<&str>,
) {
    let Ok(written) = usize::try_from(written) else {
        call.ret_i64(0, 0);
        write_host_error(call, 1, "invalid browser VFS host response");
        return;
    };
    if written > requested {
        call.ret_i64(0, 0);
        write_host_error(call, 1, "invalid browser VFS host response");
        return;
    }
    call.ret_i64(0, written as i64);
    if let Some(message) = error {
        write_host_error(call, 1, message);
    } else if written < requested {
        write_host_error(call, 1, "short write");
    } else {
        write_nil_error(call, 1);
    }
}

fn file_read(call: &mut ExternCallContext) -> ExternResult {
    let fd = match checked_i32(call.arg_i64(0), "file descriptor") {
        Ok(fd) => fd,
        Err(error) => {
            call.ret_i64(0, 0);
            write_host_error(call, 1, &error);
            return ExternResult::Ok;
        }
    };
    let buf_ref = call.arg_ref(1);
    // Safety: the resolved os.File.Read ABI supplies a rooted []byte argument.
    let buf_len = unsafe { slice::len(buf_ref) };
    if buf_len > vfs::MAX_VFS_IO_BYTES {
        call.ret_i64(0, 0);
        write_host_error(call, 1, &invalid_argument("read buffer is too large"));
        return ExternResult::Ok;
    }

    if fd == 0 {
        write_read_completion(call, buf_ref, buf_len, &[], None, false);
        return ExternResult::Ok;
    }
    if fd == 1 || fd == 2 {
        call.ret_i64(0, 0);
        write_host_error(call, 1, "permission denied");
        return ExternResult::Ok;
    }

    let length = u32::try_from(buf_len).unwrap_or(u32::MAX);
    let (data, error) = vfs::read(fd, length);
    write_read_completion(call, buf_ref, buf_len, &data, error.as_deref(), false);
    ExternResult::Ok
}

fn file_write(call: &mut ExternCallContext) -> ExternResult {
    let fd = match checked_i32(call.arg_i64(0), "file descriptor") {
        Ok(fd) => fd,
        Err(error) => {
            call.ret_i64(0, 0);
            write_host_error(call, 1, &error);
            return ExternResult::Ok;
        }
    };
    let buf_ref = call.arg_ref(1);
    // Safety: the resolved os.File.Write ABI supplies a rooted []byte argument.
    let buf_len = unsafe { slice::len(buf_ref) };
    if buf_len > vfs::MAX_VFS_IO_BYTES {
        call.ret_i64(0, 0);
        write_host_error(call, 1, &invalid_argument("write buffer is too large"));
        return ExternResult::Ok;
    }
    let data = unsafe { slice::byte_vec(buf_ref) };

    if fd == 0 {
        call.ret_i64(0, 0);
        write_host_error(call, 1, "permission denied");
        return ExternResult::Ok;
    }
    if fd == 1 || fd == 2 {
        call.write_output_bytes(&data);
        call.ret_i64(0, data.len() as i64);
        write_nil_error(call, 1);
        return ExternResult::Ok;
    }

    let (written, error) = vfs::write(fd, &data);
    write_write_completion(call, data.len(), written, error.as_deref());
    ExternResult::Ok
}

fn file_read_at(call: &mut ExternCallContext) -> ExternResult {
    let fd = match checked_i32(call.arg_i64(0), "file descriptor") {
        Ok(fd) => fd,
        Err(error) => {
            call.ret_i64(0, 0);
            write_host_error(call, 1, &error);
            return ExternResult::Ok;
        }
    };
    let buf_ref = call.arg_ref(1);
    let offset = call.arg_i64(2);
    // Safety: the resolved os.File.ReadAt ABI supplies a rooted []byte argument.
    let buf_len = unsafe { slice::len(buf_ref) };
    if buf_len > vfs::MAX_VFS_IO_BYTES {
        call.ret_i64(0, 0);
        write_host_error(call, 1, &invalid_argument("ReadAt buffer is too large"));
        return ExternResult::Ok;
    }

    if fd == 0 || fd == 1 || fd == 2 {
        call.ret_i64(0, 0);
        write_host_error(call, 1, "permission denied");
        return ExternResult::Ok;
    }

    let length = u32::try_from(buf_len).unwrap_or(u32::MAX);
    let (data, error) = vfs::read_at(fd, length, offset);
    write_read_completion(call, buf_ref, buf_len, &data, error.as_deref(), true);
    ExternResult::Ok
}

fn file_write_at(call: &mut ExternCallContext) -> ExternResult {
    let fd = match checked_i32(call.arg_i64(0), "file descriptor") {
        Ok(fd) => fd,
        Err(error) => {
            call.ret_i64(0, 0);
            write_host_error(call, 1, &error);
            return ExternResult::Ok;
        }
    };
    let buf_ref = call.arg_ref(1);
    let offset = call.arg_i64(2);
    // Safety: the resolved os.File.WriteAt ABI supplies a rooted []byte argument.
    let buf_len = unsafe { slice::len(buf_ref) };
    if buf_len > vfs::MAX_VFS_IO_BYTES {
        call.ret_i64(0, 0);
        write_host_error(call, 1, &invalid_argument("WriteAt buffer is too large"));
        return ExternResult::Ok;
    }
    let data = unsafe { slice::byte_vec(buf_ref) };

    if fd == 0 || fd == 1 || fd == 2 {
        call.ret_i64(0, 0);
        write_host_error(call, 1, "permission denied");
        return ExternResult::Ok;
    }

    let (written, error) = vfs::write_at(fd, &data, offset);
    write_write_completion(call, data.len(), written, error.as_deref());
    ExternResult::Ok
}

fn file_seek(call: &mut ExternCallContext) -> ExternResult {
    let fd = match checked_i32(call.arg_i64(0), "file descriptor") {
        Ok(fd) => fd,
        Err(error) => {
            call.ret_i64(0, 0);
            write_host_error(call, 1, &error);
            return ExternResult::Ok;
        }
    };
    let offset = call.arg_i64(1);
    let whence = match checked_i32(call.arg_i64(2), "seek whence") {
        Ok(whence) => whence,
        Err(error) => {
            call.ret_i64(0, 0);
            write_host_error(call, 1, &error);
            return ExternResult::Ok;
        }
    };

    let (pos, err) = vfs::seek(fd, offset, whence);

    if let Some(msg) = err {
        call.ret_i64(0, 0);
        write_host_error(call, 1, &msg);
    } else {
        call.ret_i64(0, pos);
        write_nil_error(call, 1);
    }
    ExternResult::Ok
}

fn file_close(call: &mut ExternCallContext) -> ExternResult {
    let fd = match checked_i32(call.arg_i64(0), "file descriptor") {
        Ok(fd) => fd,
        Err(error) => {
            write_host_error(call, 0, &error);
            return ExternResult::Ok;
        }
    };

    // Allow closing stdin/stdout/stderr without error
    if (0..=2).contains(&fd) {
        write_nil_error(call, 0);
        return ExternResult::Ok;
    }

    if let Some(msg) = vfs::close(fd) {
        write_host_error(call, 0, &msg);
    } else {
        write_nil_error(call, 0);
    }
    ExternResult::Ok
}

fn file_sync(call: &mut ExternCallContext) -> ExternResult {
    let fd = match checked_i32(call.arg_i64(0), "file descriptor") {
        Ok(fd) => fd,
        Err(error) => {
            write_host_error(call, 0, &error);
            return ExternResult::Ok;
        }
    };

    if (0..=2).contains(&fd) {
        write_nil_error(call, 0);
        return ExternResult::Ok;
    }

    if let Some(msg) = vfs::sync(fd) {
        write_host_error(call, 0, &msg);
    } else {
        write_nil_error(call, 0);
    }
    ExternResult::Ok
}

fn file_stat(call: &mut ExternCallContext) -> ExternResult {
    let fd = match checked_i32(call.arg_i64(0), "file descriptor") {
        Ok(fd) => fd,
        Err(error) => {
            write_file_info_error(call, &error);
            return ExternResult::Ok;
        }
    };
    let path = call.arg_string_bytes(1);
    let basename = vfs_base_name(&path).to_vec();

    if (0..=2).contains(&fd) {
        call.ret_string_bytes(0, &basename);
        call.ret_i64(1, 0);
        call.ret_u64(2, u64::from(0o666 | MODE_DEVICE | MODE_CHAR_DEVICE));
        call.ret_i64(3, 0);
        call.ret_u64(4, 0);
        write_nil_error(call, 5);
        return ExternResult::Ok;
    }

    let (size, mode, mod_time, is_dir, err) = vfs::fstat(fd);

    if let Some(msg) = err {
        write_file_info_error(call, &msg);
    } else {
        let mut mode_val = mode;
        if is_dir {
            mode_val |= MODE_DIR;
        }

        call.ret_string_bytes(0, &basename);
        call.ret_i64(1, size);
        call.ret_u64(2, mode_val as u64);
        call.ret_i64(3, mod_time / 1000); // JS ms -> seconds
        call.ret_u64(4, if is_dir { 1 } else { 0 });
        write_nil_error(call, 5);
    }
    ExternResult::Ok
}

fn file_truncate(call: &mut ExternCallContext) -> ExternResult {
    let fd = match checked_i32(call.arg_i64(0), "file descriptor") {
        Ok(fd) => fd,
        Err(error) => {
            write_host_error(call, 0, &error);
            return ExternResult::Ok;
        }
    };
    let size = call.arg_i64(1);

    if let Some(msg) = vfs::ftruncate(fd, size) {
        write_host_error(call, 0, &msg);
    } else {
        write_nil_error(call, 0);
    }
    ExternResult::Ok
}

// =============================================================================
// Directory Operations
// =============================================================================

fn native_mkdir(call: &mut ExternCallContext) -> ExternResult {
    let path = match os_utf8_arg(call, 0, "path") {
        Ok(path) => path,
        Err(error) => {
            write_host_error(call, 0, &error);
            return ExternResult::Ok;
        }
    };
    let perm = call.arg_u64(1) as u32;

    if let Some(msg) = vfs::mkdir(&path, perm) {
        write_host_error(call, 0, &msg);
    } else {
        write_nil_error(call, 0);
    }
    ExternResult::Ok
}

fn native_mkdir_all(call: &mut ExternCallContext) -> ExternResult {
    let path = match os_utf8_arg(call, 0, "path") {
        Ok(path) => path,
        Err(error) => {
            write_host_error(call, 0, &error);
            return ExternResult::Ok;
        }
    };
    let perm = call.arg_u64(1) as u32;

    if let Some(msg) = vfs::mkdir_all(&path, perm) {
        write_host_error(call, 0, &msg);
    } else {
        write_nil_error(call, 0);
    }
    ExternResult::Ok
}

fn native_remove(call: &mut ExternCallContext) -> ExternResult {
    let name = match os_utf8_arg(call, 0, "path") {
        Ok(name) => name,
        Err(error) => {
            write_host_error(call, 0, &error);
            return ExternResult::Ok;
        }
    };

    if let Some(msg) = vfs::remove(&name) {
        write_host_error(call, 0, &msg);
    } else {
        write_nil_error(call, 0);
    }
    ExternResult::Ok
}

fn native_remove_all(call: &mut ExternCallContext) -> ExternResult {
    let path = match os_utf8_arg(call, 0, "path") {
        Ok(path) => path,
        Err(error) => {
            write_host_error(call, 0, &error);
            return ExternResult::Ok;
        }
    };

    if let Some(msg) = vfs::remove_all(&path) {
        write_host_error(call, 0, &msg);
    } else {
        write_nil_error(call, 0);
    }
    ExternResult::Ok
}

fn native_rename(call: &mut ExternCallContext) -> ExternResult {
    let oldpath = match os_utf8_arg(call, 0, "old path") {
        Ok(path) => path,
        Err(error) => {
            write_host_error(call, 0, &error);
            return ExternResult::Ok;
        }
    };
    let newpath = match os_utf8_arg(call, 1, "new path") {
        Ok(path) => path,
        Err(error) => {
            write_host_error(call, 0, &error);
            return ExternResult::Ok;
        }
    };

    if let Some(msg) = vfs::rename(&oldpath, &newpath) {
        write_host_error(call, 0, &msg);
    } else {
        write_nil_error(call, 0);
    }
    ExternResult::Ok
}

fn native_stat(call: &mut ExternCallContext) -> ExternResult {
    let name = match os_utf8_arg(call, 0, "path") {
        Ok(name) => name,
        Err(error) => {
            write_file_info_error(call, &error);
            return ExternResult::Ok;
        }
    };

    let (basename, size, mode, mod_time, is_dir, err) = vfs::stat(&name);

    if let Some(msg) = err {
        write_file_info_error(call, &msg);
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

fn dir_entry_layout(
    call: &ExternCallContext<'_>,
) -> Result<(ValueMeta, usize, usize, usize, usize), String> {
    let elem_meta = call
        .named_type_metas()
        .iter()
        .find(|meta| meta.name == "os.DirEntry")
        .map(|meta| meta.underlying_meta)
        .filter(|meta| meta.value_kind() == ValueKind::Struct)
        .ok_or_else(|| "os.nativeReadDir could not resolve os.DirEntry metadata".to_string())?;
    let meta = call
        .struct_meta(elem_meta.meta_id() as usize)
        .ok_or_else(|| "os.nativeReadDir resolved invalid os.DirEntry metadata".to_string())?;
    let name = meta
        .get_field("name")
        .ok_or_else(|| "os.nativeReadDir os.DirEntry metadata is missing name".to_string())?;
    let is_dir = meta
        .get_field("isDir")
        .ok_or_else(|| "os.nativeReadDir os.DirEntry metadata is missing isDir".to_string())?;
    let mode = meta
        .get_field("mode")
        .ok_or_else(|| "os.nativeReadDir os.DirEntry metadata is missing mode".to_string())?;
    if name.slot_count != 1
        || is_dir.slot_count != 1
        || mode.slot_count != 1
        || name.type_info.value_kind() != ValueKind::String
        || is_dir.type_info.value_kind() != ValueKind::Bool
        || mode.type_info.value_kind() != ValueKind::Uint32
    {
        return Err(
            "os.nativeReadDir os.DirEntry field types or widths do not match the native ABI"
                .to_string(),
        );
    }
    let elem_slots = usize::from(meta.slot_count());
    let name_offset = usize::from(name.offset);
    let is_dir_offset = usize::from(is_dir.offset);
    let mode_offset = usize::from(mode.offset);
    if [name_offset, is_dir_offset, mode_offset]
        .into_iter()
        .any(|offset| offset >= elem_slots)
    {
        return Err(
            "os.nativeReadDir os.DirEntry field offset exceeds its struct layout".to_string(),
        );
    }
    Ok((
        elem_meta,
        elem_slots,
        name_offset,
        is_dir_offset,
        mode_offset,
    ))
}

fn native_read_dir(call: &mut ExternCallContext) -> ExternResult {
    let name = match os_utf8_arg(call, 0, "path") {
        Ok(name) => name,
        Err(error) => {
            call.ret_nil(0);
            write_host_error(call, 1, &error);
            return ExternResult::Ok;
        }
    };

    let (entries, err) = vfs::read_dir(&name);

    if let Some(msg) = err {
        call.ret_nil(0);
        write_host_error(call, 1, &msg);
    } else {
        let len = entries.len();
        let (elem_meta, elem_slots, name_offset, is_dir_offset, mode_offset) =
            match dir_entry_layout(call) {
                Ok(layout) => layout,
                Err(error) => return ExternResult::Panic(error),
            };
        let Some(elem_bytes) = elem_slots.checked_mul(SLOT_BYTES) else {
            return ExternResult::Panic(
                "os.nativeReadDir os.DirEntry layout exceeds the target address width".to_string(),
            );
        };
        let result = slice::create(call.gc(), elem_meta, elem_bytes, len, len);
        if result.is_null() {
            return ExternResult::Panic(
                "os.nativeReadDir could not allocate the result slice".to_string(),
            );
        }

        let owner = unsafe { slice::owner_ref(result) };
        let mut value = vec![0u64; elem_slots];
        for (i, (entry_name, is_dir, mode)) in entries.iter().enumerate() {
            let name_ref = call.alloc_string_bytes(entry_name.as_bytes());
            let mut mode_val = *mode;
            if *is_dir {
                mode_val |= MODE_DIR;
            }
            value.fill(0);
            value[name_offset] = name_ref as u64;
            value[is_dir_offset] = u64::from(*is_dir);
            value[mode_offset] = u64::from(mode_val);
            call.typed_write_barrier_by_meta(owner, &value, elem_meta);
            unsafe {
                slice::write_logical_slots(result, i, &value);
            }
        }
        call.gc().mark_allocated_for_scan(owner);
        call.ret_ref(0, result);
        write_nil_error(call, 1);
    }
    ExternResult::Ok
}

fn native_chmod(call: &mut ExternCallContext) -> ExternResult {
    let name = match os_utf8_arg(call, 0, "path") {
        Ok(name) => name,
        Err(error) => {
            write_host_error(call, 0, &error);
            return ExternResult::Ok;
        }
    };
    let mode = call.arg_u64(1) as u32;

    if let Some(msg) = vfs::chmod(&name, mode) {
        write_host_error(call, 0, &msg);
    } else {
        write_nil_error(call, 0);
    }
    ExternResult::Ok
}

fn native_truncate(call: &mut ExternCallContext) -> ExternResult {
    let name = match os_utf8_arg(call, 0, "path") {
        Ok(name) => name,
        Err(error) => {
            write_host_error(call, 0, &error);
            return ExternResult::Ok;
        }
    };
    let size = call.arg_i64(1);

    if let Some(msg) = vfs::truncate(&name, size) {
        write_host_error(call, 0, &msg);
    } else {
        write_nil_error(call, 0);
    }
    ExternResult::Ok
}

fn native_read_file(call: &mut ExternCallContext) -> ExternResult {
    let name = match os_utf8_arg(call, 0, "path") {
        Ok(name) => name,
        Err(error) => {
            call.ret_nil(0);
            write_host_error(call, 1, &error);
            return ExternResult::Ok;
        }
    };

    let (data, err) = vfs::read_file(&name);

    if let Some(msg) = err {
        call.ret_nil(0);
        write_host_error(call, 1, &msg);
    } else {
        call.ret_bytes(0, &data);
        write_nil_error(call, 1);
    }
    ExternResult::Ok
}

fn native_write_file(call: &mut ExternCallContext) -> ExternResult {
    let name = match os_utf8_arg(call, 0, "path") {
        Ok(name) => name,
        Err(error) => {
            write_host_error(call, 0, &error);
            return ExternResult::Ok;
        }
    };
    let data_ref = call.arg_ref(1);
    let data_len = unsafe { slice::len(data_ref) };
    if data_len > vfs::MAX_VFS_FILE_BYTES {
        write_host_error(call, 0, "file too large");
        return ExternResult::Ok;
    }
    let data = unsafe { slice::byte_vec(data_ref) };
    let perm = call.arg_u64(2) as u32;

    if let Some(msg) = vfs::write_file(&name, &data, perm) {
        write_host_error(call, 0, &msg);
    } else {
        write_nil_error(call, 0);
    }
    ExternResult::Ok
}

// =============================================================================
// Not Supported Operations
// =============================================================================

fn native_chown(call: &mut ExternCallContext) -> ExternResult {
    write_not_supported_error(call, 0);
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
// Process-local synthetic environment
// =============================================================================

fn validate_env_key(key: &[u8]) -> Result<(), String> {
    if key.is_empty() {
        return Err(invalid_argument(
            "environment variable name must not be empty",
        ));
    }
    if key.contains(&b'=') {
        return Err(invalid_argument(
            "environment variable name must not contain '='",
        ));
    }
    if key.contains(&0) {
        return Err(invalid_argument(
            "environment variable name must not contain NUL",
        ));
    }
    if key.len() > MAX_ENVIRONMENT_KEY_BYTES {
        return Err(invalid_argument("environment variable name is too long"));
    }
    Ok(())
}

fn validate_env_value(value: &[u8]) -> Result<(), String> {
    if value.contains(&0) {
        return Err(invalid_argument(
            "environment variable value must not contain NUL",
        ));
    }
    if value.len() > MAX_ENVIRONMENT_VALUE_BYTES {
        return Err(invalid_argument("environment variable value is too large"));
    }
    Ok(())
}

fn lookup_env(key: &[u8]) -> Option<Vec<u8>> {
    validate_env_key(key).ok()?;
    WASM_ENV.with(|environment| environment.borrow().get(key).cloned())
}

fn is_shell_special_var(byte: u8) -> bool {
    matches!(
        byte,
        b'*' | b'#' | b'$' | b'@' | b'!' | b'?' | b'-' | b'0'..=b'9'
    )
}

fn is_shell_name_byte(byte: u8) -> bool {
    byte == b'_' || byte.is_ascii_alphanumeric()
}

fn shell_name(input: &[u8]) -> (&[u8], usize) {
    if input[0] == b'{' {
        if input.len() > 2 && is_shell_special_var(input[1]) && input[2] == b'}' {
            return (&input[1..2], 3);
        }
        for index in 1..input.len() {
            if input[index] == b'}' {
                return if index == 1 {
                    (&[], 2)
                } else {
                    (&input[1..index], index + 1)
                };
            }
        }
        // Match os.Expand: an unterminated braced reference leaves the '$'
        // literal in the output and resumes scanning at the opening brace.
        return (&[], 0);
    }
    if is_shell_special_var(input[0]) {
        return (&input[..1], 1);
    }
    let width = input
        .iter()
        .position(|byte| !is_shell_name_byte(*byte))
        .unwrap_or(input.len());
    (&input[..width], width)
}

fn append_expanded(output: &mut Vec<u8>, bytes: &[u8]) -> Option<()> {
    let new_len = output.len().checked_add(bytes.len())?;
    if new_len > MAX_EXPANDED_ENV_BYTES {
        return None;
    }
    output.try_reserve(bytes.len()).ok()?;
    output.extend_from_slice(bytes);
    Some(())
}

fn expand_env(input: &[u8]) -> Option<Vec<u8>> {
    if input.len() > MAX_EXPANDED_ENV_BYTES {
        return None;
    }
    let mut output = Vec::new();
    output.try_reserve(input.len()).ok()?;
    let mut literal_start = 0usize;
    let mut index = 0usize;
    while index < input.len() {
        if input[index] == b'$' && index + 1 < input.len() {
            append_expanded(&mut output, &input[literal_start..index])?;
            let (name, width) = shell_name(&input[index + 1..]);
            if name.is_empty() && width == 0 {
                append_expanded(&mut output, b"$")?;
            } else if let Some(value) = lookup_env(name) {
                append_expanded(&mut output, &value)?;
            }
            index += width + 1;
            literal_start = index;
        } else {
            index += 1;
        }
    }
    append_expanded(&mut output, &input[literal_start..])?;
    Some(output)
}

fn native_getenv(call: &mut ExternCallContext) -> ExternResult {
    let key = call.arg_string_bytes(0);
    call.ret_string_bytes(0, &lookup_env(&key).unwrap_or_default());
    ExternResult::Ok
}

fn native_setenv(call: &mut ExternCallContext) -> ExternResult {
    let key = call.arg_string_bytes(0);
    let value = call.arg_string_bytes(1);
    if let Err(error) = validate_env_key(&key).and_then(|()| validate_env_value(&value)) {
        write_host_error(call, 0, &error);
    } else {
        let result = WASM_ENV.with(|environment| {
            let mut environment = environment.borrow_mut();
            if !environment.contains_key(&key) && environment.len() >= MAX_ENVIRONMENT_VARIABLES {
                return Err(invalid_argument("too many environment variables"));
            }
            let previous = environment
                .get(&key)
                .map_or(0, |previous| key.len() + previous.len() + 1);
            let current = environment
                .iter()
                .map(|(key, value)| key.len() + value.len() + 1)
                .sum::<usize>();
            let projected = current - previous + key.len() + value.len() + 1;
            if projected > MAX_ENVIRONMENT_BYTES {
                return Err(invalid_argument("environment is too large"));
            }
            environment.insert(key, value);
            Ok(())
        });
        match result {
            Ok(()) => write_nil_error(call, 0),
            Err(error) => write_host_error(call, 0, &error),
        }
    }
    ExternResult::Ok
}

fn native_unsetenv(call: &mut ExternCallContext) -> ExternResult {
    let key = call.arg_string_bytes(0);
    if let Err(error) = validate_env_key(&key) {
        write_host_error(call, 0, &error);
    } else {
        WASM_ENV.with(|environment| {
            environment.borrow_mut().remove(&key);
        });
        write_nil_error(call, 0);
    }
    ExternResult::Ok
}

fn native_environ(call: &mut ExternCallContext) -> ExternResult {
    let entries = WASM_ENV.with(|environment| {
        environment
            .borrow()
            .iter()
            .map(|(key, value)| {
                let mut entry = Vec::with_capacity(key.len() + value.len() + 1);
                entry.extend_from_slice(key);
                entry.push(b'=');
                entry.extend_from_slice(value);
                entry
            })
            .collect::<Vec<_>>()
    });
    call.ret_string_bytes_slice(0, &entries);
    ExternResult::Ok
}

fn native_lookup_env(call: &mut ExternCallContext) -> ExternResult {
    let key = call.arg_string_bytes(0);
    if let Some(value) = lookup_env(&key) {
        call.ret_string_bytes(0, &value);
        call.ret_bool(1, true);
    } else {
        call.ret_string_bytes(0, b"");
        call.ret_bool(1, false);
    }
    ExternResult::Ok
}

fn native_clearenv(_call: &mut ExternCallContext) -> ExternResult {
    WASM_ENV.with(|environment| environment.borrow_mut().clear());
    ExternResult::Ok
}

fn native_expand_env(call: &mut ExternCallContext) -> ExternResult {
    let bytes = call.arg_string_bytes(0);
    call.ret_string_bytes(0, expand_env(&bytes).as_deref().unwrap_or(&bytes));
    ExternResult::Ok
}

// =============================================================================
// Working Directory
// =============================================================================

fn native_getwd(call: &mut ExternCallContext) -> ExternResult {
    let (path, error) = vfs::getwd();
    if let Some(message) = error {
        call.ret_str(0, "");
        write_host_error(call, 1, &message);
    } else {
        call.ret_str(0, &path);
        write_nil_error(call, 1);
    }
    ExternResult::Ok
}

fn native_chdir(call: &mut ExternCallContext) -> ExternResult {
    let path = match os_utf8_arg(call, 0, "working directory") {
        Ok(path) => path,
        Err(error) => {
            write_host_error(call, 0, &error);
            return ExternResult::Ok;
        }
    };
    if let Some(message) = vfs::chdir(&path) {
        write_host_error(call, 0, &message);
    } else {
        write_nil_error(call, 0);
    }
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

fn native_exit(call: &mut ExternCallContext) -> ExternResult {
    ExternResult::Exit(call.arg_i64(0) as i32)
}

fn native_get_args(call: &mut ExternCallContext) -> ExternResult {
    let args = if call.program_args().is_empty() {
        WASM_PROG_ARGS.with(|cell| {
            cell.borrow()
                .as_ref()
                .map(|args| args.iter().map(|arg| arg.as_bytes().to_vec()).collect())
                .unwrap_or_else(|| vec![b"wasm".to_vec()])
        })
    } else {
        call.program_args().to_vec()
    };
    call.ret_string_bytes_slice(0, &args);
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

fn temporary_name(pattern: &str, token: &str) -> Result<String, String> {
    if pattern.contains('/') || pattern.contains('\0') {
        return Err(invalid_argument(
            "temporary file pattern must not contain a path separator",
        ));
    }
    if let Some(star) = pattern.rfind('*') {
        Ok(format!(
            "{}{}{}",
            &pattern[..star],
            token,
            &pattern[star + 1..]
        ))
    } else {
        Ok(format!("{pattern}{token}"))
    }
}

fn temporary_path(dir: &str, pattern: &str, attempt: u64) -> Result<String, String> {
    let serial = TEMP_COUNTER.with(|counter| {
        let value = counter.get();
        counter.set(value.wrapping_add(1));
        value
    }) ^ attempt;
    let entropy = js_sys::Math::random().to_bits() ^ serial.rotate_left(17);
    let name = temporary_name(pattern, &format!("{entropy:016x}"))?;
    if dir == "/" {
        Ok(format!("/{name}"))
    } else {
        Ok(format!("{}/{name}", dir.trim_end_matches('/')))
    }
}

fn native_create_temp(call: &mut ExternCallContext) -> ExternResult {
    let dir = match os_utf8_arg(call, 0, "temporary directory") {
        Ok(dir) => dir,
        Err(error) => {
            call.ret_i64(0, 0);
            call.ret_str(1, "");
            write_host_error(call, 2, &error);
            return ExternResult::Ok;
        }
    };
    let pattern = match os_utf8_arg(call, 1, "temporary file pattern") {
        Ok(pattern) => pattern,
        Err(error) => {
            call.ret_i64(0, 0);
            call.ret_str(1, "");
            write_host_error(call, 2, &error);
            return ExternResult::Ok;
        }
    };

    let dir = if dir.is_empty() { "/tmp" } else { &dir };
    for attempt in 0..10_000 {
        let path = match temporary_path(dir, &pattern, attempt) {
            Ok(path) => path,
            Err(error) => {
                call.ret_i64(0, 0);
                call.ret_str(1, "");
                write_host_error(call, 2, &error);
                return ExternResult::Ok;
            }
        };
        let (fd, error) = vfs::open_file(&path, 2 | 16 | 32, 0o600);
        match error.as_deref() {
            Some("file already exists") => continue,
            Some(error) => {
                call.ret_i64(0, 0);
                call.ret_str(1, "");
                write_host_error(call, 2, error);
                return ExternResult::Ok;
            }
            None => {
                call.ret_i64(0, i64::from(fd));
                call.ret_str(1, &path);
                write_nil_error(call, 2);
                return ExternResult::Ok;
            }
        }
    }
    call.ret_i64(0, 0);
    call.ret_str(1, "");
    write_host_error(call, 2, "file already exists");
    ExternResult::Ok
}

fn native_mkdir_temp(call: &mut ExternCallContext) -> ExternResult {
    let dir = match os_utf8_arg(call, 0, "temporary directory") {
        Ok(dir) => dir,
        Err(error) => {
            call.ret_str(0, "");
            write_host_error(call, 1, &error);
            return ExternResult::Ok;
        }
    };
    let pattern = match os_utf8_arg(call, 1, "temporary directory pattern") {
        Ok(pattern) => pattern,
        Err(error) => {
            call.ret_str(0, "");
            write_host_error(call, 1, &error);
            return ExternResult::Ok;
        }
    };

    let dir = if dir.is_empty() { "/tmp" } else { &dir };
    for attempt in 0..10_000 {
        let path = match temporary_path(dir, &pattern, attempt) {
            Ok(path) => path,
            Err(error) => {
                call.ret_str(0, "");
                write_host_error(call, 1, &error);
                return ExternResult::Ok;
            }
        };
        match vfs::mkdir(&path, 0o700).as_deref() {
            Some("file already exists") => continue,
            Some(error) => {
                call.ret_str(0, "");
                write_host_error(call, 1, error);
                return ExternResult::Ok;
            }
            None => {
                call.ret_str(0, &path);
                write_nil_error(call, 1);
                return ExternResult::Ok;
            }
        }
    }
    call.ret_str(0, "");
    write_host_error(call, 1, "file already exists");
    ExternResult::Ok
}

fn native_pipe(call: &mut ExternCallContext) -> ExternResult {
    call.ret_i64(0, 0);
    call.ret_i64(1, 0);
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
            vo_runtime::vo_extern_name!("os", "getOsErrors") => {
                crate::register_wasm_host(registry, id as u32, &def.name, os_get_errors)
            }
            vo_runtime::vo_extern_name!("os", "getOsConsts") => {
                crate::register_wasm_host(registry, id as u32, &def.name, os_get_consts)
            }
            vo_runtime::vo_extern_name!("os", "getPathSeparators") => {
                crate::register_wasm_host(registry, id as u32, &def.name, os_get_path_separators)
            }
            vo_runtime::vo_extern_name!("os", "fileRead")
            | vo_runtime::vo_extern_name!("os", "blocking_fileRead") => {
                crate::register_wasm_host(registry, id as u32, &def.name, file_read)
            }
            vo_runtime::vo_extern_name!("os", "fileWrite")
            | vo_runtime::vo_extern_name!("os", "blocking_fileWrite") => {
                crate::register_wasm_host(registry, id as u32, &def.name, file_write)
            }
            vo_runtime::vo_extern_name!("os", "blocking_fileReadAt") => {
                crate::register_wasm_host(registry, id as u32, &def.name, file_read_at)
            }
            vo_runtime::vo_extern_name!("os", "blocking_fileWriteAt") => {
                crate::register_wasm_host(registry, id as u32, &def.name, file_write_at)
            }
            vo_runtime::vo_extern_name!("os", "fileSeek") => {
                crate::register_wasm_host(registry, id as u32, &def.name, file_seek)
            }
            vo_runtime::vo_extern_name!("os", "fileClose") => {
                crate::register_wasm_host(registry, id as u32, &def.name, file_close)
            }
            vo_runtime::vo_extern_name!("os", "fileSync") => {
                crate::register_wasm_host(registry, id as u32, &def.name, file_sync)
            }
            vo_runtime::vo_extern_name!("os", "fileStat") => {
                crate::register_wasm_host(registry, id as u32, &def.name, file_stat)
            }
            vo_runtime::vo_extern_name!("os", "fileTruncate") => {
                crate::register_wasm_host(registry, id as u32, &def.name, file_truncate)
            }
            vo_runtime::vo_extern_name!("os", "openFile") => {
                crate::register_wasm_host(registry, id as u32, &def.name, open_file)
            }
            vo_runtime::vo_extern_name!("os", "nativeMkdir") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_mkdir)
            }
            vo_runtime::vo_extern_name!("os", "nativeMkdirAll") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_mkdir_all)
            }
            vo_runtime::vo_extern_name!("os", "nativeRemove") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_remove)
            }
            vo_runtime::vo_extern_name!("os", "nativeRemoveAll") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_remove_all)
            }
            vo_runtime::vo_extern_name!("os", "nativeRename") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_rename)
            }
            vo_runtime::vo_extern_name!("os", "nativeStat") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_stat)
            }
            vo_runtime::vo_extern_name!("os", "nativeLstat") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_lstat)
            }
            vo_runtime::vo_extern_name!("os", "nativeReadDir") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_read_dir)
            }
            vo_runtime::vo_extern_name!("os", "nativeChmod") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_chmod)
            }
            vo_runtime::vo_extern_name!("os", "nativeChown") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_chown)
            }
            vo_runtime::vo_extern_name!("os", "nativeSymlink") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_symlink)
            }
            vo_runtime::vo_extern_name!("os", "nativeReadlink") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_readlink)
            }
            vo_runtime::vo_extern_name!("os", "nativeLink") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_link)
            }
            vo_runtime::vo_extern_name!("os", "nativeTruncate") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_truncate)
            }
            vo_runtime::vo_extern_name!("os", "nativeReadFile") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_read_file)
            }
            vo_runtime::vo_extern_name!("os", "nativeWriteFile") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_write_file)
            }
            vo_runtime::vo_extern_name!("os", "nativeGetenv") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_getenv)
            }
            vo_runtime::vo_extern_name!("os", "nativeSetenv") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_setenv)
            }
            vo_runtime::vo_extern_name!("os", "nativeUnsetenv") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_unsetenv)
            }
            vo_runtime::vo_extern_name!("os", "nativeEnviron") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_environ)
            }
            vo_runtime::vo_extern_name!("os", "nativeLookupEnv") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_lookup_env)
            }
            vo_runtime::vo_extern_name!("os", "nativeClearenv") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_clearenv)
            }
            vo_runtime::vo_extern_name!("os", "nativeExpandEnv") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_expand_env)
            }
            vo_runtime::vo_extern_name!("os", "nativeGetwd") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_getwd)
            }
            vo_runtime::vo_extern_name!("os", "nativeChdir") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_chdir)
            }
            vo_runtime::vo_extern_name!("os", "nativeUserHomeDir") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_user_home_dir)
            }
            vo_runtime::vo_extern_name!("os", "nativeUserCacheDir") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_user_cache_dir)
            }
            vo_runtime::vo_extern_name!("os", "nativeUserConfigDir") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_user_config_dir)
            }
            vo_runtime::vo_extern_name!("os", "nativeTempDir") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_temp_dir)
            }
            vo_runtime::vo_extern_name!("os", "nativeGetpid") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_getpid)
            }
            vo_runtime::vo_extern_name!("os", "nativeGetppid") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_getppid)
            }
            vo_runtime::vo_extern_name!("os", "nativeGetuid") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_getuid)
            }
            vo_runtime::vo_extern_name!("os", "nativeGeteuid") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_geteuid)
            }
            vo_runtime::vo_extern_name!("os", "nativeGetgid") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_getgid)
            }
            vo_runtime::vo_extern_name!("os", "nativeGetegid") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_getegid)
            }
            vo_runtime::vo_extern_name!("os", "nativeExit") => registry
                .try_register_wasm_host_with_effects(
                    id as u32,
                    &def.name,
                    native_exit,
                    vo_runtime::bytecode::ExternEffects::MAY_EXIT,
                ),
            vo_runtime::vo_extern_name!("os", "nativeGetArgs") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_get_args)
            }
            vo_runtime::vo_extern_name!("os", "nativeIsTerminal") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_is_terminal)
            }
            vo_runtime::vo_extern_name!("os", "nativeHostname") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_hostname)
            }
            vo_runtime::vo_extern_name!("os", "nativeExecutable") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_executable)
            }
            vo_runtime::vo_extern_name!("os", "nativeCreateTemp") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_create_temp)
            }
            vo_runtime::vo_extern_name!("os", "nativeMkdirTemp") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_mkdir_temp)
            }
            vo_runtime::vo_extern_name!("os", "nativePipe") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_pipe)
            }
            vo_runtime::vo_extern_name!("os", "nativeChtimes") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_chtimes)
            }
            vo_runtime::vo_extern_name!("os", "nativeFindProcess") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_find_process)
            }
            vo_runtime::vo_extern_name!("os", "nativeKillProcess") => {
                crate::register_wasm_host(registry, id as u32, &def.name, native_kill_process)
            }
            _ => Ok(()),
        }?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::{
        checked_i32, expand_env, os_error_index, temporary_name, validate_env_key,
        validate_env_value, vfs_base_name, WASM_ENV,
    };

    #[test]
    fn file_stat_uses_the_opened_virtual_path_base_name() {
        assert_eq!(vfs_base_name(b""), b".");
        assert_eq!(vfs_base_name(b"/"), b"/");
        assert_eq!(vfs_base_name(b"/directory/file/"), b"file");
        assert_eq!(vfs_base_name(b"plain"), b"plain");
        assert_eq!(vfs_base_name(b"/raw/\xff\xfe/"), b"\xff\xfe");
    }

    #[test]
    fn wasm_environment_round_trips_bytes_and_expands_shell_names() {
        WASM_ENV.with(|environment| {
            let mut environment = environment.borrow_mut();
            environment.clear();
            environment.insert(b"NAME".to_vec(), b"value\xff".to_vec());
        });
        assert_eq!(
            expand_env(b"<$NAME> ${NAME}").as_deref(),
            Some(b"<value\xff> value\xff".as_slice())
        );
        assert_eq!(expand_env(b"$MISSING/x").as_deref(), Some(b"/x".as_slice()));
        assert_eq!(
            expand_env(b"trailing$").as_deref(),
            Some(b"trailing$".as_slice())
        );
        assert_eq!(
            expand_env(b"unterminated-${NAME").as_deref(),
            Some(b"unterminated-${NAME".as_slice())
        );
    }

    #[test]
    fn wasm_environment_and_integer_boundaries_are_validated() {
        assert!(validate_env_key(b"").is_err());
        assert!(validate_env_key(b"BAD=KEY").is_err());
        assert!(validate_env_key(b"BAD\0KEY").is_err());
        assert!(validate_env_value(b"BAD\0VALUE").is_err());
        assert!(checked_i32(i64::from(i32::MAX) + 1, "descriptor").is_err());
        assert_eq!(os_error_index("invalid argument: detail"), Some(3));
        assert_eq!(os_error_index("os: path contains invalid UTF-8"), Some(3));
        assert_eq!(
            temporary_name("prefix-*-suffix", "token").as_deref(),
            Ok("prefix-token-suffix")
        );
        assert!(temporary_name("sub/path", "token").is_err());
    }

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
