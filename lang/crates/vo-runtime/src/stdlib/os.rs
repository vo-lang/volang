//! os package native function implementations.

use std::fs::{self, File, OpenOptions};
use std::io::{Read, Write, Seek, SeekFrom};
use std::os::unix::fs::{MetadataExt, PermissionsExt, symlink};
use std::collections::HashMap;
use std::sync::Mutex;

use vo_common_core::types::ValueKind;
use vo_ffi_macro::{vo_extern_ctx, vo_consts};

use crate::ffi::{ExternCallContext, ExternResult};
use crate::gc::{Gc, GcRef};
use crate::objects::slice;
use super::error_helper::{write_error_to, write_nil_error};

// Import error codes from centralized errors/code.vo
vo_consts! {
    "errors" => {
        CodeEOF,
        CodeOsInvalid,
        CodeOsPermission,
        CodeOsExist,
        CodeOsNotExist,
    }
}

const MODE_DIR: u32 = 1 << 31;
const MODE_SYMLINK: u32 = 1 << 27;

lazy_static::lazy_static! {
    static ref FILE_HANDLES: Mutex<HashMap<i32, File>> = Mutex::new(HashMap::new());
    static ref NEXT_FD: Mutex<i32> = Mutex::new(100);
    static ref TEMP_COUNTER: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
}

fn temp_random() -> u64 {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};
    let mut hasher = DefaultHasher::new();
    std::process::id().hash(&mut hasher);
    std::thread::current().id().hash(&mut hasher);
    std::time::SystemTime::now().hash(&mut hasher);
    TEMP_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed).hash(&mut hasher);
    hasher.finish()
}

fn register_file(file: File) -> i32 {
    let mut handles = FILE_HANDLES.lock().unwrap();
    let mut next_fd = NEXT_FD.lock().unwrap();
    let fd = *next_fd;
    *next_fd += 1;
    handles.insert(fd, file);
    fd
}

fn remove_file(fd: i32) -> Option<File> {
    FILE_HANDLES.lock().unwrap().remove(&fd)
}

macro_rules! with_file_mut {
    ($fd:expr, $call:expr, $err_slot:expr, |$file:ident| $body:expr) => {{
        let mut handles = FILE_HANDLES.lock().unwrap();
        if let Some($file) = handles.get_mut(&$fd) { $body }
        else { write_error_to($call, $err_slot, CODE_OS_INVALID, "invalid file descriptor"); }
    }};
    ($fd:expr, $call:expr, $err_slot:expr, ret0, |$file:ident| $body:expr) => {{
        let mut handles = FILE_HANDLES.lock().unwrap();
        if let Some($file) = handles.get_mut(&$fd) { $body }
        else { $call.ret_i64(0, 0); write_error_to($call, $err_slot, CODE_OS_INVALID, "invalid file descriptor"); }
    }};
}

macro_rules! with_file {
    ($fd:expr, $call:expr, $err_slot:expr, |$file:ident| $body:expr) => {{
        let handles = FILE_HANDLES.lock().unwrap();
        if let Some($file) = handles.get(&$fd) { $body }
        else { write_error_to($call, $err_slot, CODE_OS_INVALID, "invalid file descriptor"); }
    }};
    ($fd:expr, $call:expr, $err_slot:expr, nil $n:expr, |$file:ident| $body:expr) => {{
        let handles = FILE_HANDLES.lock().unwrap();
        if let Some($file) = handles.get(&$fd) { $body }
        else { for i in 0..$n { $call.ret_nil(i as u16); } write_error_to($call, $err_slot, CODE_OS_INVALID, "invalid fd"); }
    }};
}

fn io_error_to_code(err: &std::io::Error) -> isize {
    match err.kind() {
        std::io::ErrorKind::NotFound => CODE_OS_NOT_EXIST,
        std::io::ErrorKind::PermissionDenied => CODE_OS_PERMISSION,
        std::io::ErrorKind::AlreadyExists => CODE_OS_EXIST,
        _ => CODE_OS_INVALID,
    }
}

fn write_io_error(call: &mut ExternCallContext, ret_slot: u16, err: std::io::Error) {
    write_error_to(call, ret_slot, io_error_to_code(&err), &err.to_string());
}

fn metadata_to_file_info(call: &mut ExternCallContext, name: &str, meta: &fs::Metadata) -> GcRef {
    let slots = 5u16;
    let file_info = call.gc_alloc(slots, &[]);
    let name_ref = call.alloc_str(name);
    let size = meta.len() as i64;
    let mut mode = meta.permissions().mode();
    if meta.is_dir() { mode |= MODE_DIR; }
    if meta.file_type().is_symlink() { mode |= MODE_SYMLINK; }
    let mod_time = meta.mtime();
    let is_dir = if meta.is_dir() { 1u64 } else { 0u64 };
    
    unsafe {
        Gc::write_slot(file_info, 0, name_ref as u64);
        Gc::write_slot(file_info, 1, size as u64);
        Gc::write_slot(file_info, 2, mode as u64);
        Gc::write_slot(file_info, 3, mod_time as u64);
        Gc::write_slot(file_info, 4, is_dir);
    }
    file_info
}

#[vo_extern_ctx("os", "fileRead")]
fn os_file_read(call: &mut ExternCallContext) -> ExternResult {
    let fd = call.arg_i64(0) as i32;
    let buf_ref = call.arg_ref(1);
    let buf_len = slice::len(buf_ref);
    let buf_ptr = slice::data_ptr(buf_ref);
    let buf = unsafe { std::slice::from_raw_parts_mut(buf_ptr, buf_len) };
    
    if fd == 0 {
        match std::io::stdin().read(buf) {
            Ok(n) => { call.ret_i64(0, n as i64); write_nil_error(call, 1); }
            Err(e) => { call.ret_i64(0, 0); write_io_error(call, 1, e); }
        }
        return ExternResult::Ok;
    }
    
    with_file_mut!(fd, call, 1, ret0, |file| {
        match file.read(buf) {
            Ok(0) => { call.ret_i64(0, 0); write_error_to(call, 1, CODE_EOF, "EOF"); }
            Ok(n) => { call.ret_i64(0, n as i64); write_nil_error(call, 1); }
            Err(e) => { call.ret_i64(0, 0); write_io_error(call, 1, e); }
        }
    });
    ExternResult::Ok
}

#[vo_extern_ctx("os", "fileWrite")]
fn os_file_write(call: &mut ExternCallContext) -> ExternResult {
    let fd = call.arg_i64(0) as i32;
    let buf_ref = call.arg_ref(1);
    let buf_len = slice::len(buf_ref);
    let buf_ptr = slice::data_ptr(buf_ref);
    let buf = unsafe { std::slice::from_raw_parts(buf_ptr, buf_len) };
    
    if fd == 1 || fd == 2 {
        let result = if fd == 1 { std::io::stdout().write(buf) } else { std::io::stderr().write(buf) };
        match result {
            Ok(n) => { call.ret_i64(0, n as i64); write_nil_error(call, 1); }
            Err(e) => { call.ret_i64(0, 0); write_io_error(call, 1, e); }
        }
        return ExternResult::Ok;
    }
    
    with_file_mut!(fd, call, 1, ret0, |file| {
        match file.write(buf) {
            Ok(n) => { call.ret_i64(0, n as i64); write_nil_error(call, 1); }
            Err(e) => { call.ret_i64(0, 0); write_io_error(call, 1, e); }
        }
    });
    ExternResult::Ok
}

#[vo_extern_ctx("os", "fileReadAt")]
fn os_file_read_at(call: &mut ExternCallContext) -> ExternResult {
    let fd = call.arg_i64(0) as i32;
    let buf_ref = call.arg_ref(1);
    let offset = call.arg_i64(2);
    let buf_len = slice::len(buf_ref);
    let buf_ptr = slice::data_ptr(buf_ref);
    let buf = unsafe { std::slice::from_raw_parts_mut(buf_ptr, buf_len) };
    
    with_file_mut!(fd, call, 1, ret0, |file| {
        let current_pos = file.stream_position().unwrap_or(0);
        match file.seek(SeekFrom::Start(offset as u64)) {
            Err(e) => { call.ret_i64(0, 0); write_io_error(call, 1, e); }
            Ok(_) => {
                let result = file.read(buf);
                let _ = file.seek(SeekFrom::Start(current_pos));
                match result {
                    Ok(0) => { call.ret_i64(0, 0); write_error_to(call, 1, CODE_EOF, "EOF"); }
                    Ok(n) => { call.ret_i64(0, n as i64); write_nil_error(call, 1); }
                    Err(e) => { call.ret_i64(0, 0); write_io_error(call, 1, e); }
                }
            }
        }
    });
    ExternResult::Ok
}

#[vo_extern_ctx("os", "fileWriteAt")]
fn os_file_write_at(call: &mut ExternCallContext) -> ExternResult {
    let fd = call.arg_i64(0) as i32;
    let buf_ref = call.arg_ref(1);
    let offset = call.arg_i64(2);
    let buf_len = slice::len(buf_ref);
    let buf_ptr = slice::data_ptr(buf_ref);
    let buf = unsafe { std::slice::from_raw_parts(buf_ptr, buf_len) };
    
    with_file_mut!(fd, call, 1, ret0, |file| {
        let current_pos = file.stream_position().unwrap_or(0);
        match file.seek(SeekFrom::Start(offset as u64)) {
            Err(e) => { call.ret_i64(0, 0); write_io_error(call, 1, e); }
            Ok(_) => {
                let result = file.write(buf);
                let _ = file.seek(SeekFrom::Start(current_pos));
                match result {
                    Ok(n) => { call.ret_i64(0, n as i64); write_nil_error(call, 1); }
                    Err(e) => { call.ret_i64(0, 0); write_io_error(call, 1, e); }
                }
            }
        }
    });
    ExternResult::Ok
}

#[vo_extern_ctx("os", "fileSeek")]
fn os_file_seek(call: &mut ExternCallContext) -> ExternResult {
    let fd = call.arg_i64(0) as i32;
    let offset = call.arg_i64(1);
    let whence = call.arg_i64(2) as i32;
    
    with_file_mut!(fd, call, 1, ret0, |file| {
        let seek_from = match whence {
            0 => Some(SeekFrom::Start(offset as u64)),
            1 => Some(SeekFrom::Current(offset)),
            2 => Some(SeekFrom::End(offset)),
            _ => { call.ret_i64(0, 0); write_error_to(call, 1, CODE_OS_INVALID, "invalid whence"); None }
        };
        if let Some(sf) = seek_from {
            match file.seek(sf) {
                Ok(pos) => { call.ret_i64(0, pos as i64); write_nil_error(call, 1); }
                Err(e) => { call.ret_i64(0, 0); write_io_error(call, 1, e); }
            }
        }
    });
    ExternResult::Ok
}

#[vo_extern_ctx("os", "fileClose")]
fn os_file_close(call: &mut ExternCallContext) -> ExternResult {
    let fd = call.arg_i64(0) as i32;
    if fd <= 2 { write_nil_error(call, 0); return ExternResult::Ok; }
    if remove_file(fd).is_some() { write_nil_error(call, 0); }
    else { write_error_to(call, 0, CODE_OS_INVALID, "invalid file descriptor"); }
    ExternResult::Ok
}

#[vo_extern_ctx("os", "fileSync")]
fn os_file_sync(call: &mut ExternCallContext) -> ExternResult {
    let fd = call.arg_i64(0) as i32;
    with_file!(fd, call, 0, |file| {
        match file.sync_all() { Ok(_) => write_nil_error(call, 0), Err(e) => write_io_error(call, 0, e) }
    });
    ExternResult::Ok
}

#[vo_extern_ctx("os", "fileStat")]
fn os_file_stat(call: &mut ExternCallContext) -> ExternResult {
    let fd = call.arg_i64(0) as i32;
    with_file!(fd, call, 5, nil 5, |file| {
        match file.metadata() {
            Ok(meta) => {
                let file_info = metadata_to_file_info(call, "", &meta);
                for i in 0..5 { call.ret_u64(i as u16, unsafe { Gc::read_slot(file_info, i) }); }
                write_nil_error(call, 5);
            }
            Err(e) => { for i in 0..5 { call.ret_nil(i as u16); } write_io_error(call, 5, e); }
        }
    });
    ExternResult::Ok
}

#[vo_extern_ctx("os", "fileTruncate")]
fn os_file_truncate(call: &mut ExternCallContext) -> ExternResult {
    let fd = call.arg_i64(0) as i32;
    let size = call.arg_i64(1) as u64;
    with_file!(fd, call, 0, |file| {
        match file.set_len(size) { Ok(_) => write_nil_error(call, 0), Err(e) => write_io_error(call, 0, e) }
    });
    ExternResult::Ok
}

// Import O_* flags from os.vo
vo_consts! {
    "os" => {
        O_RDONLY,
        O_WRONLY,
        O_RDWR,
        O_APPEND,
        O_CREATE,
        O_EXCL,
        O_SYNC,
        O_TRUNC,
    }
}

#[vo_extern_ctx("os", "openFile")]
fn os_open_file(call: &mut ExternCallContext) -> ExternResult {
    let name = call.arg_str(0);
    let flag = call.arg_i64(1) as i32;
    let perm = call.arg_u64(2) as u32;
    
    let mut opts = OpenOptions::new();
    let access = flag & 0x3;
    if access == O_RDONLY as i32 { opts.read(true); }
    else if access == O_WRONLY as i32 { opts.write(true); }
    else if access == O_RDWR as i32 { opts.read(true).write(true); }
    if flag & O_APPEND as i32 != 0 { opts.append(true); }
    if flag & O_CREATE as i32 != 0 { opts.create(true); }
    if flag & O_EXCL as i32 != 0 { opts.create_new(true); }
    if flag & O_TRUNC as i32 != 0 { opts.truncate(true); }
    
    match opts.open(name) {
        Ok(file) => {
            #[cfg(unix)]
            if flag & O_CREATE as i32 != 0 { let _ = file.set_permissions(fs::Permissions::from_mode(perm)); }
            let fd = register_file(file);
            call.ret_i64(0, fd as i64);
            write_nil_error(call, 1);
        }
        Err(e) => { call.ret_i64(0, -1); write_io_error(call, 1, e); }
    }
    ExternResult::Ok
}

#[vo_extern_ctx("os", "nativeMkdir")]
fn os_mkdir(call: &mut ExternCallContext) -> ExternResult {
    let path = call.arg_str(0);
    let perm = call.arg_u64(1) as u32;
    match fs::create_dir(path) {
        Ok(_) => { #[cfg(unix)] { let _ = fs::set_permissions(path, fs::Permissions::from_mode(perm)); } write_nil_error(call, 0); }
        Err(e) => write_io_error(call, 0, e),
    }
    ExternResult::Ok
}

#[vo_extern_ctx("os", "nativeMkdirAll")]
fn os_mkdir_all(call: &mut ExternCallContext) -> ExternResult {
    let path = call.arg_str(0);
    let perm = call.arg_u64(1) as u32;
    match fs::create_dir_all(path) {
        Ok(_) => { #[cfg(unix)] { let _ = fs::set_permissions(path, fs::Permissions::from_mode(perm)); } write_nil_error(call, 0); }
        Err(e) => write_io_error(call, 0, e),
    }
    ExternResult::Ok
}

#[vo_extern_ctx("os", "nativeRemove")]
fn os_remove(call: &mut ExternCallContext) -> ExternResult {
    let name = call.arg_str(0);
    match fs::remove_file(name) {
        Ok(_) => write_nil_error(call, 0),
        Err(_) => match fs::remove_dir(name) {
            Ok(_) => write_nil_error(call, 0),
            Err(e) => write_io_error(call, 0, e),
        }
    }
    ExternResult::Ok
}

#[vo_extern_ctx("os", "nativeRemoveAll")]
fn os_remove_all(call: &mut ExternCallContext) -> ExternResult {
    let path = call.arg_str(0);
    match fs::remove_dir_all(path) {
        Ok(_) => write_nil_error(call, 0),
        Err(e) => match fs::remove_file(path) { Ok(_) => write_nil_error(call, 0), Err(_) => write_io_error(call, 0, e) }
    }
    ExternResult::Ok
}

#[vo_extern_ctx("os", "nativeRename")]
fn os_rename(call: &mut ExternCallContext) -> ExternResult {
    let oldpath = call.arg_str(0);
    let newpath = call.arg_str(1);
    match fs::rename(oldpath, newpath) { Ok(_) => write_nil_error(call, 0), Err(e) => write_io_error(call, 0, e) }
    ExternResult::Ok
}

#[vo_extern_ctx("os", "nativeStat")]
fn os_stat(call: &mut ExternCallContext) -> ExternResult {
    let name = call.arg_str(0);
    match fs::metadata(name) {
        Ok(meta) => {
            let basename = std::path::Path::new(name).file_name().map(|s| s.to_string_lossy().to_string()).unwrap_or_default();
            let file_info = metadata_to_file_info(call, &basename, &meta);
            for i in 0..5 { let val = unsafe { Gc::read_slot(file_info, i) }; call.ret_u64(i as u16, val); }
            write_nil_error(call, 5);
        }
        Err(e) => { for i in 0..5 { call.ret_nil(i as u16); } write_io_error(call, 5, e); }
    }
    ExternResult::Ok
}

#[vo_extern_ctx("os", "nativeLstat")]
fn os_lstat(call: &mut ExternCallContext) -> ExternResult {
    let name = call.arg_str(0);
    match fs::symlink_metadata(name) {
        Ok(meta) => {
            let basename = std::path::Path::new(name).file_name().map(|s| s.to_string_lossy().to_string()).unwrap_or_default();
            let file_info = metadata_to_file_info(call, &basename, &meta);
            for i in 0..5 { let val = unsafe { Gc::read_slot(file_info, i) }; call.ret_u64(i as u16, val); }
            write_nil_error(call, 5);
        }
        Err(e) => { for i in 0..5 { call.ret_nil(i as u16); } write_io_error(call, 5, e); }
    }
    ExternResult::Ok
}

#[vo_extern_ctx("os", "nativeReadDir")]
fn os_read_dir(call: &mut ExternCallContext) -> ExternResult {
    let name = call.arg_str(0);
    match fs::read_dir(name) {
        Ok(entries) => {
            let mut dir_entries: Vec<(String, bool, u32)> = Vec::new();
            for entry in entries.flatten() {
                let entry_name = entry.file_name().to_string_lossy().to_string();
                let is_dir = entry.file_type().map(|t| t.is_dir()).unwrap_or(false);
                let mode = entry.metadata().map(|m| { let mut mode = m.permissions().mode(); if m.is_dir() { mode |= MODE_DIR; } mode }).unwrap_or(0);
                dir_entries.push((entry_name, is_dir, mode));
            }
            let len = dir_entries.len();
            let elem_slots = 3;
            let elem_meta = crate::ValueMeta::new(0, ValueKind::Struct);
            let result = slice::create(call.gc(), elem_meta, elem_slots * 8, len, len);
            for (i, (entry_name, is_dir, mode)) in dir_entries.iter().enumerate() {
                let name_ref = call.alloc_str(entry_name);
                let base = i * elem_slots;
                slice::set(result, base, name_ref as u64, 8);
                slice::set(result, base + 1, if *is_dir { 1 } else { 0 }, 8);
                slice::set(result, base + 2, *mode as u64, 8);
            }
            call.ret_ref(0, result);
            write_nil_error(call, 1);
        }
        Err(e) => { call.ret_nil(0); write_io_error(call, 1, e); }
    }
    ExternResult::Ok
}

#[vo_extern_ctx("os", "nativeChmod")]
fn os_chmod(call: &mut ExternCallContext) -> ExternResult {
    let name = call.arg_str(0);
    let mode = call.arg_u64(1) as u32;
    #[cfg(unix)] { match fs::set_permissions(name, fs::Permissions::from_mode(mode)) { Ok(_) => write_nil_error(call, 0), Err(e) => write_io_error(call, 0, e) } }
    #[cfg(not(unix))] { write_error_to(call, 0, CODE_OS_INVALID, "chmod not supported"); }
    ExternResult::Ok
}

#[vo_extern_ctx("os", "nativeChown")]
fn os_chown(call: &mut ExternCallContext) -> ExternResult {
    let name = call.arg_str(0);
    let uid = call.arg_i64(1) as u32;
    let gid = call.arg_i64(2) as u32;
    #[cfg(unix)] { use std::os::unix::fs::chown; match chown(name, Some(uid), Some(gid)) { Ok(_) => write_nil_error(call, 0), Err(e) => write_io_error(call, 0, e) } }
    #[cfg(not(unix))] { write_error_to(call, 0, CODE_OS_INVALID, "chown not supported"); }
    ExternResult::Ok
}

#[vo_extern_ctx("os", "nativeSymlink")]
fn os_symlink(call: &mut ExternCallContext) -> ExternResult {
    let oldname = call.arg_str(0);
    let newname = call.arg_str(1);
    #[cfg(unix)] { match symlink(oldname, newname) { Ok(_) => write_nil_error(call, 0), Err(e) => write_io_error(call, 0, e) } }
    #[cfg(not(unix))] { write_error_to(call, 0, CODE_OS_INVALID, "symlink not supported"); }
    ExternResult::Ok
}

#[vo_extern_ctx("os", "nativeReadlink")]
fn os_readlink(call: &mut ExternCallContext) -> ExternResult {
    let name = call.arg_str(0);
    match fs::read_link(name) {
        Ok(path) => { call.ret_str(0, &path.to_string_lossy()); write_nil_error(call, 1); }
        Err(e) => { call.ret_str(0, ""); write_io_error(call, 1, e); }
    }
    ExternResult::Ok
}

#[vo_extern_ctx("os", "nativeLink")]
fn os_link(call: &mut ExternCallContext) -> ExternResult {
    let oldname = call.arg_str(0);
    let newname = call.arg_str(1);
    match fs::hard_link(oldname, newname) { Ok(_) => write_nil_error(call, 0), Err(e) => write_io_error(call, 0, e) }
    ExternResult::Ok
}

#[vo_extern_ctx("os", "nativeTruncate")]
fn os_truncate(call: &mut ExternCallContext) -> ExternResult {
    let name = call.arg_str(0);
    let size = call.arg_i64(1) as u64;
    match File::options().write(true).open(name) {
        Ok(file) => match file.set_len(size) { Ok(_) => write_nil_error(call, 0), Err(e) => write_io_error(call, 0, e) }
        Err(e) => write_io_error(call, 0, e),
    }
    ExternResult::Ok
}

#[vo_extern_ctx("os", "nativeReadFile")]
fn os_read_file_native(call: &mut ExternCallContext) -> ExternResult {
    let name = call.arg_str(0);
    match fs::read(name) {
        Ok(data) => { call.ret_bytes(0, &data); write_nil_error(call, 1); }
        Err(e) => { call.ret_nil(0); write_io_error(call, 1, e); }
    }
    ExternResult::Ok
}

#[vo_extern_ctx("os", "nativeWriteFile")]
fn os_write_file_native(call: &mut ExternCallContext) -> ExternResult {
    let name = call.arg_str(0);
    let data = call.arg_bytes(1);
    let perm = call.arg_u64(2) as u32;
    match fs::write(name, data) {
        Ok(_) => { #[cfg(unix)] { let _ = fs::set_permissions(name, fs::Permissions::from_mode(perm)); } write_nil_error(call, 0); }
        Err(e) => write_io_error(call, 0, e),
    }
    ExternResult::Ok
}

#[vo_extern_ctx("os", "nativeGetenv")]
fn os_getenv(call: &mut ExternCallContext) -> ExternResult {
    let key = call.arg_str(0);
    call.ret_str(0, &std::env::var(key).unwrap_or_default());
    ExternResult::Ok
}

#[vo_extern_ctx("os", "nativeSetenv")]
fn os_setenv(call: &mut ExternCallContext) -> ExternResult {
    std::env::set_var(call.arg_str(0), call.arg_str(1));
    write_nil_error(call, 0);
    ExternResult::Ok
}

#[vo_extern_ctx("os", "nativeUnsetenv")]
fn os_unsetenv(call: &mut ExternCallContext) -> ExternResult {
    std::env::remove_var(call.arg_str(0));
    write_nil_error(call, 0);
    ExternResult::Ok
}

#[vo_extern_ctx("os", "nativeEnviron")]
fn os_environ(call: &mut ExternCallContext) -> ExternResult {
    let vars: Vec<String> = std::env::vars().map(|(k, v)| format!("{}={}", k, v)).collect();
    call.ret_string_slice(0, &vars);
    ExternResult::Ok
}

#[vo_extern_ctx("os", "nativeLookupEnv")]
fn os_lookup_env(call: &mut ExternCallContext) -> ExternResult {
    let key = call.arg_str(0);
    match std::env::var(key) {
        Ok(value) => { call.ret_str(0, &value); call.ret_bool(1, true); }
        Err(_) => { call.ret_str(0, ""); call.ret_bool(1, false); }
    }
    ExternResult::Ok
}

#[vo_extern_ctx("os", "nativeClearenv")]
fn os_clearenv(_call: &mut ExternCallContext) -> ExternResult {
    for (key, _) in std::env::vars() { std::env::remove_var(key); }
    ExternResult::Ok
}

#[vo_extern_ctx("os", "nativeExpandEnv")]
fn os_expand_env(call: &mut ExternCallContext) -> ExternResult {
    let s = call.arg_str(0);
    let mut result = s.to_string();
    for (key, value) in std::env::vars() {
        result = result.replace(&format!("${{{}}}", key), &value);
        result = result.replace(&format!("${}", key), &value);
    }
    call.ret_str(0, &result);
    ExternResult::Ok
}

#[vo_extern_ctx("os", "nativeGetwd")]
fn os_getwd(call: &mut ExternCallContext) -> ExternResult {
    match std::env::current_dir() {
        Ok(path) => { call.ret_str(0, &path.to_string_lossy()); write_nil_error(call, 1); }
        Err(e) => { call.ret_str(0, ""); write_io_error(call, 1, e); }
    }
    ExternResult::Ok
}

#[vo_extern_ctx("os", "nativeChdir")]
fn os_chdir(call: &mut ExternCallContext) -> ExternResult {
    match std::env::set_current_dir(call.arg_str(0)) { Ok(_) => write_nil_error(call, 0), Err(e) => write_io_error(call, 0, e) }
    ExternResult::Ok
}

#[vo_extern_ctx("os", "nativeUserHomeDir")]
fn os_user_home_dir(call: &mut ExternCallContext) -> ExternResult {
    match dirs::home_dir() {
        Some(path) => { call.ret_str(0, &path.to_string_lossy()); write_nil_error(call, 1); }
        None => { call.ret_str(0, ""); write_error_to(call, 1, CODE_OS_NOT_EXIST, "home directory not found"); }
    }
    ExternResult::Ok
}

#[vo_extern_ctx("os", "nativeUserCacheDir")]
fn os_user_cache_dir(call: &mut ExternCallContext) -> ExternResult {
    match dirs::cache_dir() {
        Some(path) => { call.ret_str(0, &path.to_string_lossy()); write_nil_error(call, 1); }
        None => { call.ret_str(0, ""); write_error_to(call, 1, CODE_OS_NOT_EXIST, "cache directory not found"); }
    }
    ExternResult::Ok
}

#[vo_extern_ctx("os", "nativeUserConfigDir")]
fn os_user_config_dir(call: &mut ExternCallContext) -> ExternResult {
    match dirs::config_dir() {
        Some(path) => { call.ret_str(0, &path.to_string_lossy()); write_nil_error(call, 1); }
        None => { call.ret_str(0, ""); write_error_to(call, 1, CODE_OS_NOT_EXIST, "config directory not found"); }
    }
    ExternResult::Ok
}

#[vo_extern_ctx("os", "nativeTempDir")]
fn os_temp_dir(call: &mut ExternCallContext) -> ExternResult {
    call.ret_str(0, &std::env::temp_dir().to_string_lossy());
    ExternResult::Ok
}

#[vo_extern_ctx("os", "nativeGetpid")]
fn os_getpid(call: &mut ExternCallContext) -> ExternResult { call.ret_i64(0, std::process::id() as i64); ExternResult::Ok }
#[vo_extern_ctx("os", "nativeGetppid")]
fn os_getppid(call: &mut ExternCallContext) -> ExternResult { #[cfg(unix)] { call.ret_i64(0, unsafe { libc::getppid() } as i64); } #[cfg(not(unix))] { call.ret_i64(0, 0); } ExternResult::Ok }
#[vo_extern_ctx("os", "nativeGetuid")]
fn os_getuid(call: &mut ExternCallContext) -> ExternResult { #[cfg(unix)] { call.ret_i64(0, unsafe { libc::getuid() } as i64); } #[cfg(not(unix))] { call.ret_i64(0, 0); } ExternResult::Ok }
#[vo_extern_ctx("os", "nativeGeteuid")]
fn os_geteuid(call: &mut ExternCallContext) -> ExternResult { #[cfg(unix)] { call.ret_i64(0, unsafe { libc::geteuid() } as i64); } #[cfg(not(unix))] { call.ret_i64(0, 0); } ExternResult::Ok }
#[vo_extern_ctx("os", "nativeGetgid")]
fn os_getgid(call: &mut ExternCallContext) -> ExternResult { #[cfg(unix)] { call.ret_i64(0, unsafe { libc::getgid() } as i64); } #[cfg(not(unix))] { call.ret_i64(0, 0); } ExternResult::Ok }
#[vo_extern_ctx("os", "nativeGetegid")]
fn os_getegid(call: &mut ExternCallContext) -> ExternResult { #[cfg(unix)] { call.ret_i64(0, unsafe { libc::getegid() } as i64); } #[cfg(not(unix))] { call.ret_i64(0, 0); } ExternResult::Ok }
#[vo_extern_ctx("os", "nativeExit")]
fn os_exit(call: &mut ExternCallContext) -> ExternResult { std::process::exit(call.arg_i64(0) as i32); }
#[vo_extern_ctx("os", "nativeGetArgs")]
fn os_get_args(call: &mut ExternCallContext) -> ExternResult { call.ret_string_slice(0, &std::env::args().collect::<Vec<_>>()); ExternResult::Ok }

#[vo_extern_ctx("os", "nativeHostname")]
fn os_hostname(call: &mut ExternCallContext) -> ExternResult {
    match hostname::get() {
        Ok(name) => { call.ret_str(0, &name.to_string_lossy()); write_nil_error(call, 1); }
        Err(e) => { call.ret_str(0, ""); write_io_error(call, 1, e); }
    }
    ExternResult::Ok
}

#[vo_extern_ctx("os", "nativeExecutable")]
fn os_executable(call: &mut ExternCallContext) -> ExternResult {
    match std::env::current_exe() {
        Ok(path) => { call.ret_str(0, &path.to_string_lossy()); write_nil_error(call, 1); }
        Err(e) => { call.ret_str(0, ""); write_io_error(call, 1, e); }
    }
    ExternResult::Ok
}

#[vo_extern_ctx("os", "nativeCreateTemp")]
fn os_create_temp(call: &mut ExternCallContext) -> ExternResult {
    let dir = call.arg_str(0);
    let pattern = call.arg_str(1);
    let dir = if dir.is_empty() { std::env::temp_dir() } else { std::path::PathBuf::from(dir) };
    let (prefix, suffix) = if let Some(pos) = pattern.find('*') { (&pattern[..pos], &pattern[pos + 1..]) } else { (pattern, "") };
    for _ in 0..10000 {
        let name = format!("{}{:016x}{}", prefix, temp_random(), suffix);
        let path = dir.join(&name);
        match OpenOptions::new().write(true).create_new(true).open(&path) {
            Ok(file) => { let fd = register_file(file); call.ret_i64(0, fd as i64); call.ret_str(1, &path.to_string_lossy()); write_nil_error(call, 2); return ExternResult::Ok; }
            Err(ref e) if e.kind() == std::io::ErrorKind::AlreadyExists => continue,
            Err(e) => { call.ret_i64(0, -1); call.ret_str(1, ""); write_io_error(call, 2, e); return ExternResult::Ok; }
        }
    }
    call.ret_i64(0, -1); call.ret_str(1, ""); write_error_to(call, 2, CODE_OS_EXIST, "failed to create temp file");
    ExternResult::Ok
}

#[vo_extern_ctx("os", "nativeMkdirTemp")]
fn os_mkdir_temp(call: &mut ExternCallContext) -> ExternResult {
    let dir = call.arg_str(0);
    let pattern = call.arg_str(1);
    let dir = if dir.is_empty() { std::env::temp_dir() } else { std::path::PathBuf::from(dir) };
    let (prefix, suffix) = if let Some(pos) = pattern.find('*') { (&pattern[..pos], &pattern[pos + 1..]) } else { (pattern, "") };
    for _ in 0..10000 {
        let name = format!("{}{:016x}{}", prefix, temp_random(), suffix);
        let path = dir.join(&name);
        match fs::create_dir(&path) {
            Ok(_) => { call.ret_str(0, &path.to_string_lossy()); write_nil_error(call, 1); return ExternResult::Ok; }
            Err(ref e) if e.kind() == std::io::ErrorKind::AlreadyExists => continue,
            Err(e) => { call.ret_str(0, ""); write_io_error(call, 1, e); return ExternResult::Ok; }
        }
    }
    call.ret_str(0, ""); write_error_to(call, 1, CODE_OS_EXIST, "failed to create temp dir");
    ExternResult::Ok
}

