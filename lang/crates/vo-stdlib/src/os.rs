//! os package native function implementations.
//!
//! This module requires std for file system operations.
//! In no_std mode, all functions panic with "requires std".

#[cfg(feature = "std")]
use std::collections::HashMap;
#[cfg(feature = "std")]
use std::fs::{self, DirBuilder, File, OpenOptions};
#[cfg(feature = "std")]
use std::io::{IsTerminal, Read, Seek, SeekFrom, Write};
#[cfg(feature = "std")]
use std::ops::{Deref, DerefMut};
#[cfg(all(feature = "std", unix))]
use std::os::unix::fs::{symlink, DirBuilderExt, FileTypeExt, OpenOptionsExt, PermissionsExt};
#[cfg(all(feature = "std", unix))]
use std::os::unix::io::{AsFd, AsRawFd, BorrowedFd, OwnedFd};
#[cfg(all(feature = "std", windows))]
use std::os::windows::fs::{FileExt, OpenOptionsExt};
#[cfg(all(feature = "std", windows))]
use std::os::windows::io::AsHandle;
#[cfg(feature = "std")]
use std::sync::{Mutex, MutexGuard};

#[cfg(feature = "std")]
use vo_common_core::types::ValueKind;
use vo_ffi_macro::{vostd_consts, vostd_errors, vostd_fn};
#[cfg(feature = "std")]
use vo_runtime::builtins::error_helper::{write_error_to, write_nil_error};
#[cfg(feature = "std")]
use vo_runtime::ffi::{ExternCallContext, ExternResult};
#[cfg(feature = "std")]
use vo_runtime::gc::{Gc, GcRef};
#[cfg(feature = "std")]
use vo_runtime::io::{CompletionData, IoResourceToken, IoRuntime};
#[cfg(all(feature = "std", unix))]
use vo_runtime::io::{IoCancelKey, IoCancellation, IoLease};
#[cfg(feature = "std")]
use vo_runtime::objects::slice;
#[cfg(feature = "std")]
use vo_runtime::slot::SLOT_BYTES;

#[cfg(feature = "std")]
const MODE_DIR: u32 = 1 << 31;
#[cfg(feature = "std")]
const MODE_SYMLINK: u32 = 1 << 27;
#[cfg(feature = "std")]
const MODE_DEVICE: u32 = 1 << 26;
#[cfg(feature = "std")]
const MODE_NAMED_PIPE: u32 = 1 << 25;
#[cfg(feature = "std")]
const MODE_SOCKET: u32 = 1 << 24;
#[cfg(feature = "std")]
const MODE_SETUID: u32 = 1 << 23;
#[cfg(feature = "std")]
const MODE_SETGID: u32 = 1 << 22;
#[cfg(feature = "std")]
const MODE_CHAR_DEVICE: u32 = 1 << 21;
#[cfg(feature = "std")]
const MODE_STICKY: u32 = 1 << 20;
#[cfg(feature = "std")]
const MODE_IRREGULAR: u32 = 1 << 19;
#[cfg(feature = "std")]
const MODE_PERM: u32 = 0o777;
#[cfg(all(feature = "std", unix))]
const UNIX_MODE_SETUID: u32 = 0o4000;
#[cfg(all(feature = "std", unix))]
const UNIX_MODE_SETGID: u32 = 0o2000;
#[cfg(all(feature = "std", unix))]
const UNIX_MODE_STICKY: u32 = 0o1000;

#[cfg(feature = "std")]
lazy_static::lazy_static! {
    static ref FILE_HANDLES: Mutex<HashMap<i32, NativeFile>> = Mutex::new(HashMap::new());
    static ref NEXT_FD: Mutex<i32> = Mutex::new(100);
}

#[cfg(feature = "std")]
fn lock_recover<T>(mutex: &Mutex<T>) -> MutexGuard<'_, T> {
    mutex
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
}

#[cfg(feature = "std")]
struct NativeFile {
    file: File,
    append: bool,
    cleanup_token: IoResourceToken,
    #[cfg(unix)]
    cancellation: IoCancellation,
}

#[cfg(all(feature = "std", unix))]
impl NativeFile {
    /// Must be called while `FILE_HANDLES` is locked.
    fn lease(&self, _handle: i32) -> std::io::Result<IoLease> {
        IoLease::try_clone(&self.file, self.cancellation.clone())
    }

    fn cancel(&self, _handle: i32) -> IoCancelKey {
        self.cancellation.cancel();
        self.cancellation.cancel_key()
    }
}

#[cfg(feature = "std")]
impl Deref for NativeFile {
    type Target = File;

    fn deref(&self) -> &Self::Target {
        &self.file
    }
}

#[cfg(feature = "std")]
impl DerefMut for NativeFile {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.file
    }
}

#[cfg(feature = "std")]
fn next_file_handle(current: i32) -> std::io::Result<i32> {
    current
        .checked_add(1)
        .ok_or_else(|| std::io::Error::other("file handle space is exhausted"))
}

#[cfg(feature = "std")]
fn register_file(io: &mut IoRuntime, file: File, append: bool) -> std::io::Result<i32> {
    #[cfg(unix)]
    {
        let raw_fd = file.as_raw_fd();

        let fd_flags = unsafe { libc::fcntl(raw_fd, libc::F_GETFD) };
        if fd_flags == -1 {
            return Err(std::io::Error::last_os_error());
        }
        let ret = unsafe { libc::fcntl(raw_fd, libc::F_SETFD, fd_flags | libc::FD_CLOEXEC) };
        if ret == -1 {
            return Err(std::io::Error::last_os_error());
        }

        let flags = unsafe { libc::fcntl(raw_fd, libc::F_GETFL) };
        if flags == -1 {
            return Err(std::io::Error::last_os_error());
        }
        let ret = unsafe { libc::fcntl(raw_fd, libc::F_SETFL, flags | libc::O_NONBLOCK) };
        if ret == -1 {
            return Err(std::io::Error::last_os_error());
        }
    }

    let mut handles = lock_recover(&FILE_HANDLES);
    let mut next_fd = lock_recover(&NEXT_FD);
    let fd = *next_fd;
    *next_fd = next_file_handle(fd)?;
    if handles.contains_key(&fd) {
        return Err(std::io::Error::other(
            "file handle allocation would overwrite a live handle",
        ));
    }
    let cleanup_token =
        io.register_resource_cleanup(move |token| move || cleanup_file_handle(fd, token))?;
    #[cfg(unix)]
    let cancellation = match IoCancellation::new() {
        Ok(cancellation) => cancellation,
        Err(error) => {
            io.disarm_resource_cleanup(cleanup_token);
            return Err(error);
        }
    };
    handles.insert(
        fd,
        NativeFile {
            file,
            append,
            cleanup_token,
            #[cfg(unix)]
            cancellation,
        },
    );
    Ok(fd)
}

#[cfg(feature = "std")]
fn remove_file(fd: i32) -> Option<NativeFile> {
    lock_recover(&FILE_HANDLES).remove(&fd)
}

#[cfg(feature = "std")]
fn cleanup_file_handle(fd: i32, cleanup_token: IoResourceToken) {
    let removed = {
        let mut handles = FILE_HANDLES
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if handles
            .get(&fd)
            .is_some_and(|file| file.cleanup_token == cleanup_token)
        {
            handles.remove(&fd)
        } else {
            None
        }
    };
    if let Some(file) = removed {
        #[cfg(unix)]
        file.cancellation.cancel();
        drop(file);
    }
}

#[cfg(feature = "std")]
fn discard_file(io: &mut IoRuntime, fd: i32) {
    if let Some(file) = remove_file(fd) {
        io.disarm_resource_cleanup(file.cleanup_token);
        #[cfg(unix)]
        file.cancellation.cancel();
    }
}

#[cfg(all(feature = "std", unix))]
pub(crate) fn dup_fd_from_handle(fd: i32) -> std::io::Result<OwnedFd> {
    if fd == 0 || fd == 1 || fd == 2 {
        // Safety: the process standard descriptors are borrowed only for the
        // duration of the atomic dup operation below.
        return unsafe { BorrowedFd::borrow_raw(fd) }.try_clone_to_owned();
    }
    let handles = lock_recover(&FILE_HANDLES);
    handles
        .get(&fd)
        .ok_or_else(|| {
            std::io::Error::new(std::io::ErrorKind::NotFound, "invalid file descriptor")
        })?
        .as_fd()
        .try_clone_to_owned()
}

#[cfg(feature = "std")]
pub(crate) fn clone_file_from_handle(fd: i32) -> std::io::Result<File> {
    #[cfg(unix)]
    {
        dup_fd_from_handle(fd).map(File::from)
    }

    #[cfg(windows)]
    {
        let standard = match fd {
            0 => Some(std::io::stdin().as_handle().try_clone_to_owned()),
            1 => Some(std::io::stdout().as_handle().try_clone_to_owned()),
            2 => Some(std::io::stderr().as_handle().try_clone_to_owned()),
            _ => None,
        };
        if let Some(handle) = standard {
            return handle.map(File::from);
        }
        let handles = lock_recover(&FILE_HANDLES);
        handles
            .get(&fd)
            .ok_or_else(|| {
                std::io::Error::new(std::io::ErrorKind::NotFound, "invalid file descriptor")
            })?
            .file
            .try_clone()
    }

    #[cfg(not(any(unix, windows)))]
    {
        let _ = fd;
        Err(std::io::Error::new(
            std::io::ErrorKind::Unsupported,
            "process stdio handles are unavailable on this platform",
        ))
    }
}

#[cfg(feature = "std")]
fn file_type_mode(file_type: &fs::FileType) -> u32 {
    if file_type.is_dir() {
        return MODE_DIR;
    }
    if file_type.is_symlink() {
        return MODE_SYMLINK;
    }

    #[cfg(unix)]
    {
        if file_type.is_block_device() {
            return MODE_DEVICE;
        }
        if file_type.is_char_device() {
            return MODE_DEVICE | MODE_CHAR_DEVICE;
        }
        if file_type.is_fifo() {
            return MODE_NAMED_PIPE;
        }
        if file_type.is_socket() {
            return MODE_SOCKET;
        }
    }

    if file_type.is_file() {
        0
    } else {
        MODE_IRREGULAR
    }
}

#[cfg(feature = "std")]
fn metadata_to_file_mode_with_type(meta: &fs::Metadata, file_type: &fs::FileType) -> u32 {
    #[cfg(unix)]
    let mut mode = {
        let raw = meta.permissions().mode();
        (raw & MODE_PERM) | unix_special_mode_bits(raw)
    };

    #[cfg(not(unix))]
    let mut mode = {
        if meta.permissions().readonly() {
            0o444
        } else {
            0o666
        }
    };

    mode |= file_type_mode(file_type);
    mode
}

#[cfg(all(feature = "std", unix))]
fn unix_special_mode_bits(raw: u32) -> u32 {
    let mut mode = 0;
    if raw & UNIX_MODE_SETUID != 0 {
        mode |= MODE_SETUID;
    }
    if raw & UNIX_MODE_SETGID != 0 {
        mode |= MODE_SETGID;
    }
    if raw & UNIX_MODE_STICKY != 0 {
        mode |= MODE_STICKY;
    }
    mode
}

#[cfg(feature = "std")]
fn metadata_to_file_mode(meta: &fs::Metadata) -> u32 {
    metadata_to_file_mode_with_type(meta, &meta.file_type())
}

#[cfg(feature = "std")]
fn invalid_input(message: impl Into<String>) -> std::io::Error {
    std::io::Error::new(std::io::ErrorKind::InvalidInput, message.into())
}

#[cfg(feature = "std")]
fn system_time_to_unix_seconds(time: std::time::SystemTime) -> i64 {
    match time.duration_since(std::time::UNIX_EPOCH) {
        Ok(duration) => i64::try_from(duration.as_secs()).unwrap_or(i64::MAX),
        Err(error) => {
            let duration = error.duration();
            let seconds = i128::from(duration.as_secs());
            let floored = if duration.subsec_nanos() == 0 {
                -seconds
            } else {
                -seconds - 1
            };
            i64::try_from(floored).unwrap_or(i64::MIN)
        }
    }
}

#[cfg(feature = "std")]
fn system_time_from_unix_seconds(seconds: i64) -> std::io::Result<std::time::SystemTime> {
    let magnitude = std::time::Duration::from_secs(seconds.unsigned_abs());
    let time = if seconds >= 0 {
        std::time::UNIX_EPOCH.checked_add(magnitude)
    } else {
        std::time::UNIX_EPOCH.checked_sub(magnitude)
    };
    time.ok_or_else(|| invalid_input("Unix timestamp is outside the platform SystemTime range"))
}

#[cfg(feature = "std")]
fn open_file_for_times(path: &std::path::Path) -> std::io::Result<File> {
    let mut options = OpenOptions::new();
    #[cfg(windows)]
    {
        const FILE_WRITE_ATTRIBUTES: u32 = 0x0000_0100;
        options.access_mode(FILE_WRITE_ATTRIBUTES);
    }
    #[cfg(not(windows))]
    options.read(true);
    options.open(path)
}

#[cfg(feature = "std")]
fn file_modified_time_secs(meta: &fs::Metadata) -> i64 {
    // FileInfo has no metadata-error result field. Keep the historical epoch
    // fallback only for a host that cannot report mtime; represent every
    // successfully reported pre-epoch timestamp as a signed Unix second.
    meta.modified()
        .map(system_time_to_unix_seconds)
        .unwrap_or(0)
}

#[cfg(all(feature = "std", unix))]
fn unix_file_mode(mode: u32) -> u32 {
    let mut raw = mode & MODE_PERM;
    if mode & MODE_SETUID != 0 {
        raw |= UNIX_MODE_SETUID;
    }
    if mode & MODE_SETGID != 0 {
        raw |= UNIX_MODE_SETGID;
    }
    if mode & MODE_STICKY != 0 {
        raw |= UNIX_MODE_STICKY;
    }
    raw
}

#[cfg(feature = "std")]
fn set_path_permissions(path: &std::path::Path, perm: u32) -> std::io::Result<()> {
    #[cfg(unix)]
    {
        fs::set_permissions(path, fs::Permissions::from_mode(unix_file_mode(perm)))
    }

    #[cfg(not(unix))]
    {
        let mut permissions = fs::metadata(path)?.permissions();
        permissions.set_readonly(perm & 0o222 == 0);
        fs::set_permissions(path, permissions)
    }
}

#[cfg(feature = "std")]
fn validate_open_flags(flag: i32) -> std::io::Result<()> {
    let known = O_WRONLY as i32
        | O_RDWR as i32
        | O_APPEND as i32
        | O_CREATE as i32
        | O_EXCL as i32
        | O_SYNC as i32
        | O_TRUNC as i32;
    if flag & !known != 0 {
        return Err(invalid_input(format!(
            "unsupported OpenFile flag bits: {:#x}",
            flag & !known
        )));
    }
    let access = flag & 0x3;
    if !matches!(access, 0..=2) {
        return Err(invalid_input("invalid OpenFile access mode"));
    }
    if flag & O_EXCL as i32 != 0 && flag & O_CREATE as i32 == 0 {
        return Err(invalid_input("O_EXCL requires O_CREATE"));
    }
    if flag & O_TRUNC as i32 != 0 && access == O_RDONLY as i32 {
        return Err(invalid_input("O_TRUNC requires a writable access mode"));
    }
    Ok(())
}

#[cfg(feature = "std")]
fn open_file_with_mode(path: &std::path::Path, flag: i32, perm: u32) -> std::io::Result<File> {
    validate_open_flags(flag)?;

    let mut options = OpenOptions::new();
    match flag & 0x3 {
        access if access == O_RDONLY as i32 => {
            options.read(true);
        }
        access if access == O_WRONLY as i32 => {
            options.write(true);
        }
        access if access == O_RDWR as i32 => {
            options.read(true).write(true);
        }
        _ => unreachable!("validated OpenFile access mode"),
    }

    #[cfg(unix)]
    {
        let mut native_flags = 0;
        if flag & O_APPEND as i32 != 0 {
            native_flags |= libc::O_APPEND;
        }
        if flag & O_CREATE as i32 != 0 {
            native_flags |= libc::O_CREAT;
        }
        if flag & O_EXCL as i32 != 0 {
            native_flags |= libc::O_EXCL;
        }
        if flag & O_SYNC as i32 != 0 {
            native_flags |= libc::O_SYNC;
        }
        if flag & O_TRUNC as i32 != 0 {
            native_flags |= libc::O_TRUNC;
        }
        options
            .custom_flags(native_flags)
            .mode(unix_file_mode(perm));
    }

    #[cfg(not(unix))]
    {
        let access = flag & 0x3;
        // OpenOptions::append(true) also grants write access. Preserve a
        // read-only descriptor when callers supply O_RDONLY|O_APPEND.
        if flag & O_APPEND as i32 != 0 && access != O_RDONLY as i32 {
            options.append(true);
        }
        if flag & O_EXCL as i32 != 0 {
            options.create_new(true);
        } else if flag & O_CREATE as i32 != 0 {
            options.create(true);
        }
        if flag & O_TRUNC as i32 != 0 {
            options.truncate(true);
        }
    }

    #[cfg(windows)]
    {
        const FILE_ATTRIBUTE_READONLY: u32 = 0x0000_0001;
        const FILE_FLAG_WRITE_THROUGH: u32 = 0x8000_0000;
        if flag & O_CREATE as i32 != 0 && perm & 0o222 == 0 {
            // CreateFile applies creation attributes only when it creates the
            // path, matching Go's Windows OpenFile behavior without changing
            // an existing file.
            options.attributes(FILE_ATTRIBUTE_READONLY);
        }
        if flag & O_SYNC as i32 != 0 {
            options.custom_flags(FILE_FLAG_WRITE_THROUGH);
        }
    }

    #[cfg(not(any(unix, windows)))]
    if flag & O_SYNC as i32 != 0 {
        return Err(std::io::Error::new(
            std::io::ErrorKind::Unsupported,
            "O_SYNC is unavailable on this platform",
        ));
    }

    options.open(path)
}

#[cfg(feature = "std")]
fn write_file_with_mode(path: &std::path::Path, data: &[u8], perm: u32) -> std::io::Result<()> {
    let mut file = open_file_with_mode(
        path,
        O_WRONLY as i32 | O_CREATE as i32 | O_TRUNC as i32,
        perm,
    )?;
    file.write_all(data)
}

#[cfg(feature = "std")]
fn create_dir_with_mode(path: &std::path::Path, perm: u32) -> std::io::Result<()> {
    let mut builder = DirBuilder::new();
    #[cfg(unix)]
    builder.mode(unix_file_mode(perm));
    #[cfg(not(unix))]
    let _ = perm; // Go also ignores Mkdir permission bits on Windows/WASI.
    builder.create(path)
}

#[cfg(feature = "std")]
fn create_dir_all_with_mode(path: &std::path::Path, perm: u32) -> std::io::Result<()> {
    let mut builder = DirBuilder::new();
    builder.recursive(true);
    #[cfg(unix)]
    builder.mode(unix_file_mode(perm));
    #[cfg(not(unix))]
    let _ = perm;
    builder.create(path)
}

#[cfg(feature = "std")]
fn remove_path(path: &std::path::Path) -> std::io::Result<()> {
    let metadata = fs::symlink_metadata(path)?;
    if metadata.file_type().is_dir() {
        fs::remove_dir(path)
    } else {
        // Symlinks are removed as links, including links whose targets are
        // directories.
        fs::remove_file(path)
    }
}

#[cfg(feature = "std")]
fn remove_all_path(path: &std::path::Path) -> std::io::Result<()> {
    let metadata = match fs::symlink_metadata(path) {
        Ok(metadata) => metadata,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(()),
        Err(error) => return Err(error),
    };
    let result = if metadata.file_type().is_dir() {
        fs::remove_dir_all(path)
    } else {
        fs::remove_file(path)
    };
    match result {
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(()),
        other => other,
    }
}

#[cfg(feature = "std")]
fn checked_file_size(size: i64) -> std::io::Result<u64> {
    u64::try_from(size).map_err(|_| invalid_input("file size must be non-negative"))
}

#[cfg(feature = "std")]
fn checked_i32(value: i64, description: &str) -> std::io::Result<i32> {
    i32::try_from(value).map_err(|_| invalid_input(format!("{description} is out of range")))
}

#[cfg(feature = "std")]
fn checked_chown_id(value: i64, description: &str) -> std::io::Result<u32> {
    if value == -1 {
        return Ok(u32::MAX);
    }
    u32::try_from(value).map_err(|_| invalid_input(format!("{description} is out of range")))
}

#[cfg(feature = "std")]
fn os_string_bytes(value: std::ffi::OsString, description: &str) -> std::io::Result<Vec<u8>> {
    crate::host_bytes::os_string_into_bytes(value, description)
}

#[cfg(feature = "std")]
fn path_bytes(path: std::path::PathBuf, description: &str) -> std::io::Result<Vec<u8>> {
    crate::host_bytes::path_buf_into_bytes(path, description)
}

#[cfg(feature = "std")]
fn path_arg(
    call: &ExternCallContext,
    slot: u16,
    description: &str,
) -> std::io::Result<std::path::PathBuf> {
    crate::host_bytes::path_buf_from_bytes(call.arg_string_bytes(slot), description)
}

#[cfg(feature = "std")]
fn file_info_name(path: &std::path::Path) -> std::io::Result<Vec<u8>> {
    let name = path
        .file_name()
        .or_else(|| path.components().next_back().map(|part| part.as_os_str()))
        .unwrap_or_default();
    os_string_bytes(name.to_os_string(), "file name")
}

#[cfg(feature = "std")]
fn temp_pattern_parts(pattern: &[u8]) -> std::io::Result<(&[u8], &[u8])> {
    if pattern.contains(&b'/') || pattern.contains(&b'\\') {
        return Err(invalid_input(
            "temporary-file pattern must not contain a path separator",
        ));
    }
    Ok(match pattern.iter().rposition(|byte| *byte == b'*') {
        Some(position) => (&pattern[..position], &pattern[position + 1..]),
        None => (pattern, b""),
    })
}

#[cfg(feature = "std")]
fn temp_parent(dir: Vec<u8>) -> std::io::Result<std::path::PathBuf> {
    if dir.is_empty() {
        Ok(std::env::temp_dir())
    } else {
        crate::host_bytes::path_buf_from_bytes(dir, "temporary-file directory")
    }
}

#[cfg(feature = "std")]
fn create_temp_file_in(
    dir: &std::path::Path,
    pattern: &[u8],
) -> std::io::Result<(File, std::path::PathBuf)> {
    let (prefix, suffix) = temp_pattern_parts(pattern)?;
    for _ in 0..10_000 {
        let mut name = Vec::with_capacity(prefix.len() + 16 + suffix.len());
        name.extend_from_slice(prefix);
        name.extend_from_slice(format!("{:016x}", crate::rand::next_u64()).as_bytes());
        name.extend_from_slice(suffix);
        let path = dir.join(crate::host_bytes::os_string_from_bytes(
            name,
            "temporary-file pattern",
        )?);
        match open_file_with_mode(
            &path,
            O_WRONLY as i32 | O_CREATE as i32 | O_EXCL as i32,
            0o600,
        ) {
            Ok(file) => return Ok((file, path)),
            Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => continue,
            Err(error) => return Err(error),
        }
    }
    Err(std::io::Error::new(
        std::io::ErrorKind::AlreadyExists,
        "failed to create a unique temporary file",
    ))
}

#[cfg(feature = "std")]
fn create_temp_dir_in(
    dir: &std::path::Path,
    pattern: &[u8],
) -> std::io::Result<std::path::PathBuf> {
    let (prefix, suffix) = temp_pattern_parts(pattern)?;
    for _ in 0..10_000 {
        let mut name = Vec::with_capacity(prefix.len() + 16 + suffix.len());
        name.extend_from_slice(prefix);
        name.extend_from_slice(format!("{:016x}", crate::rand::next_u64()).as_bytes());
        name.extend_from_slice(suffix);
        let path = dir.join(crate::host_bytes::os_string_from_bytes(
            name,
            "temporary-directory pattern",
        )?);
        match create_dir_with_mode(&path, 0o700) {
            Ok(()) => return Ok(path),
            Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => continue,
            Err(error) => return Err(error),
        }
    }
    Err(std::io::Error::new(
        std::io::ErrorKind::AlreadyExists,
        "failed to create a unique temporary directory",
    ))
}

#[cfg(feature = "std")]
fn validate_env_key(key: &[u8]) -> std::io::Result<()> {
    if key.is_empty() {
        return Err(invalid_input("environment variable name must not be empty"));
    }
    if key.contains(&b'=') {
        return Err(invalid_input(
            "environment variable name must not contain '='",
        ));
    }
    if key.contains(&0) {
        return Err(invalid_input(
            "environment variable name must not contain NUL",
        ));
    }
    Ok(())
}

#[cfg(feature = "std")]
fn validate_env_value(value: &[u8]) -> std::io::Result<()> {
    if value.contains(&0) {
        return Err(invalid_input(
            "environment variable value must not contain NUL",
        ));
    }
    Ok(())
}

#[cfg(feature = "std")]
fn set_env_checked(key: &[u8], value: &[u8]) -> std::io::Result<()> {
    validate_env_key(key)?;
    validate_env_value(value)?;
    let key = crate::host_bytes::os_string_from_bytes(key.to_vec(), "environment variable name")?;
    let value =
        crate::host_bytes::os_string_from_bytes(value.to_vec(), "environment variable value")?;
    std::env::set_var(key, value);
    Ok(())
}

#[cfg(feature = "std")]
fn unset_env_checked(key: &[u8]) -> std::io::Result<()> {
    validate_env_key(key)?;
    let key = crate::host_bytes::os_string_from_bytes(key.to_vec(), "environment variable name")?;
    std::env::remove_var(key);
    Ok(())
}

#[cfg(feature = "std")]
fn lookup_env_bytes(key: &[u8]) -> Option<Vec<u8>> {
    validate_env_key(key).ok()?;
    let key =
        crate::host_bytes::os_string_from_bytes(key.to_vec(), "environment variable name").ok()?;
    std::env::var_os(key)
        .and_then(|value| os_string_bytes(value, "environment variable value").ok())
}

#[cfg(feature = "std")]
fn environment_entry(
    key: std::ffi::OsString,
    value: std::ffi::OsString,
) -> std::io::Result<Vec<u8>> {
    let key = os_string_bytes(key, "environment variable name")?;
    let value = os_string_bytes(value, "environment variable value")?;
    let mut entry = Vec::with_capacity(key.len() + 1 + value.len());
    entry.extend_from_slice(&key);
    entry.push(b'=');
    entry.extend_from_slice(&value);
    Ok(entry)
}

#[cfg(feature = "std")]
fn is_shell_special_var(byte: u8) -> bool {
    matches!(
        byte,
        b'*' | b'#' | b'$' | b'@' | b'!' | b'?' | b'-' | b'0'..=b'9'
    )
}

#[cfg(feature = "std")]
fn is_shell_name_byte(byte: u8) -> bool {
    byte == b'_' || byte.is_ascii_alphanumeric()
}

#[cfg(feature = "std")]
fn shell_name(input: &[u8]) -> (&[u8], usize) {
    debug_assert!(!input.is_empty());
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
        return (&[], 1);
    }
    if is_shell_special_var(input[0]) {
        return (&input[..1], 1);
    }
    let width = input
        .iter()
        .position(|&byte| !is_shell_name_byte(byte))
        .unwrap_or(input.len());
    (&input[..width], width)
}

#[cfg(feature = "std")]
fn expand_env_bytes(input: &[u8]) -> Vec<u8> {
    let mut output = Vec::with_capacity(input.len());
    let mut literal_start = 0usize;
    let mut index = 0usize;
    while index < input.len() {
        if input[index] == b'$' && index + 1 < input.len() {
            output.extend_from_slice(&input[literal_start..index]);
            let (name, width) = shell_name(&input[index + 1..]);
            if name.is_empty() && width == 0 {
                output.push(b'$');
            } else if !name.is_empty() && validate_env_key(name).is_ok() {
                if let Some(value) = lookup_env_bytes(name) {
                    output.extend_from_slice(&value);
                }
            }
            index += width + 1;
            literal_start = index;
        } else {
            index += 1;
        }
    }
    output.extend_from_slice(&input[literal_start..]);
    output
}

#[cfg(feature = "std")]
macro_rules! with_file_mut {
    ($fd:expr, $call:expr, $err_slot:expr, |$file:ident| $body:expr) => {{
        let mut handles = lock_recover(&FILE_HANDLES);
        if let Some($file) = handles.get_mut(&$fd) {
            $body
        } else {
            write_error_to($call, $err_slot, "invalid file descriptor");
        }
    }};
    ($fd:expr, $call:expr, $err_slot:expr, ret0, |$file:ident| $body:expr) => {{
        let mut handles = lock_recover(&FILE_HANDLES);
        if let Some($file) = handles.get_mut(&$fd) {
            $body
        } else {
            $call.ret_i64(0, 0);
            write_error_to($call, $err_slot, "invalid file descriptor");
        }
    }};
}

#[cfg(feature = "std")]
macro_rules! with_file {
    ($fd:expr, $call:expr, $err_slot:expr, |$file:ident| $body:expr) => {{
        let handles = lock_recover(&FILE_HANDLES);
        if let Some($file) = handles.get(&$fd) {
            $body
        } else {
            write_error_to($call, $err_slot, "invalid file descriptor");
        }
    }};
    ($fd:expr, $call:expr, $err_slot:expr, nil $n:expr, |$file:ident| $body:expr) => {{
        let handles = lock_recover(&FILE_HANDLES);
        if let Some($file) = handles.get(&$fd) {
            $body
        } else {
            for i in 0..$n {
                $call.ret_nil(i as u16);
            }
            write_error_to($call, $err_slot, "invalid fd");
        }
    }};
}

// Os sentinel errors - generated by vostd_errors! macro
vostd_errors! {
    "os" => {
        NotExist => "file does not exist",
        Exist => "file already exists",
        Permission => "permission denied",
        Invalid => "invalid argument",
        Timeout => "operation timed out",
        Closed => "file already closed",
        NotDir => "not a directory",
        IsDir => "is a directory",
    }
}

#[cfg(feature = "std")]
pub(crate) fn write_io_error(call: &mut ExternCallContext, ret_slot: u16, err: std::io::Error) {
    let kind = match err.kind() {
        std::io::ErrorKind::NotFound => Some(OsErrorKind::NotExist),
        std::io::ErrorKind::PermissionDenied => Some(OsErrorKind::Permission),
        std::io::ErrorKind::AlreadyExists => Some(OsErrorKind::Exist),
        std::io::ErrorKind::InvalidInput => Some(OsErrorKind::Invalid),
        std::io::ErrorKind::TimedOut => Some(OsErrorKind::Timeout),
        std::io::ErrorKind::NotADirectory => Some(OsErrorKind::NotDir),
        std::io::ErrorKind::IsADirectory => Some(OsErrorKind::IsDir),
        _ => None,
    };

    if let Some(k) = kind {
        let pair = os_sentinel_error(call, k);
        call.ret_interface_pair(ret_slot, pair);
    } else {
        write_error_to(call, ret_slot, &err.to_string());
    }
}

#[cfg(feature = "std")]
const FILE_INFO_ABI_SLOTS: usize = 5;

#[cfg(feature = "std")]
struct FileInfoLayout {
    name: usize,
    size: usize,
    mode: usize,
    mod_time: usize,
    is_dir: usize,
}

#[cfg(feature = "std")]
fn file_info_layout(call: &ExternCallContext) -> Result<FileInfoLayout, String> {
    let value_meta = call
        .named_type_metas()
        .iter()
        .find(|meta| meta.name == "os.FileInfo")
        .map(|meta| meta.underlying_meta)
        .ok_or_else(|| "os.FileInfo named metadata is missing".to_string())?;
    if value_meta.value_kind() != ValueKind::Struct {
        return Err("os.FileInfo underlying metadata is not a struct".to_string());
    }
    let meta = call
        .struct_meta(value_meta.meta_id() as usize)
        .ok_or_else(|| "os.FileInfo struct metadata is missing".to_string())?;
    if usize::from(meta.slot_count()) != FILE_INFO_ABI_SLOTS {
        return Err(format!(
            "os.FileInfo extern ABI requires {FILE_INFO_ABI_SLOTS} slots, metadata has {}",
            meta.slot_count()
        ));
    }

    let field = |name: &str, kind: ValueKind| -> Result<usize, String> {
        let field = meta
            .get_field(name)
            .ok_or_else(|| format!("os.FileInfo metadata is missing {name}"))?;
        if field.slot_count != 1 || field.type_info.value_kind() != kind {
            return Err(format!(
                "os.FileInfo.{name} must have kind {kind:?} and one ABI slot"
            ));
        }
        let offset = usize::from(field.offset);
        if offset >= FILE_INFO_ABI_SLOTS {
            return Err(format!("os.FileInfo.{name} offset is outside its ABI"));
        }
        Ok(offset)
    };

    let layout = FileInfoLayout {
        name: field("name", ValueKind::String)?,
        size: field("size", ValueKind::Int64)?,
        mode: field("mode", ValueKind::Uint32)?,
        mod_time: field("modTime", ValueKind::Int64)?,
        is_dir: field("isDir", ValueKind::Bool)?,
    };
    let mut offsets = [
        layout.name,
        layout.size,
        layout.mode,
        layout.mod_time,
        layout.is_dir,
    ];
    offsets.sort_unstable();
    if offsets != [0, 1, 2, 3, 4] {
        return Err("os.FileInfo fields do not cover its five-slot ABI exactly once".to_string());
    }
    Ok(layout)
}

#[cfg(feature = "std")]
fn metadata_to_file_info(
    call: &mut ExternCallContext,
    name: &[u8],
    meta: &fs::Metadata,
) -> Result<GcRef, String> {
    let layout = file_info_layout(call)?;
    let file_info = call.gc_alloc_struct("os.FileInfo");
    let name_ref = call.alloc_string_bytes(name);
    let size = i64::try_from(meta.len())
        .map_err(|_| "file size exceeds os.FileInfo int64 representation".to_string())?;
    let mode = metadata_to_file_mode(meta);
    let mod_time = file_modified_time_secs(meta);
    let is_dir = if meta.is_dir() { 1u64 } else { 0u64 };

    unsafe {
        Gc::write_slot(file_info, layout.name, name_ref as u64);
        Gc::write_slot(file_info, layout.size, size as u64);
        Gc::write_slot(file_info, layout.mode, mode as u64);
        Gc::write_slot(file_info, layout.mod_time, mod_time as u64);
        Gc::write_slot(file_info, layout.is_dir, is_dir);
    }
    call.gc().mark_allocated_for_scan(file_info);
    Ok(file_info)
}

#[cfg(feature = "std")]
fn return_file_info(call: &mut ExternCallContext, file_info: GcRef) {
    for index in 0..FILE_INFO_ABI_SLOTS {
        let value = unsafe { Gc::read_slot(file_info, index) };
        call.ret_u64(index as u16, value);
    }
    write_nil_error(call, FILE_INFO_ABI_SLOTS as u16);
}

#[cfg(feature = "std")]
fn return_file_info_error(call: &mut ExternCallContext, error: std::io::Error) {
    for index in 0..FILE_INFO_ABI_SLOTS {
        call.ret_nil(index as u16);
    }
    write_io_error(call, FILE_INFO_ABI_SLOTS as u16, error);
}

#[cfg(feature = "std")]
#[vostd_fn("os", "getPathSeparators", std)]
fn os_get_path_separators(call: &mut ExternCallContext) -> ExternResult {
    #[cfg(windows)]
    let (path_separator, list_separator) = (b'\\', b';');
    #[cfg(not(windows))]
    let (path_separator, list_separator) = (b'/', b':');

    call.ret_i64(0, i64::from(path_separator));
    call.ret_i64(1, i64::from(list_separator));
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn handle_read_completion(
    call: &mut ExternCallContext,
    c: vo_runtime::io::Completion,
    ret_n: u16,
    ret_err: u16,
    requested: usize,
    require_full: bool,
) {
    match c.result {
        Ok(CompletionData::Size(n)) => {
            write_read_result(call, ret_n, ret_err, n, requested, require_full);
        }
        Ok(_) => panic!("unexpected completion data for read"),
        Err(e) => {
            call.ret_i64(ret_n, 0);
            write_io_error(call, ret_err, e);
        }
    }
}

#[cfg(feature = "std")]
fn handle_write_completion(
    call: &mut ExternCallContext,
    c: vo_runtime::io::Completion,
    ret_n: u16,
    ret_err: u16,
    requested: usize,
) {
    match c.result {
        Ok(CompletionData::Size(n)) => {
            write_write_result(call, ret_n, ret_err, n, requested);
        }
        Ok(_) => panic!("unexpected completion data for write"),
        Err(e) => {
            call.ret_i64(ret_n, 0);
            write_io_error(call, ret_err, e);
        }
    }
}

#[cfg(feature = "std")]
fn write_read_result(
    call: &mut ExternCallContext,
    ret_n: u16,
    ret_err: u16,
    n: usize,
    requested: usize,
    require_full: bool,
) {
    if n > requested {
        call.ret_i64(ret_n, 0);
        write_io_error(
            call,
            ret_err,
            std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "read completion exceeds the destination buffer",
            ),
        );
        return;
    }
    call.ret_i64(
        ret_n,
        i64::try_from(n).expect("Rust buffer lengths fit in int64"),
    );
    if requested > 0 && (n == 0 || (require_full && n < requested)) {
        let eof_pair = crate::io::io_sentinel_error(call, crate::io::IoErrorKind::EOF);
        call.ret_interface_pair(ret_err, eof_pair);
    } else {
        write_nil_error(call, ret_err);
    }
}

#[cfg(feature = "std")]
fn write_write_result(
    call: &mut ExternCallContext,
    ret_n: u16,
    ret_err: u16,
    n: usize,
    requested: usize,
) {
    if n > requested {
        call.ret_i64(ret_n, 0);
        write_io_error(
            call,
            ret_err,
            std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "write completion exceeds the source buffer",
            ),
        );
        return;
    }
    call.ret_i64(
        ret_n,
        i64::try_from(n).expect("Rust buffer lengths fit in int64"),
    );
    if n < requested {
        let short_write = crate::io::io_sentinel_error(call, crate::io::IoErrorKind::ShortWrite);
        call.ret_interface_pair(ret_err, short_write);
    } else {
        write_nil_error(call, ret_err);
    }
}

#[cfg(feature = "std")]
fn validate_write_at_mode(append: bool) -> std::io::Result<()> {
    if append {
        Err(invalid_input(
            "invalid use of WriteAt on file opened with O_APPEND",
        ))
    } else {
        Ok(())
    }
}

#[vostd_fn("os", "blocking_fileRead", std, effects(MAY_WAIT_IO_REPLAY))]
fn os_file_read(call: &mut ExternCallContext) -> ExternResult {
    let fd = match checked_i32(call.arg_i64(slots::ARG_FD), "file descriptor") {
        Ok(fd) => fd,
        Err(error) => {
            call.ret_i64(slots::RET_0, 0);
            write_io_error(call, slots::RET_1, error);
            return ExternResult::Ok;
        }
    };
    let buf_ref = call.arg_ref(slots::ARG_B);
    // Safety: `buf_ref` is a rooted []byte extern argument.
    let mut buf = unsafe { slice::byte_vec(buf_ref) };

    if fd == 0 {
        match std::io::stdin().read(&mut buf) {
            Ok(n) => {
                unsafe { slice::write_bytes(buf_ref, &buf[..n]) };
                write_read_result(call, slots::RET_0, slots::RET_1, n, buf.len(), false);
            }
            Err(e) => {
                call.ret_i64(slots::RET_0, 0);
                write_io_error(call, slots::RET_1, e);
            }
        }
        return ExternResult::Ok;
    }

    #[cfg(unix)]
    {
        let resume_token = call.take_resume_io_token();
        let token = match resume_token {
            Some(token) => token,
            None => {
                let lease = {
                    let handles = lock_recover(&FILE_HANDLES);
                    match handles.get(&fd) {
                        Some(file) => match file.lease(fd) {
                            Ok(lease) => lease,
                            Err(error) => {
                                call.ret_i64(slots::RET_0, 0);
                                write_io_error(call, slots::RET_1, error);
                                return ExternResult::Ok;
                            }
                        },
                        None => {
                            call.ret_i64(slots::RET_0, 0);
                            write_error_to(call, slots::RET_1, "invalid file descriptor");
                            return ExternResult::Ok;
                        }
                    }
                };
                let token = call.io_mut().submit_lease_slice_read(lease, buf_ref);
                match call.io_mut().try_take_completion(token) {
                    Some(c) => {
                        handle_read_completion(
                            call,
                            c,
                            slots::RET_0,
                            slots::RET_1,
                            buf.len(),
                            false,
                        );
                        return ExternResult::Ok;
                    }
                    None => return ExternResult::WaitIo { token },
                }
            }
        };

        let c = call.io_mut().take_completion(token);
        handle_read_completion(call, c, slots::RET_0, slots::RET_1, buf.len(), false);
        return ExternResult::Ok;
    }

    #[cfg(not(unix))]
    {
        let mut handles = lock_recover(&FILE_HANDLES);
        let file = match handles.get_mut(&fd) {
            Some(f) => f,
            None => {
                call.ret_i64(slots::RET_0, 0);
                write_error_to(call, slots::RET_1, "invalid file descriptor");
                return ExternResult::Ok;
            }
        };

        match file.read(&mut buf) {
            Ok(n) => {
                unsafe { slice::write_bytes(buf_ref, &buf[..n]) };
                write_read_result(call, slots::RET_0, slots::RET_1, n, buf.len(), false);
            }
            Err(e) => {
                call.ret_i64(slots::RET_0, 0);
                write_io_error(call, slots::RET_1, e);
            }
        }
        ExternResult::Ok
    }
}

#[vostd_fn("os", "blocking_fileWrite", std, effects(MAY_WAIT_IO_REPLAY))]
fn os_file_write(call: &mut ExternCallContext) -> ExternResult {
    let fd = match checked_i32(call.arg_i64(slots::ARG_FD), "file descriptor") {
        Ok(fd) => fd,
        Err(error) => {
            call.ret_i64(slots::RET_0, 0);
            write_io_error(call, slots::RET_1, error);
            return ExternResult::Ok;
        }
    };
    let buf_ref = call.arg_ref(slots::ARG_B);
    // Safety: `buf_ref` is a rooted []byte extern argument.
    let buf = unsafe { slice::byte_vec(buf_ref) };

    if fd == 1 || fd == 2 {
        let result = if fd == 1 {
            std::io::stdout().write(&buf)
        } else {
            std::io::stderr().write(&buf)
        };
        match result {
            Ok(n) => {
                write_write_result(call, slots::RET_0, slots::RET_1, n, buf.len());
            }
            Err(e) => {
                call.ret_i64(slots::RET_0, 0);
                write_io_error(call, slots::RET_1, e);
            }
        }
        return ExternResult::Ok;
    }

    #[cfg(unix)]
    {
        let resume_token = call.take_resume_io_token();
        let token = match resume_token {
            Some(token) => token,
            None => {
                let lease = {
                    let handles = lock_recover(&FILE_HANDLES);
                    match handles.get(&fd) {
                        Some(file) => match file.lease(fd) {
                            Ok(lease) => lease,
                            Err(error) => {
                                call.ret_i64(slots::RET_0, 0);
                                write_io_error(call, slots::RET_1, error);
                                return ExternResult::Ok;
                            }
                        },
                        None => {
                            call.ret_i64(slots::RET_0, 0);
                            write_error_to(call, slots::RET_1, "invalid file descriptor");
                            return ExternResult::Ok;
                        }
                    }
                };
                let token = call.io_mut().submit_lease_slice_write(lease, buf_ref);
                match call.io_mut().try_take_completion(token) {
                    Some(c) => {
                        handle_write_completion(call, c, slots::RET_0, slots::RET_1, buf.len());
                        return ExternResult::Ok;
                    }
                    None => return ExternResult::WaitIo { token },
                }
            }
        };

        let c = call.io_mut().take_completion(token);
        handle_write_completion(call, c, slots::RET_0, slots::RET_1, buf.len());
        return ExternResult::Ok;
    }

    #[cfg(not(unix))]
    {
        let mut handles = lock_recover(&FILE_HANDLES);
        let file = match handles.get_mut(&fd) {
            Some(f) => f,
            None => {
                call.ret_i64(slots::RET_0, 0);
                write_error_to(call, slots::RET_1, "invalid file descriptor");
                return ExternResult::Ok;
            }
        };

        match file.write(&buf) {
            Ok(n) => {
                write_write_result(call, slots::RET_0, slots::RET_1, n, buf.len());
            }
            Err(e) => {
                call.ret_i64(slots::RET_0, 0);
                write_io_error(call, slots::RET_1, e);
            }
        }
        ExternResult::Ok
    }
}

#[vostd_fn("os", "blocking_fileReadAt", std, effects(MAY_WAIT_IO_REPLAY))]
fn os_file_read_at(call: &mut ExternCallContext) -> ExternResult {
    let fd = match checked_i32(call.arg_i64(slots::ARG_FD), "file descriptor") {
        Ok(fd) => fd,
        Err(error) => {
            call.ret_i64(slots::RET_0, 0);
            write_io_error(call, slots::RET_1, error);
            return ExternResult::Ok;
        }
    };
    let buf_ref = call.arg_ref(slots::ARG_B);
    // Safety: `buf_ref` is a rooted []byte extern argument.
    let requested = unsafe { slice::len(buf_ref) };
    let offset = call.arg_i64(slots::ARG_OFF);
    if offset < 0 {
        call.ret_i64(slots::RET_0, 0);
        write_io_error(
            call,
            slots::RET_1,
            invalid_input("file offset must be non-negative"),
        );
        return ExternResult::Ok;
    }
    #[cfg(unix)]
    {
        let resume_token = call.take_resume_io_token();
        let token = match resume_token {
            Some(token) => token,
            None => {
                let lease = {
                    let handles = lock_recover(&FILE_HANDLES);
                    match handles.get(&fd) {
                        Some(file) => match file.lease(fd) {
                            Ok(lease) => lease,
                            Err(error) => {
                                call.ret_i64(slots::RET_0, 0);
                                write_io_error(call, slots::RET_1, error);
                                return ExternResult::Ok;
                            }
                        },
                        None => {
                            call.ret_i64(slots::RET_0, 0);
                            write_error_to(call, slots::RET_1, "invalid file descriptor");
                            return ExternResult::Ok;
                        }
                    }
                };
                let token = call
                    .io_mut()
                    .submit_lease_slice_read_at(lease, buf_ref, offset);
                match call.io_mut().try_take_completion(token) {
                    Some(c) => {
                        handle_read_completion(
                            call,
                            c,
                            slots::RET_0,
                            slots::RET_1,
                            requested,
                            true,
                        );
                        return ExternResult::Ok;
                    }
                    None => return ExternResult::WaitIo { token },
                }
            }
        };

        let c = call.io_mut().take_completion(token);
        handle_read_completion(call, c, slots::RET_0, slots::RET_1, requested, true);
        return ExternResult::Ok;
    }

    #[cfg(not(unix))]
    {
        // Safety: `buf_ref` is a rooted []byte extern argument.
        let mut buf = unsafe { slice::byte_vec(buf_ref) };
        let handles = lock_recover(&FILE_HANDLES);
        let file = match handles.get(&fd) {
            Some(f) => f,
            None => {
                call.ret_i64(slots::RET_0, 0);
                write_error_to(call, slots::RET_1, "invalid file descriptor");
                return ExternResult::Ok;
            }
        };

        match file.seek_read(&mut buf, offset as u64) {
            Ok(n) => {
                unsafe { slice::write_bytes(buf_ref, &buf[..n]) };
                write_read_result(call, slots::RET_0, slots::RET_1, n, requested, true);
            }
            Err(e) => {
                call.ret_i64(slots::RET_0, 0);
                write_io_error(call, slots::RET_1, e);
            }
        }
        ExternResult::Ok
    }
}

#[vostd_fn("os", "blocking_fileWriteAt", std, effects(MAY_WAIT_IO_REPLAY))]
fn os_file_write_at(call: &mut ExternCallContext) -> ExternResult {
    let fd = match checked_i32(call.arg_i64(slots::ARG_FD), "file descriptor") {
        Ok(fd) => fd,
        Err(error) => {
            call.ret_i64(slots::RET_0, 0);
            write_io_error(call, slots::RET_1, error);
            return ExternResult::Ok;
        }
    };
    let buf_ref = call.arg_ref(slots::ARG_B);
    // Safety: `buf_ref` is a rooted []byte extern argument.
    let requested = unsafe { slice::len(buf_ref) };
    let offset = call.arg_i64(slots::ARG_OFF);
    if offset < 0 {
        call.ret_i64(slots::RET_0, 0);
        write_io_error(
            call,
            slots::RET_1,
            invalid_input("file offset must be non-negative"),
        );
        return ExternResult::Ok;
    }
    #[cfg(unix)]
    {
        let resume_token = call.take_resume_io_token();
        let token = match resume_token {
            Some(token) => token,
            None => {
                let lease = {
                    let handles = lock_recover(&FILE_HANDLES);
                    match handles.get(&fd) {
                        Some(file) => {
                            if let Err(error) = validate_write_at_mode(file.append) {
                                call.ret_i64(slots::RET_0, 0);
                                write_io_error(call, slots::RET_1, error);
                                return ExternResult::Ok;
                            }
                            match file.lease(fd) {
                                Ok(lease) => lease,
                                Err(error) => {
                                    call.ret_i64(slots::RET_0, 0);
                                    write_io_error(call, slots::RET_1, error);
                                    return ExternResult::Ok;
                                }
                            }
                        }
                        None => {
                            call.ret_i64(slots::RET_0, 0);
                            write_error_to(call, slots::RET_1, "invalid file descriptor");
                            return ExternResult::Ok;
                        }
                    }
                };
                let token = call
                    .io_mut()
                    .submit_lease_slice_write_at(lease, buf_ref, offset);
                match call.io_mut().try_take_completion(token) {
                    Some(c) => {
                        handle_write_completion(call, c, slots::RET_0, slots::RET_1, requested);
                        return ExternResult::Ok;
                    }
                    None => return ExternResult::WaitIo { token },
                }
            }
        };

        let c = call.io_mut().take_completion(token);
        handle_write_completion(call, c, slots::RET_0, slots::RET_1, requested);
        return ExternResult::Ok;
    }

    #[cfg(not(unix))]
    {
        // Safety: `buf_ref` is a rooted []byte extern argument.
        let buf = unsafe { slice::byte_vec(buf_ref) };
        let handles = lock_recover(&FILE_HANDLES);
        let file = match handles.get(&fd) {
            Some(f) => f,
            None => {
                call.ret_i64(slots::RET_0, 0);
                write_error_to(call, slots::RET_1, "invalid file descriptor");
                return ExternResult::Ok;
            }
        };

        if let Err(error) = validate_write_at_mode(file.append) {
            call.ret_i64(slots::RET_0, 0);
            write_io_error(call, slots::RET_1, error);
            return ExternResult::Ok;
        }

        match file.seek_write(&buf, offset as u64) {
            Ok(n) => {
                write_write_result(call, slots::RET_0, slots::RET_1, n, requested);
            }
            Err(e) => {
                call.ret_i64(slots::RET_0, 0);
                write_io_error(call, slots::RET_1, e);
            }
        }
        ExternResult::Ok
    }
}

#[vostd_fn("os", "fileSeek", std)]
fn os_file_seek(call: &mut ExternCallContext) -> ExternResult {
    let fd = match checked_i32(call.arg_i64(slots::ARG_FD), "file descriptor") {
        Ok(fd) => fd,
        Err(error) => {
            call.ret_i64(slots::RET_0, 0);
            write_io_error(call, slots::RET_1, error);
            return ExternResult::Ok;
        }
    };
    let offset = call.arg_i64(slots::ARG_OFFSET);
    let whence = call.arg_i64(slots::ARG_WHENCE);

    with_file_mut!(fd, call, slots::RET_1, ret0, |file| {
        let seek_from = match whence {
            0 if offset >= 0 => Some(SeekFrom::Start(offset as u64)),
            0 => {
                call.ret_i64(slots::RET_0, 0);
                write_io_error(
                    call,
                    slots::RET_1,
                    invalid_input("negative offset is invalid for SeekStart"),
                );
                None
            }
            1 => Some(SeekFrom::Current(offset)),
            2 => Some(SeekFrom::End(offset)),
            _ => {
                call.ret_i64(slots::RET_0, 0);
                write_error_to(call, slots::RET_1, "invalid whence");
                None
            }
        };
        if let Some(sf) = seek_from {
            match file.seek(sf) {
                Ok(pos) => match i64::try_from(pos) {
                    Ok(pos) => {
                        call.ret_i64(slots::RET_0, pos);
                        write_nil_error(call, slots::RET_1);
                    }
                    Err(_) => {
                        call.ret_i64(slots::RET_0, 0);
                        write_io_error(
                            call,
                            slots::RET_1,
                            invalid_input("seek result exceeds int64"),
                        );
                    }
                },
                Err(e) => {
                    call.ret_i64(slots::RET_0, 0);
                    write_io_error(call, slots::RET_1, e);
                }
            }
        }
    });
    ExternResult::Ok
}

#[vostd_fn("os", "fileClose", std)]
fn os_file_close(call: &mut ExternCallContext) -> ExternResult {
    let fd = match checked_i32(call.arg_i64(slots::ARG_FD), "file descriptor") {
        Ok(fd) => fd,
        Err(error) => {
            write_io_error(call, slots::RET_0, error);
            return ExternResult::Ok;
        }
    };
    if (0..=2).contains(&fd) {
        write_nil_error(call, slots::RET_0);
        return ExternResult::Ok;
    }
    if let Some(file) = remove_file(fd) {
        call.io_mut().disarm_resource_cleanup(file.cleanup_token);
        #[cfg(unix)]
        {
            let cancel_key = file.cancel(fd);
            call.io_mut().cancel(cancel_key);
        }
        write_nil_error(call, slots::RET_0);
    } else {
        write_error_to(call, slots::RET_0, "invalid file descriptor");
    }
    ExternResult::Ok
}

#[vostd_fn("os", "fileSync", std)]
fn os_file_sync(call: &mut ExternCallContext) -> ExternResult {
    let fd = match checked_i32(call.arg_i64(slots::ARG_FD), "file descriptor") {
        Ok(fd) => fd,
        Err(error) => {
            write_io_error(call, slots::RET_0, error);
            return ExternResult::Ok;
        }
    };
    with_file!(fd, call, slots::RET_0, |file| {
        match file.sync_all() {
            Ok(_) => write_nil_error(call, slots::RET_0),
            Err(e) => write_io_error(call, slots::RET_0, e),
        }
    });
    ExternResult::Ok
}

#[vostd_fn("os", "fileStat", std)]
fn os_file_stat(call: &mut ExternCallContext) -> ExternResult {
    let fd = match checked_i32(call.arg_i64(slots::ARG_FD), "file descriptor") {
        Ok(fd) => fd,
        Err(error) => {
            return_file_info_error(call, error);
            return ExternResult::Ok;
        }
    };
    let path = match path_arg(call, slots::ARG_NAME, "file name") {
        Ok(path) => path,
        Err(error) => {
            return_file_info_error(call, error);
            return ExternResult::Ok;
        }
    };
    let name = match file_info_name(&path) {
        Ok(name) => name,
        Err(error) => {
            return_file_info_error(call, error);
            return ExternResult::Ok;
        }
    };
    with_file!(fd, call, 5, nil 5, |file| {
        match file.metadata() {
            Ok(meta) => {
                let file_info = match metadata_to_file_info(call, &name, &meta) {
                    Ok(file_info) => file_info,
                    Err(error) => return ExternResult::Panic(error),
                };
                return_file_info(call, file_info);
            }
            Err(error) => return_file_info_error(call, error),
        }
    });
    ExternResult::Ok
}

#[vostd_fn("os", "fileTruncate", std)]
fn os_file_truncate(call: &mut ExternCallContext) -> ExternResult {
    let fd = match checked_i32(call.arg_i64(slots::ARG_FD), "file descriptor") {
        Ok(fd) => fd,
        Err(error) => {
            write_io_error(call, slots::RET_0, error);
            return ExternResult::Ok;
        }
    };
    let size = match checked_file_size(call.arg_i64(slots::ARG_SIZE)) {
        Ok(size) => size,
        Err(error) => {
            write_io_error(call, slots::RET_0, error);
            return ExternResult::Ok;
        }
    };
    with_file!(fd, call, slots::RET_0, |file| {
        match file.set_len(size) {
            Ok(_) => write_nil_error(call, slots::RET_0),
            Err(e) => write_io_error(call, slots::RET_0, e),
        }
    });
    ExternResult::Ok
}

// O_* flags - generated together with the stdlib getter registration.
vostd_consts! {
    "os" => {
        O_RDONLY => 0,
        O_WRONLY => 1,
        O_RDWR => 2,
        O_APPEND => 8,
        O_CREATE => 16,
        O_EXCL => 32,
        O_SYNC => 64,
        O_TRUNC => 128,
    }
}

#[vostd_fn("os", "openFile", std)]
fn os_open_file(call: &mut ExternCallContext) -> ExternResult {
    let flag = match checked_i32(call.arg_i64(slots::ARG_FLAG), "OpenFile flags") {
        Ok(flag) => flag,
        Err(error) => {
            call.ret_i64(slots::RET_0, -1);
            write_io_error(call, slots::RET_1, error);
            return ExternResult::Ok;
        }
    };
    let perm = call.arg_u64(slots::ARG_PERM) as u32;

    let result = path_arg(call, slots::ARG_NAME, "file name")
        .and_then(|name| open_file_with_mode(&name, flag, perm));
    let result = match result {
        Ok(file) => register_file(call.io_mut(), file, flag & O_APPEND as i32 != 0),
        Err(error) => Err(error),
    };
    match result {
        Ok(fd) => {
            call.ret_i64(slots::RET_0, fd as i64);
            write_nil_error(call, slots::RET_1);
        }
        Err(e) => {
            call.ret_i64(slots::RET_0, -1);
            write_io_error(call, slots::RET_1, e);
        }
    }
    ExternResult::Ok
}

#[vostd_fn("os", "nativeMkdir", std)]
fn os_mkdir(call: &mut ExternCallContext) -> ExternResult {
    let perm = call.arg_u64(slots::ARG_PERM) as u32;
    match path_arg(call, slots::ARG_PATH, "directory path")
        .and_then(|path| create_dir_with_mode(&path, perm))
    {
        Ok(()) => write_nil_error(call, slots::RET_0),
        Err(e) => write_io_error(call, slots::RET_0, e),
    }
    ExternResult::Ok
}

#[vostd_fn("os", "nativeMkdirAll", std)]
fn os_mkdir_all(call: &mut ExternCallContext) -> ExternResult {
    let perm = call.arg_u64(slots::ARG_PERM) as u32;
    match path_arg(call, slots::ARG_PATH, "directory path")
        .and_then(|path| create_dir_all_with_mode(&path, perm))
    {
        Ok(()) => write_nil_error(call, slots::RET_0),
        Err(e) => write_io_error(call, slots::RET_0, e),
    }
    ExternResult::Ok
}

#[vostd_fn("os", "nativeRemove", std)]
fn os_remove(call: &mut ExternCallContext) -> ExternResult {
    match path_arg(call, slots::ARG_NAME, "path").and_then(|path| remove_path(&path)) {
        Ok(()) => write_nil_error(call, slots::RET_0),
        Err(error) => write_io_error(call, slots::RET_0, error),
    }
    ExternResult::Ok
}

#[vostd_fn("os", "nativeRemoveAll", std)]
fn os_remove_all(call: &mut ExternCallContext) -> ExternResult {
    match path_arg(call, slots::ARG_PATH, "path").and_then(|path| remove_all_path(&path)) {
        Ok(()) => write_nil_error(call, slots::RET_0),
        Err(error) => write_io_error(call, slots::RET_0, error),
    }
    ExternResult::Ok
}

#[vostd_fn("os", "nativeRename", std)]
fn os_rename(call: &mut ExternCallContext) -> ExternResult {
    let result = path_arg(call, slots::ARG_OLDPATH, "old path").and_then(|oldpath| {
        path_arg(call, slots::ARG_NEWPATH, "new path")
            .and_then(|newpath| fs::rename(oldpath, newpath))
    });
    match result {
        Ok(_) => write_nil_error(call, slots::RET_0),
        Err(e) => write_io_error(call, slots::RET_0, e),
    }
    ExternResult::Ok
}

#[vostd_fn("os", "nativeStat", std)]
fn os_stat(call: &mut ExternCallContext) -> ExternResult {
    let result = path_arg(call, slots::ARG_NAME, "file name")
        .and_then(|name| fs::metadata(&name).map(|metadata| (name, metadata)));
    match result {
        Ok((name, meta)) => {
            let basename = match file_info_name(&name) {
                Ok(name) => name,
                Err(error) => {
                    return_file_info_error(call, error);
                    return ExternResult::Ok;
                }
            };
            let file_info = match metadata_to_file_info(call, &basename, &meta) {
                Ok(file_info) => file_info,
                Err(error) => return ExternResult::Panic(error),
            };
            return_file_info(call, file_info);
        }
        Err(error) => return_file_info_error(call, error),
    }
    ExternResult::Ok
}

#[vostd_fn("os", "nativeLstat", std)]
fn os_lstat(call: &mut ExternCallContext) -> ExternResult {
    let result = path_arg(call, slots::ARG_NAME, "file name")
        .and_then(|name| fs::symlink_metadata(&name).map(|metadata| (name, metadata)));
    match result {
        Ok((name, meta)) => {
            let basename = match file_info_name(&name) {
                Ok(name) => name,
                Err(error) => {
                    return_file_info_error(call, error);
                    return ExternResult::Ok;
                }
            };
            let file_info = match metadata_to_file_info(call, &basename, &meta) {
                Ok(file_info) => file_info,
                Err(error) => return ExternResult::Panic(error),
            };
            return_file_info(call, file_info);
        }
        Err(error) => return_file_info_error(call, error),
    }
    ExternResult::Ok
}

#[cfg(feature = "std")]
type NativeDirEntry = (Vec<u8>, bool, u32);

#[cfg(feature = "std")]
fn os_name_bytes(name: std::ffi::OsString) -> std::io::Result<Vec<u8>> {
    os_string_bytes(name, "directory entry name")
}

#[cfg(feature = "std")]
fn decode_native_dir_entry(entry: fs::DirEntry) -> std::io::Result<NativeDirEntry> {
    // DirEntry::metadata may follow links on some hosts. An explicit lstat-style
    // lookup provides one coherent snapshot for Type, IsDir, and permissions.
    let metadata = fs::symlink_metadata(entry.path())?;
    let mode = metadata_to_file_mode(&metadata);
    let name = os_name_bytes(entry.file_name())?;
    Ok((name, metadata.is_dir(), mode))
}

#[cfg(feature = "std")]
fn collect_until_io_error<T>(
    values: impl IntoIterator<Item = std::io::Result<T>>,
) -> (Vec<T>, Option<std::io::Error>) {
    let mut collected = Vec::new();
    for value in values {
        match value {
            Ok(value) => collected.push(value),
            Err(error) => return (collected, Some(error)),
        }
    }
    (collected, None)
}

#[cfg(feature = "std")]
fn read_native_dir_entries(entries: fs::ReadDir) -> (Vec<NativeDirEntry>, Option<std::io::Error>) {
    let (mut entries, error) =
        collect_until_io_error(entries.map(|entry| entry.and_then(decode_native_dir_entry)));
    entries.sort_by(|left, right| left.0.cmp(&right.0));
    (entries, error)
}

#[vostd_fn("os", "nativeReadDir", std)]
fn os_read_dir(call: &mut ExternCallContext) -> ExternResult {
    match path_arg(call, slots::ARG_NAME, "directory path").and_then(fs::read_dir) {
        Ok(entries) => {
            let (dir_entries, read_error) = read_native_dir_entries(entries);
            let len = dir_entries.len();
            let elem_meta = match call
                .named_type_metas()
                .iter()
                .find(|meta| meta.name == "os.DirEntry")
                .map(|meta| meta.underlying_meta)
            {
                Some(meta) if meta.value_kind() == ValueKind::Struct => meta,
                _ => {
                    return ExternResult::Panic(
                        "os.nativeReadDir could not resolve os.DirEntry metadata".to_string(),
                    );
                }
            };
            let (elem_slots, name_offset, is_dir_offset, mode_offset) = {
                let Some(meta) = call.struct_meta(elem_meta.meta_id() as usize) else {
                    return ExternResult::Panic(
                        "os.nativeReadDir resolved invalid os.DirEntry struct metadata".to_string(),
                    );
                };
                let Some(name) = meta.get_field("name") else {
                    return ExternResult::Panic(
                        "os.nativeReadDir os.DirEntry metadata is missing name".to_string(),
                    );
                };
                let Some(is_dir) = meta.get_field("isDir") else {
                    return ExternResult::Panic(
                        "os.nativeReadDir os.DirEntry metadata is missing isDir".to_string(),
                    );
                };
                let Some(mode) = meta.get_field("mode") else {
                    return ExternResult::Panic(
                        "os.nativeReadDir os.DirEntry metadata is missing mode".to_string(),
                    );
                };
                if name.slot_count != 1
                    || is_dir.slot_count != 1
                    || mode.slot_count != 1
                    || name.type_info.value_kind() != ValueKind::String
                    || is_dir.type_info.value_kind() != ValueKind::Bool
                    || mode.type_info.value_kind() != ValueKind::Uint32
                {
                    return ExternResult::Panic(
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
                    return ExternResult::Panic(
                        "os.nativeReadDir os.DirEntry field offset exceeds its struct layout"
                            .to_string(),
                    );
                }
                (elem_slots, name_offset, is_dir_offset, mode_offset)
            };
            let Some(elem_bytes) = elem_slots.checked_mul(SLOT_BYTES) else {
                return ExternResult::Panic(
                    "os.nativeReadDir os.DirEntry layout exceeds the target address width"
                        .to_string(),
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
            for (i, (entry_name, is_dir, mode)) in dir_entries.iter().enumerate() {
                let name_ref = call.alloc_string_bytes(entry_name);
                value.fill(0);
                value[name_offset] = name_ref as u64;
                value[is_dir_offset] = u64::from(*is_dir);
                value[mode_offset] = u64::from(*mode);
                call.typed_write_barrier_by_meta(owner, &value, elem_meta);
                unsafe {
                    slice::write_logical_slots(result, i, &value);
                }
            }
            call.gc().mark_allocated_for_scan(owner);
            call.ret_ref(slots::RET_0, result);
            if let Some(error) = read_error {
                write_io_error(call, slots::RET_1, error);
            } else {
                write_nil_error(call, slots::RET_1);
            }
        }
        Err(e) => {
            call.ret_nil(slots::RET_0);
            write_io_error(call, slots::RET_1, e);
        }
    }
    ExternResult::Ok
}

#[vostd_fn("os", "nativeChmod", std)]
fn os_chmod(call: &mut ExternCallContext) -> ExternResult {
    let mode = call.arg_u64(slots::ARG_MODE) as u32;
    match path_arg(call, slots::ARG_NAME, "file name")
        .and_then(|name| set_path_permissions(&name, mode))
    {
        Ok(_) => write_nil_error(call, slots::RET_0),
        Err(e) => write_io_error(call, slots::RET_0, e),
    }
    ExternResult::Ok
}

#[vostd_fn("os", "nativeChown", std)]
fn os_chown(call: &mut ExternCallContext) -> ExternResult {
    let uid = match checked_chown_id(call.arg_i64(slots::ARG_UID), "user id") {
        Ok(uid) => uid,
        Err(error) => {
            write_io_error(call, slots::RET_0, error);
            return ExternResult::Ok;
        }
    };
    let gid = match checked_chown_id(call.arg_i64(slots::ARG_GID), "group id") {
        Ok(gid) => gid,
        Err(error) => {
            write_io_error(call, slots::RET_0, error);
            return ExternResult::Ok;
        }
    };
    #[cfg(unix)]
    {
        use std::os::unix::fs::chown;
        match path_arg(call, slots::ARG_NAME, "file name")
            .and_then(|name| chown(name, Some(uid), Some(gid)))
        {
            Ok(_) => write_nil_error(call, slots::RET_0),
            Err(e) => write_io_error(call, slots::RET_0, e),
        }
    }
    #[cfg(not(unix))]
    {
        write_error_to(call, slots::RET_0, "chown not supported");
    }
    ExternResult::Ok
}

#[vostd_fn("os", "nativeSymlink", std)]
fn os_symlink(call: &mut ExternCallContext) -> ExternResult {
    #[cfg(unix)]
    {
        let result =
            path_arg(call, slots::ARG_OLDNAME, "symbolic link target").and_then(|oldname| {
                path_arg(call, slots::ARG_NEWNAME, "symbolic link path")
                    .and_then(|newname| symlink(oldname, newname))
            });
        match result {
            Ok(_) => write_nil_error(call, slots::RET_0),
            Err(e) => write_io_error(call, slots::RET_0, e),
        }
    }
    #[cfg(not(unix))]
    {
        write_error_to(call, slots::RET_0, "symlink not supported");
    }
    ExternResult::Ok
}

#[vostd_fn("os", "nativeReadlink", std)]
fn os_readlink(call: &mut ExternCallContext) -> ExternResult {
    let result = path_arg(call, slots::ARG_NAME, "symbolic link path")
        .and_then(fs::read_link)
        .and_then(|path| path_bytes(path, "symbolic link target"));
    match result {
        Ok(path) => {
            call.ret_string_bytes(slots::RET_0, &path);
            write_nil_error(call, slots::RET_1);
        }
        Err(e) => {
            call.ret_string_bytes(slots::RET_0, b"");
            write_io_error(call, slots::RET_1, e);
        }
    }
    ExternResult::Ok
}

#[vostd_fn("os", "nativeLink", std)]
fn os_link(call: &mut ExternCallContext) -> ExternResult {
    let result = path_arg(call, slots::ARG_OLDNAME, "existing file path").and_then(|oldname| {
        path_arg(call, slots::ARG_NEWNAME, "new link path")
            .and_then(|newname| fs::hard_link(oldname, newname))
    });
    match result {
        Ok(_) => write_nil_error(call, slots::RET_0),
        Err(e) => write_io_error(call, slots::RET_0, e),
    }
    ExternResult::Ok
}

#[vostd_fn("os", "nativeTruncate", std)]
fn os_truncate(call: &mut ExternCallContext) -> ExternResult {
    let size = match checked_file_size(call.arg_i64(slots::ARG_SIZE)) {
        Ok(size) => size,
        Err(error) => {
            write_io_error(call, slots::RET_0, error);
            return ExternResult::Ok;
        }
    };
    match path_arg(call, slots::ARG_NAME, "file name")
        .and_then(|name| File::options().write(true).open(name))
    {
        Ok(file) => match file.set_len(size) {
            Ok(_) => write_nil_error(call, slots::RET_0),
            Err(e) => write_io_error(call, slots::RET_0, e),
        },
        Err(e) => write_io_error(call, slots::RET_0, e),
    }
    ExternResult::Ok
}

#[vostd_fn("os", "nativeReadFile", std)]
fn os_read_file_native(call: &mut ExternCallContext) -> ExternResult {
    match path_arg(call, slots::ARG_NAME, "file name").and_then(fs::read) {
        Ok(data) => {
            call.ret_bytes(slots::RET_0, &data);
            write_nil_error(call, slots::RET_1);
        }
        Err(e) => {
            call.ret_nil(slots::RET_0);
            write_io_error(call, slots::RET_1, e);
        }
    }
    ExternResult::Ok
}

#[vostd_fn("os", "nativeWriteFile", std)]
fn os_write_file_native(call: &mut ExternCallContext) -> ExternResult {
    let data = call.arg_bytes(slots::ARG_DATA);
    let perm = call.arg_u64(slots::ARG_PERM) as u32;
    match path_arg(call, slots::ARG_NAME, "file name")
        .and_then(|name| write_file_with_mode(&name, data, perm))
    {
        Ok(()) => write_nil_error(call, slots::RET_0),
        Err(e) => write_io_error(call, slots::RET_0, e),
    }
    ExternResult::Ok
}

#[vostd_fn("os", "nativeGetenv", std)]
fn os_getenv(call: &mut ExternCallContext) -> ExternResult {
    let key = call.arg_string_bytes(slots::ARG_KEY);
    call.ret_string_bytes(slots::RET_0, &lookup_env_bytes(&key).unwrap_or_default());
    ExternResult::Ok
}

#[vostd_fn("os", "nativeSetenv", std)]
fn os_setenv(call: &mut ExternCallContext) -> ExternResult {
    let key = call.arg_string_bytes(slots::ARG_KEY);
    let value = call.arg_string_bytes(slots::ARG_VALUE);
    match set_env_checked(&key, &value) {
        Ok(()) => write_nil_error(call, slots::RET_0),
        Err(error) => write_io_error(call, slots::RET_0, error),
    }
    ExternResult::Ok
}

#[vostd_fn("os", "nativeUnsetenv", std)]
fn os_unsetenv(call: &mut ExternCallContext) -> ExternResult {
    let key = call.arg_string_bytes(slots::ARG_KEY);
    match unset_env_checked(&key) {
        Ok(()) => write_nil_error(call, slots::RET_0),
        Err(error) => write_io_error(call, slots::RET_0, error),
    }
    ExternResult::Ok
}

#[vostd_fn("os", "nativeEnviron", std)]
fn os_environ(call: &mut ExternCallContext) -> ExternResult {
    // Environ has no error return. Keep every representable entry and omit
    // host entries that cannot be represented as a valid native string;
    // std::env::vars would panic as soon as it encountered one.
    let vars: Vec<Vec<u8>> = std::env::vars_os()
        .filter_map(|(key, value)| environment_entry(key, value).ok())
        .collect();
    call.ret_string_bytes_slice(slots::RET_0, &vars);
    ExternResult::Ok
}

#[vostd_fn("os", "nativeLookupEnv", std)]
fn os_lookup_env(call: &mut ExternCallContext) -> ExternResult {
    let key = call.arg_string_bytes(slots::ARG_KEY);
    match lookup_env_bytes(&key) {
        Some(value) => {
            call.ret_string_bytes(slots::RET_0, &value);
            call.ret_bool(slots::RET_1, true);
        }
        None => {
            call.ret_string_bytes(slots::RET_0, b"");
            call.ret_bool(slots::RET_1, false);
        }
    }
    ExternResult::Ok
}

#[vostd_fn("os", "nativeClearenv", std)]
fn os_clearenv(_call: &mut ExternCallContext) -> ExternResult {
    let keys = std::env::vars_os().map(|(key, _)| key).collect::<Vec<_>>();
    for key in keys {
        std::env::remove_var(key);
    }
    ExternResult::Ok
}

#[vostd_fn("os", "nativeExpandEnv", std)]
fn os_expand_env(call: &mut ExternCallContext) -> ExternResult {
    let result = expand_env_bytes(&call.arg_string_bytes(slots::ARG_S));
    call.ret_string_bytes(slots::RET_0, &result);
    ExternResult::Ok
}

#[vostd_fn("os", "nativeGetwd", std)]
fn os_getwd(call: &mut ExternCallContext) -> ExternResult {
    match std::env::current_dir().and_then(|path| path_bytes(path, "working directory")) {
        Ok(path) => {
            call.ret_string_bytes(slots::RET_0, &path);
            write_nil_error(call, slots::RET_1);
        }
        Err(e) => {
            call.ret_string_bytes(slots::RET_0, b"");
            write_io_error(call, slots::RET_1, e);
        }
    }
    ExternResult::Ok
}

#[vostd_fn("os", "nativeChdir", std)]
fn os_chdir(call: &mut ExternCallContext) -> ExternResult {
    match path_arg(call, slots::ARG_DIR, "working directory").and_then(std::env::set_current_dir) {
        Ok(_) => write_nil_error(call, slots::RET_0),
        Err(e) => write_io_error(call, slots::RET_0, e),
    }
    ExternResult::Ok
}

#[vostd_fn("os", "nativeUserHomeDir", std)]
fn os_user_home_dir(call: &mut ExternCallContext) -> ExternResult {
    match dirs::home_dir() {
        Some(path) => match path_bytes(path, "home directory") {
            Ok(path) => {
                call.ret_string_bytes(slots::RET_0, &path);
                write_nil_error(call, slots::RET_1);
            }
            Err(error) => {
                call.ret_string_bytes(slots::RET_0, b"");
                write_io_error(call, slots::RET_1, error);
            }
        },
        None => {
            call.ret_string_bytes(slots::RET_0, b"");
            write_error_to(call, slots::RET_1, "home directory not found");
        }
    }
    ExternResult::Ok
}

#[vostd_fn("os", "nativeUserCacheDir", std)]
fn os_user_cache_dir(call: &mut ExternCallContext) -> ExternResult {
    match dirs::cache_dir() {
        Some(path) => match path_bytes(path, "cache directory") {
            Ok(path) => {
                call.ret_string_bytes(slots::RET_0, &path);
                write_nil_error(call, slots::RET_1);
            }
            Err(error) => {
                call.ret_string_bytes(slots::RET_0, b"");
                write_io_error(call, slots::RET_1, error);
            }
        },
        None => {
            call.ret_string_bytes(slots::RET_0, b"");
            write_error_to(call, slots::RET_1, "cache directory not found");
        }
    }
    ExternResult::Ok
}

#[vostd_fn("os", "nativeUserConfigDir", std)]
fn os_user_config_dir(call: &mut ExternCallContext) -> ExternResult {
    match dirs::config_dir() {
        Some(path) => match path_bytes(path, "config directory") {
            Ok(path) => {
                call.ret_string_bytes(slots::RET_0, &path);
                write_nil_error(call, slots::RET_1);
            }
            Err(error) => {
                call.ret_string_bytes(slots::RET_0, b"");
                write_io_error(call, slots::RET_1, error);
            }
        },
        None => {
            call.ret_string_bytes(slots::RET_0, b"");
            write_error_to(call, slots::RET_1, "config directory not found");
        }
    }
    ExternResult::Ok
}

#[vostd_fn("os", "nativeTempDir", std)]
fn os_temp_dir(call: &mut ExternCallContext) -> ExternResult {
    // TempDir has no error result. Unix paths always round-trip; on hosts
    // whose native path encoding cannot cross the platform's strict UTF-8
    // boundary, return the documented empty fallback without substitution.
    let path = path_bytes(std::env::temp_dir(), "temporary directory").unwrap_or_default();
    call.ret_string_bytes(slots::RET_0, &path);
    ExternResult::Ok
}

#[vostd_fn("os", "nativeGetpid", std)]
fn os_getpid(call: &mut ExternCallContext) -> ExternResult {
    call.ret_i64(slots::RET_0, std::process::id() as i64);
    ExternResult::Ok
}
#[vostd_fn("os", "nativeGetppid", std)]
fn os_getppid(call: &mut ExternCallContext) -> ExternResult {
    #[cfg(unix)]
    {
        call.ret_i64(slots::RET_0, unsafe { libc::getppid() } as i64);
    }
    #[cfg(not(unix))]
    {
        call.ret_i64(slots::RET_0, 0);
    }
    ExternResult::Ok
}
#[vostd_fn("os", "nativeGetuid", std)]
fn os_getuid(call: &mut ExternCallContext) -> ExternResult {
    #[cfg(unix)]
    {
        call.ret_i64(slots::RET_0, unsafe { libc::getuid() } as i64);
    }
    #[cfg(not(unix))]
    {
        call.ret_i64(slots::RET_0, 0);
    }
    ExternResult::Ok
}
#[vostd_fn("os", "nativeGeteuid", std)]
fn os_geteuid(call: &mut ExternCallContext) -> ExternResult {
    #[cfg(unix)]
    {
        call.ret_i64(slots::RET_0, unsafe { libc::geteuid() } as i64);
    }
    #[cfg(not(unix))]
    {
        call.ret_i64(slots::RET_0, 0);
    }
    ExternResult::Ok
}
#[vostd_fn("os", "nativeGetgid", std)]
fn os_getgid(call: &mut ExternCallContext) -> ExternResult {
    #[cfg(unix)]
    {
        call.ret_i64(slots::RET_0, unsafe { libc::getgid() } as i64);
    }
    #[cfg(not(unix))]
    {
        call.ret_i64(slots::RET_0, 0);
    }
    ExternResult::Ok
}
#[vostd_fn("os", "nativeGetegid", std)]
fn os_getegid(call: &mut ExternCallContext) -> ExternResult {
    #[cfg(unix)]
    {
        call.ret_i64(slots::RET_0, unsafe { libc::getegid() } as i64);
    }
    #[cfg(not(unix))]
    {
        call.ret_i64(slots::RET_0, 0);
    }
    ExternResult::Ok
}
#[vostd_fn("os", "nativeExit", std, effects(MAY_EXIT))]
fn os_exit(call: &mut ExternCallContext) -> ExternResult {
    match checked_i32(call.arg_i64(slots::ARG_CODE), "os.Exit status code") {
        Ok(code) => ExternResult::Exit(code),
        Err(error) => ExternResult::Panic(error.to_string()),
    }
}
#[vostd_fn("os", "nativeGetArgs", std)]
fn os_get_args(call: &mut ExternCallContext) -> ExternResult {
    let args = call.program_args().to_vec();
    call.ret_string_bytes_slice(slots::RET_0, &args);
    ExternResult::Ok
}

#[vostd_fn("os", "nativeIsTerminal", std)]
fn os_is_terminal(call: &mut ExternCallContext) -> ExternResult {
    let fd = checked_i32(call.arg_i64(slots::ARG_FD), "file descriptor").ok();
    let ok = match fd {
        Some(0) => std::io::stdin().is_terminal(),
        Some(1) => std::io::stdout().is_terminal(),
        Some(2) => std::io::stderr().is_terminal(),
        Some(fd) => {
            let handles = lock_recover(&FILE_HANDLES);
            handles
                .get(&fd)
                .map(|file| file.is_terminal())
                .unwrap_or(false)
        }
        None => false,
    };
    call.ret_bool(slots::RET_0, ok);
    ExternResult::Ok
}

#[vostd_fn("os", "nativeHostname", std)]
fn os_hostname(call: &mut ExternCallContext) -> ExternResult {
    match hostname::get().and_then(|name| os_string_bytes(name, "host name")) {
        Ok(name) => {
            call.ret_string_bytes(slots::RET_0, &name);
            write_nil_error(call, slots::RET_1);
        }
        Err(e) => {
            call.ret_string_bytes(slots::RET_0, b"");
            write_io_error(call, slots::RET_1, e);
        }
    }
    ExternResult::Ok
}

#[vostd_fn("os", "nativeExecutable", std)]
fn os_executable(call: &mut ExternCallContext) -> ExternResult {
    match std::env::current_exe().and_then(|path| path_bytes(path, "executable path")) {
        Ok(path) => {
            call.ret_string_bytes(slots::RET_0, &path);
            write_nil_error(call, slots::RET_1);
        }
        Err(e) => {
            call.ret_string_bytes(slots::RET_0, b"");
            write_io_error(call, slots::RET_1, e);
        }
    }
    ExternResult::Ok
}

#[vostd_fn("os", "nativeCreateTemp", std)]
fn os_create_temp(call: &mut ExternCallContext) -> ExternResult {
    let dir = call.arg_string_bytes(slots::ARG_DIR);
    let pattern = call.arg_string_bytes(slots::ARG_PATTERN);
    let result = temp_parent(dir).and_then(|dir| {
        let (file, path) = create_temp_file_in(&dir, &pattern)?;
        let path_value = match path_bytes(path.clone(), "temporary-file path") {
            Ok(path_value) => path_value,
            Err(error) => {
                drop(file);
                let _ = fs::remove_file(path);
                return Err(error);
            }
        };
        match register_file(call.io_mut(), file, false) {
            Ok(fd) => Ok((fd, path_value)),
            Err(error) => {
                let _ = fs::remove_file(path);
                Err(error)
            }
        }
    });
    match result {
        Ok((fd, path)) => {
            call.ret_i64(slots::RET_0, fd as i64);
            call.ret_string_bytes(slots::RET_1, &path);
            write_nil_error(call, slots::RET_2);
        }
        Err(error) => {
            call.ret_i64(slots::RET_0, -1);
            call.ret_string_bytes(slots::RET_1, b"");
            write_io_error(call, slots::RET_2, error);
        }
    }
    ExternResult::Ok
}

#[vostd_fn("os", "nativeMkdirTemp", std)]
fn os_mkdir_temp(call: &mut ExternCallContext) -> ExternResult {
    let dir = call.arg_string_bytes(slots::ARG_DIR);
    let pattern = call.arg_string_bytes(slots::ARG_PATTERN);
    let result = temp_parent(dir).and_then(|dir| {
        let path = create_temp_dir_in(&dir, &pattern)?;
        match path_bytes(path.clone(), "temporary-directory path") {
            Ok(path_value) => Ok(path_value),
            Err(error) => {
                let _ = fs::remove_dir(path);
                Err(error)
            }
        }
    });
    match result {
        Ok(path) => {
            call.ret_string_bytes(slots::RET_0, &path);
            write_nil_error(call, slots::RET_1);
        }
        Err(error) => {
            call.ret_string_bytes(slots::RET_0, b"");
            write_io_error(call, slots::RET_1, error);
        }
    }
    ExternResult::Ok
}

// ==================== Pipe ====================

#[cfg(all(feature = "std", unix))]
#[vostd_fn("os", "nativePipe", std)]
fn os_native_pipe(call: &mut ExternCallContext) -> ExternResult {
    use std::os::unix::io::{FromRawFd, IntoRawFd};

    match nix::unistd::pipe() {
        Ok((read_fd, write_fd)) => {
            let rfd_raw = read_fd.into_raw_fd();
            let wfd_raw = write_fd.into_raw_fd();

            // Wrap raw fds into File and register in our handle system
            let r_file = unsafe { File::from_raw_fd(rfd_raw) };
            let w_file = unsafe { File::from_raw_fd(wfd_raw) };

            let rfd = match register_file(call.io_mut(), r_file, false) {
                Ok(fd) => fd,
                Err(error) => {
                    call.ret_i64(slots::RET_0, -1);
                    call.ret_i64(slots::RET_1, -1);
                    write_io_error(call, slots::RET_2, error);
                    return ExternResult::Ok;
                }
            };
            match register_file(call.io_mut(), w_file, false) {
                Ok(wfd) => {
                    call.ret_i64(slots::RET_0, rfd as i64);
                    call.ret_i64(slots::RET_1, wfd as i64);
                    write_nil_error(call, slots::RET_2);
                }
                Err(error) => {
                    discard_file(call.io_mut(), rfd);
                    call.ret_i64(slots::RET_0, -1);
                    call.ret_i64(slots::RET_1, -1);
                    write_io_error(call, slots::RET_2, error);
                }
            }
        }
        Err(e) => {
            call.ret_i64(slots::RET_0, -1);
            call.ret_i64(slots::RET_1, -1);
            write_error_to(call, slots::RET_2, &e.to_string());
        }
    }
    ExternResult::Ok
}

#[cfg(all(feature = "std", not(unix)))]
#[vostd_fn("os", "nativePipe", std)]
fn os_native_pipe(call: &mut ExternCallContext) -> ExternResult {
    call.ret_i64(slots::RET_0, -1);
    call.ret_i64(slots::RET_1, -1);
    write_error_to(call, slots::RET_2, "pipe not supported on this platform");
    ExternResult::Ok
}

// ==================== Chtimes ====================

#[cfg(feature = "std")]
#[vostd_fn("os", "nativeChtimes", std)]
fn os_native_chtimes(call: &mut ExternCallContext) -> ExternResult {
    use std::fs::FileTimes;

    let atime = call.arg_i64(slots::ARG_ATIME);
    let mtime = call.arg_i64(slots::ARG_MTIME);

    let atime_systime = match system_time_from_unix_seconds(atime) {
        Ok(time) => time,
        Err(error) => {
            write_io_error(call, slots::RET_0, error);
            return ExternResult::Ok;
        }
    };
    let mtime_systime = match system_time_from_unix_seconds(mtime) {
        Ok(time) => time,
        Err(error) => {
            write_io_error(call, slots::RET_0, error);
            return ExternResult::Ok;
        }
    };

    let file = match path_arg(call, slots::ARG_NAME, "file name")
        .and_then(|name| open_file_for_times(&name))
    {
        Ok(f) => f,
        Err(e) => {
            write_io_error(call, slots::RET_0, e);
            return ExternResult::Ok;
        }
    };

    let times = FileTimes::new()
        .set_accessed(atime_systime)
        .set_modified(mtime_systime);

    match file.set_times(times) {
        Ok(_) => write_nil_error(call, slots::RET_0),
        Err(e) => write_io_error(call, slots::RET_0, e),
    }
    ExternResult::Ok
}

// ==================== FindProcess / Kill ====================

#[cfg(all(feature = "std", unix))]
#[vostd_fn("os", "nativeFindProcess", std)]
fn os_native_find_process(call: &mut ExternCallContext) -> ExternResult {
    let pid = match checked_i32(call.arg_i64(slots::ARG_PID), "process id") {
        Ok(pid) => pid,
        Err(error) => {
            write_io_error(call, slots::RET_0, error);
            return ExternResult::Ok;
        }
    };

    // Check if process exists by sending signal 0
    match nix::sys::signal::kill(nix::unistd::Pid::from_raw(pid), None) {
        Ok(_) => write_nil_error(call, slots::RET_0),
        Err(e) => write_error_to(call, slots::RET_0, &e.to_string()),
    }
    ExternResult::Ok
}

#[cfg(all(feature = "std", not(unix)))]
#[vostd_fn("os", "nativeFindProcess", std)]
fn os_native_find_process(call: &mut ExternCallContext) -> ExternResult {
    if let Err(error) = checked_i32(call.arg_i64(slots::ARG_PID), "process id") {
        write_io_error(call, slots::RET_0, error);
        return ExternResult::Ok;
    }
    write_error_to(
        call,
        slots::RET_0,
        "FindProcess not supported on this platform",
    );
    ExternResult::Ok
}

#[cfg(all(feature = "std", unix))]
#[vostd_fn("os", "nativeKillProcess", std)]
fn os_native_kill_process(call: &mut ExternCallContext) -> ExternResult {
    let pid = match checked_i32(call.arg_i64(slots::ARG_PID), "process id") {
        Ok(pid) => pid,
        Err(error) => {
            write_io_error(call, slots::RET_0, error);
            return ExternResult::Ok;
        }
    };
    let sig = match checked_i32(call.arg_i64(slots::ARG_SIG), "signal") {
        Ok(signal) => signal,
        Err(error) => {
            write_io_error(call, slots::RET_0, error);
            return ExternResult::Ok;
        }
    };

    let signal = match nix::sys::signal::Signal::try_from(sig) {
        Ok(s) => Some(s),
        Err(_) => {
            write_error_to(call, slots::RET_0, "invalid signal");
            return ExternResult::Ok;
        }
    };

    match nix::sys::signal::kill(nix::unistd::Pid::from_raw(pid), signal) {
        Ok(_) => write_nil_error(call, slots::RET_0),
        Err(e) => write_error_to(call, slots::RET_0, &e.to_string()),
    }
    ExternResult::Ok
}

#[cfg(all(feature = "std", not(unix)))]
#[vostd_fn("os", "nativeKillProcess", std)]
fn os_native_kill_process(call: &mut ExternCallContext) -> ExternResult {
    if let Err(error) = checked_i32(call.arg_i64(slots::ARG_PID), "process id") {
        write_io_error(call, slots::RET_0, error);
        return ExternResult::Ok;
    }
    if let Err(error) = checked_i32(call.arg_i64(slots::ARG_SIG), "signal") {
        write_io_error(call, slots::RET_0, error);
        return ExternResult::Ok;
    }
    write_error_to(
        call,
        slots::RET_0,
        "KillProcess not supported on this platform",
    );
    ExternResult::Ok
}

#[cfg(feature = "std")]
vo_ffi_macro::vostd_register!("os":
    getOsErrors, getOsConsts, getPathSeparators,
    blocking_fileRead, blocking_fileWrite, blocking_fileReadAt, blocking_fileWriteAt, fileSeek, fileClose, fileSync, fileStat, fileTruncate,
    openFile, nativeMkdir, nativeMkdirAll, nativeRemove, nativeRemoveAll, nativeRename,
    nativeStat, nativeLstat, nativeReadDir, nativeChmod, nativeChown, nativeSymlink, nativeReadlink,
    nativeLink, nativeTruncate, nativeReadFile, nativeWriteFile,
    nativeGetenv, nativeSetenv, nativeUnsetenv, nativeEnviron, nativeLookupEnv, nativeClearenv, nativeExpandEnv,
    nativeGetwd, nativeChdir, nativeUserHomeDir, nativeUserCacheDir, nativeUserConfigDir, nativeTempDir,
    nativeGetpid, nativeGetppid, nativeGetuid, nativeGeteuid, nativeGetgid, nativeGetegid,
    nativeExit, nativeGetArgs, nativeIsTerminal, nativeHostname, nativeExecutable, nativeCreateTemp, nativeMkdirTemp,
    nativePipe, nativeChtimes, nativeFindProcess, nativeKillProcess,
);

#[cfg(not(feature = "std"))]
pub fn register_externs(
    _registry: &mut vo_runtime::ffi::ExternRegistry,
    _externs: &[vo_runtime::bytecode::ExternDef],
) -> Result<(), vo_runtime::ffi::ExternContractError> {
    // No-op: WASM platform externs are registered by vo-web-runtime-wasm
    Ok(())
}

#[cfg(all(test, feature = "std"))]
mod tests {
    use std::io;
    use std::path::{Path, PathBuf};
    use std::sync::atomic::{AtomicU64, Ordering};

    use super::*;

    static NEXT_TEMP_DIR: AtomicU64 = AtomicU64::new(0);

    #[test]
    fn exit_status_requires_an_exact_signed_32_bit_value() {
        assert_eq!(checked_i32(i32::MIN as i64, "status").unwrap(), i32::MIN);
        assert_eq!(checked_i32(i32::MAX as i64, "status").unwrap(), i32::MAX);
        assert!(checked_i32(i32::MIN as i64 - 1, "status").is_err());
        assert!(checked_i32(i32::MAX as i64 + 1, "status").is_err());
    }

    #[test]
    fn file_registry_lock_recovers_after_poisoning() {
        let table = std::sync::Arc::new(std::sync::Mutex::new(Vec::new()));
        let poisoner = std::sync::Arc::clone(&table);
        let result = std::thread::spawn(move || {
            let _guard = poisoner.lock().expect("fresh file registry lock");
            panic!("poison file registry lock");
        })
        .join();
        assert!(result.is_err());

        lock_recover(&table).push(11);
        assert_eq!(lock_recover(&table).as_slice(), [11]);
    }

    struct TempDir(PathBuf);

    impl TempDir {
        fn new(label: &str) -> Self {
            let sequence = NEXT_TEMP_DIR.fetch_add(1, Ordering::Relaxed);
            let path = std::env::temp_dir().join(format!(
                "volang-stdlib-os-{label}-{}-{sequence}",
                std::process::id()
            ));
            fs::create_dir(&path).expect("create isolated os test directory");
            Self(path)
        }

        fn path(&self) -> &Path {
            &self.0
        }
    }

    impl Drop for TempDir {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.0);
        }
    }

    #[test]
    fn file_handles_follow_vm_lifetime_and_explicit_cleanup_is_idempotent() {
        let root = TempDir::new("vm-file-cleanup");
        let path = root.path().join("owned.txt");
        let mut owner = IoRuntime::new().expect("owner I/O runtime");
        let handle = register_file(
            &mut owner,
            File::create(&path).expect("create owned file"),
            false,
        )
        .expect("register owned file");
        assert!(lock_recover(&FILE_HANDLES).contains_key(&handle));
        drop(owner);
        assert!(
            !lock_recover(&FILE_HANDLES).contains_key(&handle),
            "VM drop must remove its unclosed file handle"
        );

        let mut explicit = IoRuntime::new().expect("explicit-close I/O runtime");
        let handle = register_file(
            &mut explicit,
            File::open(&path).expect("open owned file"),
            false,
        )
        .expect("register explicitly closed file");
        let cleanup_token = FILE_HANDLES
            .lock()
            .unwrap()
            .get(&handle)
            .expect("registered file")
            .cleanup_token;
        discard_file(&mut explicit, handle);
        assert!(!explicit.disarm_resource_cleanup(cleanup_token));
        assert!(!lock_recover(&FILE_HANDLES).contains_key(&handle));
        drop(explicit);
        assert!(!lock_recover(&FILE_HANDLES).contains_key(&handle));
    }

    #[test]
    fn collect_until_io_error_returns_the_successful_prefix_and_first_error() {
        let values = [
            Ok(3),
            Ok(1),
            Err(io::Error::new(io::ErrorKind::PermissionDenied, "denied")),
            Ok(2),
        ];
        let (collected, error) = collect_until_io_error(values);

        assert_eq!(collected, [3, 1]);
        assert_eq!(
            error.expect("first iterator error").kind(),
            io::ErrorKind::PermissionDenied
        );
    }

    #[test]
    fn native_read_dir_sorts_names_and_preserves_symlink_type() {
        let root = TempDir::new("read-dir");
        fs::write(root.path().join("zeta"), b"z").expect("create zeta");
        fs::write(root.path().join("alpha"), b"a").expect("create alpha");
        fs::write(root.path().join("middle"), b"m").expect("create middle");
        #[cfg(unix)]
        symlink("alpha", root.path().join("link")).expect("create symlink");

        let (entries, error) =
            read_native_dir_entries(fs::read_dir(root.path()).expect("read test directory"));
        assert!(
            error.is_none(),
            "unexpected directory iteration error: {error:?}"
        );
        let names = entries
            .iter()
            .map(|entry| entry.0.as_slice())
            .collect::<Vec<_>>();
        #[cfg(unix)]
        assert_eq!(
            names,
            [&b"alpha"[..], &b"link"[..], &b"middle"[..], &b"zeta"[..]]
        );
        #[cfg(not(unix))]
        assert_eq!(names, [&b"alpha"[..], &b"middle"[..], &b"zeta"[..]]);

        #[cfg(unix)]
        {
            let link = entries
                .iter()
                .find(|entry| entry.0 == b"link")
                .expect("link entry");
            assert!(!link.1, "a symlink to a file is not a directory entry");
            assert_eq!(link.2 & MODE_SYMLINK, MODE_SYMLINK);
            assert_eq!(link.2 & MODE_DIR, 0);
        }

        #[cfg(all(unix, not(target_vendor = "apple")))]
        {
            use std::os::unix::ffi::OsStringExt as _;
            let raw_name = std::ffi::OsString::from_vec(b"invalid-\xff".to_vec());
            if let Err(error) = fs::write(root.path().join(&raw_name), b"invalid") {
                if error.kind() == io::ErrorKind::PermissionDenied {
                    eprintln!("host sandbox does not permit non-UTF-8 file names: {error}");
                    return;
                }
                panic!("create real non-UTF-8 directory entry: {error}");
            }
            let (with_raw_name, error) =
                read_native_dir_entries(fs::read_dir(root.path()).expect("read invalid-name dir"));
            assert!(
                error.is_none(),
                "raw Unix name must be representable: {error:?}"
            );
            assert!(
                with_raw_name.windows(2).all(|pair| pair[0].0 <= pair[1].0),
                "byte names must remain sorted"
            );
            assert!(
                with_raw_name.iter().any(|entry| entry.0 == b"invalid-\xff"),
                "raw Unix name must round-trip"
            );
        }
    }

    #[test]
    fn signed_unix_time_helpers_floor_pre_epoch_subseconds() {
        use std::time::{Duration, UNIX_EPOCH};

        assert_eq!(system_time_to_unix_seconds(UNIX_EPOCH), 0);
        assert_eq!(
            system_time_to_unix_seconds(UNIX_EPOCH + Duration::new(3, 999_999_999)),
            3
        );
        assert_eq!(
            system_time_to_unix_seconds(UNIX_EPOCH - Duration::new(0, 1)),
            -1
        );
        assert_eq!(
            system_time_to_unix_seconds(UNIX_EPOCH - Duration::new(1, 0)),
            -1
        );
        assert_eq!(
            system_time_to_unix_seconds(UNIX_EPOCH - Duration::new(1, 1)),
            -2
        );
        for seconds in [-2, -1, 0, 1, 2] {
            assert_eq!(
                system_time_to_unix_seconds(
                    system_time_from_unix_seconds(seconds).expect("representable timestamp")
                ),
                seconds
            );
        }
        match system_time_from_unix_seconds(i64::MIN) {
            Ok(time) => assert_eq!(system_time_to_unix_seconds(time), i64::MIN),
            Err(error) => assert_eq!(error.kind(), io::ErrorKind::InvalidInput),
        }
    }

    #[test]
    fn checked_integer_boundaries_reject_aliasing_and_negative_sizes() {
        assert_eq!(checked_i32(i32::MIN as i64, "value").unwrap(), i32::MIN);
        assert_eq!(checked_i32(i32::MAX as i64, "value").unwrap(), i32::MAX);
        assert!(checked_i32(i32::MAX as i64 + 1, "value").is_err());
        assert!(checked_i32(i32::MIN as i64 - 1, "value").is_err());
        assert_eq!(checked_file_size(0).unwrap(), 0);
        assert!(checked_file_size(-1).is_err());
        assert_eq!(checked_chown_id(-1, "uid").unwrap(), u32::MAX);
        assert!(checked_chown_id(-2, "uid").is_err());
        assert!(next_file_handle(i32::MAX).is_err());
    }

    #[test]
    fn open_flag_validation_rejects_unknown_and_invalid_combinations() {
        for flag in [
            1 << 20,
            O_EXCL as i32,
            O_RDONLY as i32 | O_TRUNC as i32,
            O_WRONLY as i32 | O_RDWR as i32,
        ] {
            assert!(validate_open_flags(flag).is_err(), "flag {flag:#x}");
        }
        for flag in [
            O_RDONLY as i32,
            O_RDONLY as i32 | O_CREATE as i32,
            O_RDONLY as i32 | O_APPEND as i32,
            O_WRONLY as i32 | O_CREATE as i32 | O_TRUNC as i32,
            O_RDWR as i32 | O_SYNC as i32,
        ] {
            validate_open_flags(flag).expect("documented flag combination");
        }
        validate_write_at_mode(false).expect("positioned write on a regular file");
        assert_eq!(
            validate_write_at_mode(true).unwrap_err().kind(),
            io::ErrorKind::InvalidInput
        );
    }

    #[test]
    fn remove_helpers_choose_one_operation_and_remove_all_ignores_absence() {
        let root = TempDir::new("remove");
        let file = root.path().join("file");
        fs::write(&file, b"data").unwrap();
        remove_path(&file).expect("remove regular file");
        assert!(!file.exists());

        let nonempty = root.path().join("nonempty");
        fs::create_dir(&nonempty).unwrap();
        fs::write(nonempty.join("child"), b"data").unwrap();
        assert!(remove_path(&nonempty).is_err());
        assert!(nonempty.join("child").exists());
        remove_all_path(&nonempty).expect("recursive directory removal");
        remove_all_path(&nonempty).expect("missing path is successful");

        let standalone = root.path().join("standalone");
        fs::write(&standalone, b"data").unwrap();
        remove_all_path(&standalone).expect("RemoveAll accepts a file");

        #[cfg(unix)]
        {
            let target = root.path().join("target");
            fs::create_dir(&target).unwrap();
            let link = root.path().join("dir-link");
            symlink(&target, &link).unwrap();
            remove_path(&link).expect("remove symlink as a file-like entry");
            assert!(target.is_dir());
        }
    }

    #[test]
    fn environment_validation_and_expansion_preserve_bytes_without_panics() {
        assert!(validate_env_key(b"").is_err());
        assert!(validate_env_key(b"BAD=KEY").is_err());
        assert!(validate_env_key(b"BAD\0KEY").is_err());
        assert!(validate_env_value(b"BAD\0VALUE").is_err());

        let key = format!(
            "VOLANG_OS_EXPAND_TEST_{}_{}",
            std::process::id(),
            NEXT_TEMP_DIR.fetch_add(1, Ordering::Relaxed)
        );
        set_env_checked(key.as_bytes(), "值😀".as_bytes()).expect("set unique test variable");
        assert_eq!(expand_env_bytes("中文😀".as_bytes()), "中文😀".as_bytes());
        assert_eq!(expand_env_bytes("$é".as_bytes()), "$é".as_bytes());
        assert_eq!(expand_env_bytes(b"before-${"), b"before-");
        assert_eq!(
            expand_env_bytes(format!("x${{{key}}}y").as_bytes()),
            "x值😀y".as_bytes()
        );
        unset_env_checked(key.as_bytes()).expect("unset unique test variable");

        assert_eq!(
            environment_entry("KEY".into(), "值😀".into()).unwrap(),
            "KEY=值😀".as_bytes()
        );
        #[cfg(unix)]
        {
            use std::os::unix::ffi::OsStringExt as _;

            let invalid = std::ffi::OsString::from_vec(b"bad-\xff".to_vec());
            assert_eq!(
                environment_entry(invalid.clone(), "value".into()).unwrap(),
                b"bad-\xff=value"
            );
            assert_eq!(
                environment_entry("KEY".into(), invalid).unwrap(),
                b"KEY=bad-\xff"
            );

            let mut raw_key = key.as_bytes().to_vec();
            raw_key.push(0xff);
            let raw_value = b"value-\xfe";
            set_env_checked(&raw_key, raw_value).expect("set raw Unix environment value");
            assert_eq!(lookup_env_bytes(&raw_key).as_deref(), Some(&raw_value[..]));
            unset_env_checked(&raw_key).expect("unset raw Unix environment value");
        }

        assert_eq!(
            file_info_name(Path::new("directory/file")).unwrap(),
            b"file"
        );
        assert_eq!(
            file_info_name(Path::new("directory/file/")).unwrap(),
            b"file"
        );
        #[cfg(unix)]
        assert_eq!(file_info_name(Path::new("/")).unwrap(), b"/");
    }

    #[test]
    fn temp_patterns_use_the_last_star_and_reject_path_escape() {
        assert_eq!(
            temp_pattern_parts(b"pre*middle*suffix").unwrap(),
            (&b"pre*middle"[..], &b"suffix"[..])
        );
        assert_eq!(
            temp_pattern_parts(b"plain").unwrap(),
            (&b"plain"[..], &b""[..])
        );
        assert!(temp_pattern_parts(b"../escape*").is_err());
        assert!(temp_pattern_parts(b"dir\\escape*").is_err());
    }

    #[cfg(unix)]
    fn unix_permissions(path: &Path) -> u32 {
        fs::symlink_metadata(path).unwrap().permissions().mode() & MODE_PERM
    }

    #[cfg(unix)]
    #[test]
    fn unix_creation_modes_are_atomic_umask_aware_and_preserve_existing_modes() {
        let root = TempDir::new("creation-mode");
        let probe = root.path().join("umask-probe");
        drop(
            open_file_with_mode(
                &probe,
                O_WRONLY as i32 | O_CREATE as i32 | O_EXCL as i32,
                0o777,
            )
            .expect("create umask probe"),
        );
        let umask_allowed = unix_permissions(&probe);

        let readonly_create = root.path().join("readonly-create");
        let mut readonly =
            open_file_with_mode(&readonly_create, O_RDONLY as i32 | O_CREATE as i32, 0o640)
                .expect("O_RDONLY|O_CREATE is valid on Unix");
        assert_eq!(unix_permissions(&readonly_create), 0o640 & umask_allowed);
        assert!(readonly.write_all(b"must fail").is_err());

        let append_readonly = root.path().join("append-readonly");
        fs::write(&append_readonly, b"original").unwrap();
        let mut append_handle =
            open_file_with_mode(&append_readonly, O_RDONLY as i32 | O_APPEND as i32, 0)
                .expect("open read-only append descriptor");
        assert!(append_handle.write_all(b"x").is_err());
        assert_eq!(fs::read(&append_readonly).unwrap(), b"original");

        let existing = root.path().join("existing");
        fs::write(&existing, b"old").unwrap();
        fs::set_permissions(&existing, fs::Permissions::from_mode(0o600)).unwrap();
        write_file_with_mode(&existing, b"new", 0o777).expect("rewrite existing file");
        assert_eq!(unix_permissions(&existing), 0o600);
        drop(open_file_with_mode(&existing, O_RDONLY as i32 | O_CREATE as i32, 0o777).unwrap());
        assert_eq!(unix_permissions(&existing), 0o600);

        let created = root.path().join("write-created");
        write_file_with_mode(&created, b"new", 0o620).unwrap();
        assert_eq!(unix_permissions(&created), 0o620 & umask_allowed);

        let directory = root.path().join("directory");
        create_dir_with_mode(&directory, 0o750).unwrap();
        assert_eq!(unix_permissions(&directory), 0o750 & umask_allowed);
        fs::set_permissions(&directory, fs::Permissions::from_mode(0o700)).unwrap();
        create_dir_all_with_mode(&directory, 0o777).unwrap();
        assert_eq!(unix_permissions(&directory), 0o700);

        let nested = root.path().join("parent/child");
        create_dir_all_with_mode(&nested, 0o751).unwrap();
        assert_eq!(
            unix_permissions(&root.path().join("parent")),
            0o751 & umask_allowed
        );
        assert_eq!(unix_permissions(&nested), 0o751 & umask_allowed);

        let sync_path = root.path().join("sync");
        let sync_file = open_file_with_mode(
            &sync_path,
            O_WRONLY as i32 | O_CREATE as i32 | O_SYNC as i32,
            0o600,
        )
        .expect("open O_SYNC file");
        let flags = unsafe { libc::fcntl(sync_file.as_raw_fd(), libc::F_GETFL) };
        assert_ne!(flags, -1);
        assert_ne!(flags & libc::O_SYNC, 0);

        let (temp_file, temp_path) = create_temp_file_in(root.path(), b"file-*-tail").unwrap();
        drop(temp_file);
        assert_eq!(unix_permissions(&temp_path), 0o600 & umask_allowed);
        let temp_dir = create_temp_dir_in(root.path(), b"dir-*-tail").unwrap();
        assert_eq!(unix_permissions(&temp_dir), 0o700 & umask_allowed);
    }

    #[cfg(unix)]
    #[test]
    fn readlink_preserves_non_utf8_targets() {
        use std::os::unix::ffi::OsStringExt as _;

        let root = TempDir::new("readlink-utf8");
        let link = root.path().join("link");
        let target = std::ffi::OsString::from_vec(b"target-\xff".to_vec());
        symlink(&target, &link).unwrap();
        let target = fs::read_link(&link).unwrap();
        assert_eq!(
            path_bytes(target, "symbolic link target").unwrap(),
            b"target-\xff"
        );
    }

    #[cfg(unix)]
    #[test]
    fn unix_file_mode_maps_special_bits_fifo_and_character_device() {
        use std::ffi::CString;
        use std::os::unix::ffi::OsStrExt as _;

        let root = TempDir::new("file-mode");
        let special = root.path().join("special");
        fs::write(&special, b"").expect("create special-mode file");
        fs::set_permissions(&special, fs::Permissions::from_mode(0o754))
            .expect("set file permissions");
        let special_mode = metadata_to_file_mode(&fs::symlink_metadata(&special).unwrap());
        assert_eq!(special_mode & MODE_PERM, 0o754);
        assert_eq!(special_mode & MODE_IRREGULAR, 0);

        assert_eq!(
            unix_special_mode_bits(UNIX_MODE_SETUID | UNIX_MODE_SETGID | UNIX_MODE_STICKY),
            MODE_SETUID | MODE_SETGID | MODE_STICKY
        );

        let sticky = root.path().join("sticky");
        fs::create_dir(&sticky).expect("create sticky directory");
        fs::set_permissions(&sticky, fs::Permissions::from_mode(0o770))
            .expect("set directory permissions");
        let sticky_mode = metadata_to_file_mode(&fs::symlink_metadata(&sticky).unwrap());
        assert_eq!(sticky_mode & MODE_DIR, MODE_DIR);
        assert_eq!(sticky_mode & MODE_PERM, 0o770);

        let fifo = root.path().join("fifo");
        let fifo_path = CString::new(fifo.as_os_str().as_bytes()).expect("NUL-free FIFO path");
        let status = unsafe { libc::mkfifo(fifo_path.as_ptr(), 0o640) };
        assert_eq!(status, 0, "mkfifo failed: {}", io::Error::last_os_error());
        fs::set_permissions(&fifo, fs::Permissions::from_mode(0o640))
            .expect("set FIFO permissions");
        let fifo_mode = metadata_to_file_mode(&fs::symlink_metadata(&fifo).unwrap());
        assert_eq!(fifo_mode & MODE_NAMED_PIPE, MODE_NAMED_PIPE);
        assert_eq!(fifo_mode & MODE_PERM, 0o640);
        assert_eq!(fifo_mode & (MODE_DEVICE | MODE_SOCKET | MODE_IRREGULAR), 0);

        let null_mode = metadata_to_file_mode(&fs::symlink_metadata("/dev/null").unwrap());
        assert_eq!(null_mode & MODE_DEVICE, MODE_DEVICE);
        assert_eq!(null_mode & MODE_CHAR_DEVICE, MODE_CHAR_DEVICE);
        assert_eq!(null_mode & MODE_IRREGULAR, 0);
    }

    #[test]
    fn native_read_dir_uses_canonical_typed_logical_elements() {
        let source = include_str!("os.rs");
        let production_source = source
            .split("#[cfg(all(test, feature = \"std\"))]")
            .next()
            .expect("production os implementation");
        let function = source
            .split("fn os_read_dir(")
            .nth(1)
            .expect("nativeReadDir implementation")
            .split("fn os_chmod(")
            .next()
            .expect("nativeReadDir section");

        for required in [
            ".find(|meta| meta.name == \"os.DirEntry\")",
            ".map(|meta| meta.underlying_meta)",
            "name.type_info.value_kind() != ValueKind::String",
            "is_dir.type_info.value_kind() != ValueKind::Bool",
            "mode.type_info.value_kind() != ValueKind::Uint32",
            "value.fill(0)",
            "typed_write_barrier_by_meta(owner, &value, elem_meta)",
            "slice::write_logical_slots(result, i, &value)",
        ] {
            assert!(
                function.contains(required),
                "nativeReadDir must preserve {required}"
            );
        }
        assert!(
            !function.contains("slice::set(result, base"),
            "nativeReadDir must address DirEntry values by logical element index"
        );
        let value_buffer = function
            .find("let mut value = vec![0u64; elem_slots]")
            .expect("reusable logical-slot buffer");
        let entry_loop = function
            .find("for (i, (entry_name, is_dir, mode))")
            .expect("directory-entry construction loop");
        assert!(
            value_buffer < entry_loop,
            "nativeReadDir must reuse one cleared logical-slot buffer across directory entries"
        );
        for required in [
            "collect_until_io_error(entries.map(|entry| entry.and_then(decode_native_dir_entry)))",
            "entries.sort_by(|left, right| left.0.cmp(&right.0))",
            "fs::symlink_metadata(entry.path())?",
            "os_name_bytes(entry.file_name())?",
            "call.alloc_string_bytes(entry_name)",
            "write_io_error(call, slots::RET_1, error)",
        ] {
            assert!(
                production_source.contains(required),
                "nativeReadDir must preserve {required}"
            );
        }
        assert!(
            !production_source.contains("entries.flatten()"),
            "nativeReadDir must retain directory iteration errors"
        );
    }

    #[test]
    fn file_info_and_environment_paths_preserve_raw_bytes() {
        let source = include_str!("os.rs");
        let production_source = source
            .split("#[cfg(all(test, feature = \"std\"))]")
            .next()
            .expect("production os implementation");
        for required in [
            ".find(|meta| meta.name == \"os.FileInfo\")",
            "usize::from(meta.slot_count()) != FILE_INFO_ABI_SLOTS",
            "field(\"name\", ValueKind::String)",
            "field(\"size\", ValueKind::Int64)",
            "field(\"mode\", ValueKind::Uint32)",
            "field(\"modTime\", ValueKind::Int64)",
            "field(\"isDir\", ValueKind::Bool)",
            "Gc::write_slot(file_info, layout.name",
            "let path = match path_arg(call, slots::ARG_NAME, \"file name\")",
            "call.alloc_string_bytes(name)",
            "std::env::vars_os()",
            "call.ret_string_bytes_slice(slots::RET_0, &vars)",
        ] {
            assert!(
                production_source.contains(required),
                "os native ABI must preserve {required}"
            );
        }
        assert!(
            !production_source.contains("Gc::write_slot(file_info, 0"),
            "FileInfo construction must not depend on hard-coded field offsets"
        );
        assert!(
            !production_source.contains("std::env::vars()"),
            "Environ must not panic when the host contains a non-UTF-8 entry"
        );
    }
}
