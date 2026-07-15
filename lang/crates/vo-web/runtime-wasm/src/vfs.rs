//! Virtual File System - checked Rust bindings to the browser VFS host.
//!
//! The JavaScript implementation owns storage and path semantics. This layer
//! validates every returned tuple so a missing or incompatible host binding
//! becomes a normal I/O error instead of a false successful result.

use wasm_bindgen::{prelude::*, JsCast};

pub const MAX_VFS_FILE_BYTES: usize = 256 * 1024 * 1024;
pub const MAX_VFS_IO_BYTES: usize = 64 * 1024 * 1024;
const MAX_VFS_DIRECTORY_ENTRIES: u32 = 100_000;
const MAX_VFS_PATH_DEPTH: usize = 256;
const MAX_VFS_PATH_UTF16_UNITS: usize = 4096;
const MAX_VFS_NAME_UTF16_UNITS: usize = 255;
const VFS_MUTABLE_MODE_BITS: u32 = 0o777 | (1 << 23) | (1 << 22) | (1 << 20);
const INVALID_HOST_RESPONSE: &str = "invalid browser VFS host response";

// =============================================================================
// JS imports
// =============================================================================

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = window, js_name = _vfsOpenFile)]
    fn js_open_file(path: &str, flags: i32, mode: u32) -> JsValue;

    #[wasm_bindgen(js_namespace = window, js_name = _vfsRead)]
    fn js_read(fd: i32, length: u32) -> JsValue;

    #[wasm_bindgen(js_namespace = window, js_name = _vfsWrite)]
    fn js_write(fd: i32, data: &[u8]) -> JsValue;

    #[wasm_bindgen(js_namespace = window, js_name = _vfsReadAt)]
    fn js_read_at(fd: i32, length: u32, offset: i64) -> JsValue;

    #[wasm_bindgen(js_namespace = window, js_name = _vfsWriteAt)]
    fn js_write_at(fd: i32, data: &[u8], offset: i64) -> JsValue;

    #[wasm_bindgen(js_namespace = window, js_name = _vfsSeek)]
    fn js_seek(fd: i32, offset: i64, whence: i32) -> JsValue;

    #[wasm_bindgen(js_namespace = window, js_name = _vfsClose)]
    fn js_close(fd: i32) -> JsValue;

    #[wasm_bindgen(js_namespace = window, js_name = _vfsSync)]
    fn js_sync(fd: i32) -> JsValue;

    #[wasm_bindgen(js_namespace = window, js_name = _vfsFstat)]
    fn js_fstat(fd: i32) -> JsValue;

    #[wasm_bindgen(js_namespace = window, js_name = _vfsFtruncate)]
    fn js_ftruncate(fd: i32, size: i64) -> JsValue;

    #[wasm_bindgen(js_namespace = window, js_name = _vfsMkdir)]
    fn js_mkdir(path: &str, mode: u32) -> JsValue;

    #[wasm_bindgen(js_namespace = window, js_name = _vfsMkdirAll)]
    fn js_mkdir_all(path: &str, mode: u32) -> JsValue;

    #[wasm_bindgen(js_namespace = window, js_name = _vfsRemove)]
    fn js_remove(path: &str) -> JsValue;

    #[wasm_bindgen(js_namespace = window, js_name = _vfsRemoveAll)]
    fn js_remove_all(path: &str) -> JsValue;

    #[wasm_bindgen(js_namespace = window, js_name = _vfsRename)]
    fn js_rename(old_path: &str, new_path: &str) -> JsValue;

    #[wasm_bindgen(js_namespace = window, js_name = _vfsRenameNoreplace)]
    fn js_rename_noreplace(old_path: &str, new_path: &str) -> JsValue;

    #[wasm_bindgen(js_namespace = window, js_name = _vfsStat)]
    fn js_stat(path: &str) -> JsValue;

    #[wasm_bindgen(js_namespace = window, js_name = _vfsReadDir)]
    fn js_read_dir(path: &str) -> JsValue;

    #[wasm_bindgen(js_namespace = window, js_name = _vfsChmod)]
    fn js_chmod(path: &str, mode: u32) -> JsValue;

    #[wasm_bindgen(js_namespace = window, js_name = _vfsTruncate)]
    fn js_truncate(path: &str, size: i64) -> JsValue;

    #[wasm_bindgen(js_namespace = window, js_name = _vfsGetwd)]
    fn js_getwd() -> JsValue;

    #[wasm_bindgen(js_namespace = window, js_name = _vfsChdir)]
    fn js_chdir(path: &str) -> JsValue;

    #[wasm_bindgen(js_namespace = window, js_name = _vfsReadFileLimited)]
    fn js_read_file_limited(path: &str, max_bytes: u32) -> JsValue;

    #[wasm_bindgen(js_namespace = window, js_name = _vfsWriteFile)]
    fn js_write_file(path: &str, data: &[u8], mode: u32) -> JsValue;
}

// =============================================================================
// Checked host-result decoding
// =============================================================================

fn tuple_value(result: &JsValue, index: u32) -> Result<JsValue, String> {
    if !js_sys::Array::is_array(result) {
        return Err(INVALID_HOST_RESPONSE.to_string());
    }
    js_sys::Reflect::get_u32(result, index).map_err(|_| INVALID_HOST_RESPONSE.to_string())
}

fn tuple_error(result: &JsValue, index: u32) -> Result<Option<String>, String> {
    let value = tuple_value(result, index)?;
    if value.is_null() {
        return Ok(None);
    }
    value
        .as_string()
        .map(Some)
        .ok_or_else(|| INVALID_HOST_RESPONSE.to_string())
}

fn single_error(result: &JsValue) -> Option<String> {
    if result.is_null() {
        return None;
    }
    result
        .as_string()
        .or_else(|| Some(INVALID_HOST_RESPONSE.to_string()))
}

fn finite_integer(value: &JsValue) -> Result<f64, String> {
    let value = value
        .as_f64()
        .ok_or_else(|| INVALID_HOST_RESPONSE.to_string())?;
    if !value.is_finite() || value.fract() != 0.0 || value.abs() > 9_007_199_254_740_991.0 {
        return Err(INVALID_HOST_RESPONSE.to_string());
    }
    Ok(value)
}

fn tuple_i32(result: &JsValue, index: u32) -> Result<i32, String> {
    let value = finite_integer(&tuple_value(result, index)?)?;
    if value < f64::from(i32::MIN) || value > f64::from(i32::MAX) {
        return Err(INVALID_HOST_RESPONSE.to_string());
    }
    Ok(value as i32)
}

fn tuple_i64(result: &JsValue, index: u32) -> Result<i64, String> {
    Ok(finite_integer(&tuple_value(result, index)?)? as i64)
}

fn tuple_u32(result: &JsValue, index: u32) -> Result<u32, String> {
    let value = finite_integer(&tuple_value(result, index)?)?;
    if value < 0.0 || value > f64::from(u32::MAX) {
        return Err(INVALID_HOST_RESPONSE.to_string());
    }
    Ok(value as u32)
}

fn tuple_bool(result: &JsValue, index: u32) -> Result<bool, String> {
    tuple_value(result, index)?
        .as_bool()
        .ok_or_else(|| INVALID_HOST_RESPONSE.to_string())
}

fn tuple_string(result: &JsValue, index: u32) -> Result<String, String> {
    tuple_value(result, index)?
        .as_string()
        .ok_or_else(|| INVALID_HOST_RESPONSE.to_string())
}

fn tuple_bytes(result: &JsValue, index: u32) -> Result<Vec<u8>, String> {
    let value = tuple_value(result, index)?;
    if !value.is_instance_of::<js_sys::Uint8Array>() {
        return Err(INVALID_HOST_RESPONSE.to_string());
    }
    Ok(js_sys::Uint8Array::new(&value).to_vec())
}

fn valid_vfs_name(name: &str, allow_root: bool) -> bool {
    (allow_root && name == "/")
        || (!name.is_empty()
            && name != "."
            && name != ".."
            && !name.contains('/')
            && !name.contains('\0')
            && name.encode_utf16().count() <= MAX_VFS_NAME_UTF16_UNITS)
}

fn valid_canonical_absolute_path(path: &str) -> bool {
    if !path.starts_with('/')
        || path.contains('\0')
        || path.encode_utf16().count() > MAX_VFS_PATH_UTF16_UNITS
    {
        return false;
    }
    if path == "/" {
        return true;
    }
    if path.ends_with('/') {
        return false;
    }
    let mut depth = 0usize;
    for part in path[1..].split('/') {
        if !valid_vfs_name(part, false) {
            return false;
        }
        depth += 1;
        if depth > MAX_VFS_PATH_DEPTH {
            return false;
        }
    }
    true
}

fn byte_result(result: &JsValue, data_index: u32, error_index: u32) -> (Vec<u8>, Option<String>) {
    let error = match tuple_error(result, error_index) {
        Ok(error) => error,
        Err(error) => return (Vec::new(), Some(error)),
    };
    let value = match tuple_value(result, data_index) {
        Ok(value) => value,
        Err(error) => return (Vec::new(), Some(error)),
    };
    if value.is_null() && error.is_some() {
        return (Vec::new(), error);
    }
    match tuple_bytes(result, data_index) {
        Ok(data) => (data, error),
        Err(error) => (Vec::new(), Some(error)),
    }
}

fn decoded_or_host_error<T>(
    decoded: Result<T, String>,
    error: Result<Option<String>, String>,
) -> (Option<T>, Option<String>) {
    match (decoded, error) {
        (Ok(value), Ok(error)) => (Some(value), error),
        (Err(error), _) | (_, Err(error)) => (None, Some(error)),
    }
}

// =============================================================================
// Public API used by the WASM stdlib and compiler
// =============================================================================

pub fn open_file(path: &str, flags: i32, mode: u32) -> (i32, Option<String>) {
    let result = js_open_file(path, flags, mode);
    let (fd, error) = decoded_or_host_error(tuple_i32(&result, 0), tuple_error(&result, 1));
    match error {
        Some(error) => (fd.unwrap_or(-1), Some(error)),
        None => match fd {
            Some(fd) if fd >= 3 => (fd, None),
            _ => (-1, Some(INVALID_HOST_RESPONSE.to_string())),
        },
    }
}

pub fn read(fd: i32, length: u32) -> (Vec<u8>, Option<String>) {
    let result = js_read(fd, length);
    let (data, error) = byte_result(&result, 0, 1);
    if data.len() > length as usize {
        return (Vec::new(), Some(INVALID_HOST_RESPONSE.to_string()));
    }
    (data, error)
}

pub fn write(fd: i32, data: &[u8]) -> (i32, Option<String>) {
    let result = js_write(fd, data);
    let (written, error) = decoded_or_host_error(tuple_i32(&result, 0), tuple_error(&result, 1));
    match written {
        Some(written) if written >= 0 && written as usize <= data.len() => (written, error),
        _ => (0, Some(INVALID_HOST_RESPONSE.to_string())),
    }
}

pub fn read_at(fd: i32, length: u32, offset: i64) -> (Vec<u8>, Option<String>) {
    let result = js_read_at(fd, length, offset);
    let (data, error) = byte_result(&result, 0, 1);
    if data.len() > length as usize {
        return (Vec::new(), Some(INVALID_HOST_RESPONSE.to_string()));
    }
    (data, error)
}

pub fn write_at(fd: i32, data: &[u8], offset: i64) -> (i32, Option<String>) {
    let result = js_write_at(fd, data, offset);
    let (written, error) = decoded_or_host_error(tuple_i32(&result, 0), tuple_error(&result, 1));
    match written {
        Some(written) if written >= 0 && written as usize <= data.len() => (written, error),
        _ => (0, Some(INVALID_HOST_RESPONSE.to_string())),
    }
}

pub fn seek(fd: i32, offset: i64, whence: i32) -> (i64, Option<String>) {
    let result = js_seek(fd, offset, whence);
    let (position, error) = decoded_or_host_error(tuple_i64(&result, 0), tuple_error(&result, 1));
    match error {
        Some(error) => (position.unwrap_or(0), Some(error)),
        None => match position {
            Some(position) if position >= 0 => (position, None),
            _ => (0, Some(INVALID_HOST_RESPONSE.to_string())),
        },
    }
}

pub fn close(fd: i32) -> Option<String> {
    single_error(&js_close(fd))
}

pub fn sync(fd: i32) -> Option<String> {
    single_error(&js_sync(fd))
}

pub fn fstat(fd: i32) -> (i64, u32, i64, bool, Option<String>) {
    let result = js_fstat(fd);
    let error = match tuple_error(&result, 4) {
        Ok(error) => error,
        Err(error) => return (0, 0, 0, false, Some(error)),
    };
    let decoded = (
        tuple_i64(&result, 0),
        tuple_u32(&result, 1),
        tuple_i64(&result, 2),
        tuple_bool(&result, 3),
    );
    match decoded {
        (Ok(size), Ok(mode), Ok(mod_time), Ok(is_dir))
            if error.is_some()
                || (size >= 0 && (!is_dir || size == 0) && mode & !VFS_MUTABLE_MODE_BITS == 0) =>
        {
            (size, mode, mod_time, is_dir, error)
        }
        _ => (0, 0, 0, false, Some(INVALID_HOST_RESPONSE.to_string())),
    }
}

pub fn ftruncate(fd: i32, size: i64) -> Option<String> {
    single_error(&js_ftruncate(fd, size))
}

pub fn mkdir(path: &str, mode: u32) -> Option<String> {
    single_error(&js_mkdir(path, mode))
}

pub fn mkdir_all(path: &str, mode: u32) -> Option<String> {
    single_error(&js_mkdir_all(path, mode))
}

pub fn remove(path: &str) -> Option<String> {
    single_error(&js_remove(path))
}

pub fn remove_all(path: &str) -> Option<String> {
    single_error(&js_remove_all(path))
}

pub fn rename(old_path: &str, new_path: &str) -> Option<String> {
    single_error(&js_rename(old_path, new_path))
}

pub fn rename_noreplace(old_path: &str, new_path: &str) -> Option<String> {
    single_error(&js_rename_noreplace(old_path, new_path))
}

pub fn stat(path: &str) -> (String, i64, u32, i64, bool, Option<String>) {
    let result = js_stat(path);
    let error = match tuple_error(&result, 5) {
        Ok(error) => error,
        Err(error) => return (String::new(), 0, 0, 0, false, Some(error)),
    };
    let decoded = (
        tuple_string(&result, 0),
        tuple_i64(&result, 1),
        tuple_u32(&result, 2),
        tuple_i64(&result, 3),
        tuple_bool(&result, 4),
    );
    match decoded {
        (Ok(name), Ok(size), Ok(mode), Ok(mod_time), Ok(is_dir))
            if error.is_some()
                || (size >= 0
                    && (!is_dir || size == 0)
                    && mode & !VFS_MUTABLE_MODE_BITS == 0
                    && valid_vfs_name(&name, true)) =>
        {
            (name, size, mode, mod_time, is_dir, error)
        }
        _ => (
            String::new(),
            0,
            0,
            0,
            false,
            Some(INVALID_HOST_RESPONSE.to_string()),
        ),
    }
}

/// Check path existence without collapsing host/protocol failures into a
/// negative result. Callers may safely use `Ok(false)` solely for the VFS
/// not-exist sentinel.
pub fn exists(path: &str) -> Result<bool, String> {
    let (_, _, _, _, _, error) = stat(path);
    classify_exists(error)
}

fn classify_exists(error: Option<String>) -> Result<bool, String> {
    match error {
        None => Ok(true),
        Some(error) if error == "file does not exist" => Ok(false),
        Some(error) => Err(error),
    }
}

pub fn read_dir(path: &str) -> (Vec<(String, bool, u32)>, Option<String>) {
    let result = js_read_dir(path);
    let error = match tuple_error(&result, 1) {
        Ok(Some(error)) => return (Vec::new(), Some(error)),
        Ok(None) => None,
        Err(error) => return (Vec::new(), Some(error)),
    };
    let Ok(value) = tuple_value(&result, 0) else {
        return (Vec::new(), Some(INVALID_HOST_RESPONSE.to_string()));
    };
    let Some(array) = value.dyn_ref::<js_sys::Array>() else {
        return (Vec::new(), Some(INVALID_HOST_RESPONSE.to_string()));
    };
    if array.length() > MAX_VFS_DIRECTORY_ENTRIES {
        return (
            Vec::new(),
            Some("directory contains too many entries".to_string()),
        );
    }

    let mut entries = Vec::with_capacity(array.length() as usize);
    for index in 0..array.length() {
        let entry_value = array.get(index);
        let Some(entry) = entry_value.dyn_ref::<js_sys::Array>() else {
            return (Vec::new(), Some(INVALID_HOST_RESPONSE.to_string()));
        };
        let Some(name) = entry.get(0).as_string() else {
            return (Vec::new(), Some(INVALID_HOST_RESPONSE.to_string()));
        };
        if !valid_vfs_name(&name, false) {
            return (Vec::new(), Some(INVALID_HOST_RESPONSE.to_string()));
        }
        let Some(is_dir) = entry.get(1).as_bool() else {
            return (Vec::new(), Some(INVALID_HOST_RESPONSE.to_string()));
        };
        let Ok(mode) = finite_integer(&entry.get(2)) else {
            return (Vec::new(), Some(INVALID_HOST_RESPONSE.to_string()));
        };
        if mode < 0.0 || mode > f64::from(u32::MAX) || (mode as u32) & !VFS_MUTABLE_MODE_BITS != 0 {
            return (Vec::new(), Some(INVALID_HOST_RESPONSE.to_string()));
        }
        entries.push((name, is_dir, mode as u32));
    }
    entries.sort_by(|left, right| left.0.as_bytes().cmp(right.0.as_bytes()));
    if entries.windows(2).any(|pair| pair[0].0 == pair[1].0) {
        return (Vec::new(), Some(INVALID_HOST_RESPONSE.to_string()));
    }
    (entries, error)
}

pub fn chmod(path: &str, mode: u32) -> Option<String> {
    single_error(&js_chmod(path, mode))
}

pub fn truncate(path: &str, size: i64) -> Option<String> {
    single_error(&js_truncate(path, size))
}

pub fn getwd() -> (String, Option<String>) {
    let result = js_getwd();
    let (path, error) = decoded_or_host_error(tuple_string(&result, 0), tuple_error(&result, 1));
    match error {
        Some(error) => (path.unwrap_or_default(), Some(error)),
        None => match path {
            Some(path) if valid_canonical_absolute_path(&path) => (path, None),
            _ => (String::new(), Some(INVALID_HOST_RESPONSE.to_string())),
        },
    }
}

pub fn chdir(path: &str) -> Option<String> {
    single_error(&js_chdir(path))
}

pub fn read_file(path: &str) -> (Vec<u8>, Option<String>) {
    read_file_limited(path, MAX_VFS_FILE_BYTES)
}

pub fn read_file_limited(path: &str, max_bytes: usize) -> (Vec<u8>, Option<String>) {
    let effective_limit = max_bytes.min(MAX_VFS_FILE_BYTES);
    let limit = u32::try_from(effective_limit).expect("browser VFS file limit fits u32");
    let result = js_read_file_limited(path, limit);
    let (data, error) = byte_result(&result, 0, 1);
    if data.len() > effective_limit {
        return (Vec::new(), Some(INVALID_HOST_RESPONSE.to_string()));
    }
    (data, error)
}

pub fn write_file(path: &str, data: &[u8], mode: u32) -> Option<String> {
    if data.len() > MAX_VFS_FILE_BYTES {
        return Some("file too large".to_string());
    }
    single_error(&js_write_file(path, data, mode))
}

#[cfg(test)]
mod tests {
    use super::{
        classify_exists, decoded_or_host_error, valid_canonical_absolute_path, valid_vfs_name,
        MAX_VFS_FILE_BYTES,
    };

    #[test]
    fn browser_vfs_allocation_limit_is_explicit_and_u32_compatible() {
        assert_eq!(MAX_VFS_FILE_BYTES, 256 * 1024 * 1024);
        assert!(u32::try_from(MAX_VFS_FILE_BYTES).is_ok());
    }

    #[test]
    fn malformed_decoding_wins_over_a_false_success() {
        let (value, error) = decoded_or_host_error::<u32>(
            Err("invalid browser VFS host response".to_string()),
            Ok(None),
        );
        assert_eq!(value, None);
        assert_eq!(error.as_deref(), Some("invalid browser VFS host response"));
    }

    #[test]
    fn host_operation_error_preserves_a_partial_value() {
        let (value, error) = decoded_or_host_error(Ok(3_u32), Ok(Some("EOF".to_string())));
        assert_eq!(value, Some(3));
        assert_eq!(error.as_deref(), Some("EOF"));
    }

    #[test]
    fn host_paths_and_entry_names_must_be_canonical_and_bounded() {
        assert!(valid_canonical_absolute_path("/"));
        assert!(valid_canonical_absolute_path("/tmp/value"));
        assert!(!valid_canonical_absolute_path("tmp/value"));
        assert!(!valid_canonical_absolute_path("/tmp//value"));
        assert!(!valid_canonical_absolute_path("/tmp/../value"));
        assert!(valid_vfs_name("é", false));
        assert!(!valid_vfs_name("..", false));
        assert!(!valid_vfs_name("a/b", false));
    }

    #[test]
    fn existence_only_swallows_the_exact_not_exist_sentinel() {
        assert_eq!(classify_exists(None), Ok(true));
        assert_eq!(
            classify_exists(Some("file does not exist".to_string())),
            Ok(false)
        );
        assert_eq!(
            classify_exists(Some("permission denied".to_string())),
            Err("permission denied".to_string())
        );
        assert_eq!(
            classify_exists(Some("invalid browser VFS host response".to_string())),
            Err("invalid browser VFS host response".to_string())
        );
    }
}
