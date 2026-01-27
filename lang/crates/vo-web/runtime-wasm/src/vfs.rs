//! Virtual File System - Rust bindings to JS VFS.
//!
//! All actual filesystem operations are implemented in JS (vfs.ts).
//! This module declares the wasm_bindgen imports and provides Rust wrappers.

use wasm_bindgen::prelude::*;

// =============================================================================
// JS Imports
// =============================================================================

#[wasm_bindgen]
extern "C" {
    // File operations
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

    // Directory operations
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

    #[wasm_bindgen(js_namespace = window, js_name = _vfsStat)]
    fn js_stat(path: &str) -> JsValue;

    #[wasm_bindgen(js_namespace = window, js_name = _vfsReadDir)]
    fn js_read_dir(path: &str) -> JsValue;

    #[wasm_bindgen(js_namespace = window, js_name = _vfsChmod)]
    fn js_chmod(path: &str, mode: u32) -> JsValue;

    #[wasm_bindgen(js_namespace = window, js_name = _vfsTruncate)]
    fn js_truncate(path: &str, size: i64) -> JsValue;

    // Convenience
    #[wasm_bindgen(js_namespace = window, js_name = _vfsReadFile)]
    fn js_read_file(path: &str) -> JsValue;

    #[wasm_bindgen(js_namespace = window, js_name = _vfsWriteFile)]
    fn js_write_file(path: &str, data: &[u8], mode: u32) -> JsValue;
}

// =============================================================================
// Helper Functions
// =============================================================================

/// Get error string from JS result array at given index.
/// Returns None if null/undefined, Some(msg) if error string.
pub fn get_js_error(result: &JsValue, idx: u32) -> Option<String> {
    let err = js_sys::Reflect::get_u32(result, idx).ok()?;
    if err.is_null() || err.is_undefined() {
        None
    } else {
        err.as_string()
    }
}

/// Get i32 from JS result array.
pub fn get_js_i32(result: &JsValue, idx: u32) -> i32 {
    js_sys::Reflect::get_u32(result, idx)
        .ok()
        .and_then(|v| v.as_f64())
        .map(|v| v as i32)
        .unwrap_or(-1)
}

/// Get i64 from JS result array.
pub fn get_js_i64(result: &JsValue, idx: u32) -> i64 {
    js_sys::Reflect::get_u32(result, idx)
        .ok()
        .and_then(|v| v.as_f64())
        .map(|v| v as i64)
        .unwrap_or(0)
}

/// Get bool from JS result array.
pub fn get_js_bool(result: &JsValue, idx: u32) -> bool {
    js_sys::Reflect::get_u32(result, idx)
        .ok()
        .and_then(|v| v.as_bool())
        .unwrap_or(false)
}

/// Get string from JS result array.
pub fn get_js_string(result: &JsValue, idx: u32) -> String {
    js_sys::Reflect::get_u32(result, idx)
        .ok()
        .and_then(|v| v.as_string())
        .unwrap_or_default()
}

/// Get bytes (Uint8Array) from JS result array.
pub fn get_js_bytes(result: &JsValue, idx: u32) -> Vec<u8> {
    js_sys::Reflect::get_u32(result, idx)
        .ok()
        .and_then(|v| {
            if v.is_null() || v.is_undefined() {
                return None;
            }
            let arr = js_sys::Uint8Array::new(&v);
            Some(arr.to_vec())
        })
        .unwrap_or_default()
}

/// Get error from single-value JS result (not array).
pub fn get_js_single_error(result: &JsValue) -> Option<String> {
    if result.is_null() || result.is_undefined() {
        None
    } else {
        result.as_string()
    }
}

// =============================================================================
// Public API (called from os.rs)
// =============================================================================

/// Open file. Returns (fd, error).
pub fn open_file(path: &str, flags: i32, mode: u32) -> (i32, Option<String>) {
    let result = js_open_file(path, flags, mode);
    let fd = get_js_i32(&result, 0);
    let err = get_js_error(&result, 1);
    (fd, err)
}

/// Read from fd. Returns (data, error).
pub fn read(fd: i32, length: u32) -> (Vec<u8>, Option<String>) {
    let result = js_read(fd, length);
    let data = get_js_bytes(&result, 0);
    let err = get_js_error(&result, 1);
    (data, err)
}

/// Write to fd. Returns (n, error).
pub fn write(fd: i32, data: &[u8]) -> (i32, Option<String>) {
    let result = js_write(fd, data);
    let n = get_js_i32(&result, 0);
    let err = get_js_error(&result, 1);
    (n, err)
}

/// Read at offset. Returns (data, error).
pub fn read_at(fd: i32, length: u32, offset: i64) -> (Vec<u8>, Option<String>) {
    let result = js_read_at(fd, length, offset);
    let data = get_js_bytes(&result, 0);
    let err = get_js_error(&result, 1);
    (data, err)
}

/// Write at offset. Returns (n, error).
pub fn write_at(fd: i32, data: &[u8], offset: i64) -> (i32, Option<String>) {
    let result = js_write_at(fd, data, offset);
    let n = get_js_i32(&result, 0);
    let err = get_js_error(&result, 1);
    (n, err)
}

/// Seek. Returns (pos, error).
pub fn seek(fd: i32, offset: i64, whence: i32) -> (i64, Option<String>) {
    let result = js_seek(fd, offset, whence);
    let pos = get_js_i64(&result, 0);
    let err = get_js_error(&result, 1);
    (pos, err)
}

/// Close fd. Returns error.
pub fn close(fd: i32) -> Option<String> {
    let result = js_close(fd);
    get_js_single_error(&result)
}

/// Sync fd. Returns error.
pub fn sync(fd: i32) -> Option<String> {
    let result = js_sync(fd);
    get_js_single_error(&result)
}

/// Fstat. Returns (size, mode, mod_time, is_dir, error).
pub fn fstat(fd: i32) -> (i64, u32, i64, bool, Option<String>) {
    let result = js_fstat(fd);
    let size = get_js_i64(&result, 0);
    let mode = get_js_i64(&result, 1) as u32;
    let mod_time = get_js_i64(&result, 2);
    let is_dir = get_js_bool(&result, 3);
    let err = get_js_error(&result, 4);
    (size, mode, mod_time, is_dir, err)
}

/// Ftruncate. Returns error.
pub fn ftruncate(fd: i32, size: i64) -> Option<String> {
    let result = js_ftruncate(fd, size);
    get_js_single_error(&result)
}

/// Mkdir. Returns error.
pub fn mkdir(path: &str, mode: u32) -> Option<String> {
    let result = js_mkdir(path, mode);
    get_js_single_error(&result)
}

/// MkdirAll. Returns error.
pub fn mkdir_all(path: &str, mode: u32) -> Option<String> {
    let result = js_mkdir_all(path, mode);
    get_js_single_error(&result)
}

/// Remove. Returns error.
pub fn remove(path: &str) -> Option<String> {
    let result = js_remove(path);
    get_js_single_error(&result)
}

/// RemoveAll. Returns error.
pub fn remove_all(path: &str) -> Option<String> {
    let result = js_remove_all(path);
    get_js_single_error(&result)
}

/// Rename. Returns error.
pub fn rename(old_path: &str, new_path: &str) -> Option<String> {
    let result = js_rename(old_path, new_path);
    get_js_single_error(&result)
}

/// Stat. Returns (name, size, mode, mod_time, is_dir, error).
pub fn stat(path: &str) -> (String, i64, u32, i64, bool, Option<String>) {
    let result = js_stat(path);
    let name = get_js_string(&result, 0);
    let size = get_js_i64(&result, 1);
    let mode = get_js_i64(&result, 2) as u32;
    let mod_time = get_js_i64(&result, 3);
    let is_dir = get_js_bool(&result, 4);
    let err = get_js_error(&result, 5);
    (name, size, mode, mod_time, is_dir, err)
}

/// ReadDir. Returns (entries, error). Each entry is (name, is_dir, mode).
pub fn read_dir(path: &str) -> (Vec<(String, bool, u32)>, Option<String>) {
    let result = js_read_dir(path);
    let err = get_js_error(&result, 1);
    
    let mut entries = Vec::new();
    if err.is_none() {
        if let Ok(arr) = js_sys::Reflect::get_u32(&result, 0) {
            if let Some(arr) = arr.dyn_ref::<js_sys::Array>() {
                for i in 0..arr.length() {
                    if let Some(entry) = arr.get(i).dyn_ref::<js_sys::Array>() {
                        let name = entry.get(0).as_string().unwrap_or_default();
                        let is_dir = entry.get(1).as_bool().unwrap_or(false);
                        let mode = entry.get(2).as_f64().unwrap_or(0.0) as u32;
                        entries.push((name, is_dir, mode));
                    }
                }
            }
        }
    }
    
    (entries, err)
}

/// Chmod. Returns error.
pub fn chmod(path: &str, mode: u32) -> Option<String> {
    let result = js_chmod(path, mode);
    get_js_single_error(&result)
}

/// Truncate. Returns error.
pub fn truncate(path: &str, size: i64) -> Option<String> {
    let result = js_truncate(path, size);
    get_js_single_error(&result)
}

/// ReadFile. Returns (data, error).
pub fn read_file(path: &str) -> (Vec<u8>, Option<String>) {
    let result = js_read_file(path);
    let data = get_js_bytes(&result, 0);
    let err = get_js_error(&result, 1);
    (data, err)
}

/// WriteFile. Returns error.
pub fn write_file(path: &str, data: &[u8], mode: u32) -> Option<String> {
    let result = js_write_file(path, data, mode);
    get_js_single_error(&result)
}
