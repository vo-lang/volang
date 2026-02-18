//! path/filepath package native function implementations.

#[cfg(feature = "std")]
use std::path::PathBuf;
#[cfg(feature = "std")]
use std::fs;

use vo_ffi_macro::vostd_fn;
use vo_runtime::ffi::{ExternCallContext, ExternResult};
#[cfg(feature = "std")]
use vo_runtime::builtins::error_helper::{write_error_to, write_nil_error};

#[cfg(feature = "std")]
#[vostd_fn("path/filepath", "evalSymlinks", std)]
fn filepath_eval_symlinks(call: &mut ExternCallContext) -> ExternResult {
    let path = call.arg_str(slots::ARG_PATH);
    
    match eval_symlinks_impl(&path) {
        Ok(resolved) => {
            let result = call.alloc_str(&resolved);
            call.ret_ref(slots::RET_0, result);
            write_nil_error(call, slots::RET_1);
        }
        Err(e) => {
            call.ret_ref(slots::RET_0, std::ptr::null_mut()); // empty string
            write_error_to(call, slots::RET_1, &e);
        }
    }
    
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn eval_symlinks_impl(path: &str) -> Result<String, String> {
    // Start with the input path
    let current = PathBuf::from(path);
    
    // fs::canonicalize does the heavy lifting:
    // - Resolves all symlinks
    // - Resolves . and ..
    // - Returns absolute path
    match fs::canonicalize(&current) {
        Ok(resolved) => {
            // Convert back to string
            resolved.to_str()
                .map(|s| s.to_string())
                .ok_or_else(|| "path contains invalid UTF-8".to_string())
        }
        Err(e) => {
            // If the file doesn't exist, try to resolve what we can
            // Walk the path component by component
            if e.kind() == std::io::ErrorKind::NotFound {
                // Try resolving the parent
                if let Some(parent) = current.parent() {
                    if parent.as_os_str().is_empty() {
                        return Err(format!("lstat {}: no such file or directory", path));
                    }
                    
                    // Try to canonicalize the parent
                    match fs::canonicalize(parent) {
                        Ok(resolved_parent) => {
                            // Get the file name
                            if let Some(file_name) = current.file_name() {
                                let mut result = resolved_parent;
                                result.push(file_name);
                                return result.to_str()
                                    .map(|s| s.to_string())
                                    .ok_or_else(|| "path contains invalid UTF-8".to_string());
                            }
                        }
                        Err(_) => {}
                    }
                }
                Err(format!("lstat {}: no such file or directory", path))
            } else {
                Err(e.to_string())
            }
        }
    }
}

#[cfg(feature = "std")]
vo_runtime::stdlib_register!(path_filepath:
    evalSymlinks,
);
