//! path/filepath package WASM implementation.

use vo_runtime::builtins::error_helper::{write_error_to, write_nil_error};
use vo_runtime::bytecode::ExternDef;
use vo_runtime::ffi::{ExternCallContext, ExternContractError, ExternRegistry, ExternResult};
use vo_runtime::objects::string;

use crate::{text::utf8_arg, vfs};

const INVALID_PATH_UTF8: &str = "filepath: path contains invalid UTF-8";

fn write_eval_error(call: &mut ExternCallContext, message: &str) -> ExternResult {
    call.ret_str(0, "");
    write_error_to(call, 1, message);
    ExternResult::Ok
}

fn eval_symlinks(call: &mut ExternCallContext) -> ExternResult {
    let path = match utf8_arg(call, 0) {
        Ok(path) => path,
        Err(_) => return write_eval_error(call, INVALID_PATH_UTF8),
    };
    let host_path = match vfs::resolve_guest_path(&path) {
        Ok(path) => path,
        Err(error) => return write_eval_error(call, &error),
    };
    let (_, _, _, _, _, err) = vfs::stat(&host_path);

    if err.is_some() {
        return write_eval_error(call, "no such file or directory");
    }

    let result = normalize_path(&path);
    let str_ref = string::from_rust_str(call.gc(), &result);
    call.ret_ref(0, str_ref);
    write_nil_error(call, 1);
    ExternResult::Ok
}

fn abs_path(call: &mut ExternCallContext) -> ExternResult {
    let path = match utf8_arg(call, 0) {
        Ok(path) => path,
        Err(_) => return write_eval_error(call, INVALID_PATH_UTF8),
    };
    let joined = if path.starts_with('/') {
        path
    } else {
        format!("/{path}")
    };
    let result = normalize_path(&joined);
    let str_ref = string::from_rust_str(call.gc(), &result);
    call.ret_ref(0, str_ref);
    write_nil_error(call, 1);
    ExternResult::Ok
}

fn normalize_path(path: &str) -> String {
    if path.is_empty() {
        return ".".to_string();
    }

    let rooted = path.starts_with('/');
    let mut parts: Vec<&str> = Vec::new();

    for part in path.split('/') {
        match part {
            "" | "." => continue,
            ".." => {
                if !parts.is_empty() && parts.last() != Some(&"..") {
                    parts.pop();
                } else if !rooted {
                    parts.push("..");
                }
            }
            _ => parts.push(part),
        }
    }

    if parts.is_empty() {
        if rooted {
            return "/".to_string();
        }
        return ".".to_string();
    }

    let result = parts.join("/");
    if rooted {
        format!("/{}", result)
    } else {
        result
    }
}

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
            vo_runtime::vo_extern_name!("path/filepath", "evalSymlinks") => {
                crate::register_wasm_host(registry, id as u32, &def.name, eval_symlinks)?
            }
            vo_runtime::vo_extern_name!("path/filepath", "absPath") => {
                crate::register_wasm_host(registry, id as u32, &def.name, abs_path)?
            }
            _ => {}
        }
    }
    Ok(())
}
