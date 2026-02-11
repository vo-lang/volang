//! path/filepath package WASM implementation.

use vo_runtime::bytecode::ExternDef;
use vo_runtime::ffi::{ExternCallContext, ExternRegistry, ExternResult};
use vo_runtime::objects::string;
use vo_runtime::builtins::error_helper::{write_nil_error, write_error_to};

use crate::vfs;

fn eval_symlinks(call: &mut ExternCallContext) -> ExternResult {
    let path = call.arg_str(0);
    let (_, _, _, _, _, err) = vfs::stat(&path);
    
    if err.is_some() {
        // Path doesn't exist - try to resolve parent
        if let Some(slash) = path.rfind('/') {
            let (parent, name) = (&path[..slash], &path[slash + 1..]);
            if !parent.is_empty() && vfs::stat(parent).5.is_none() {
                let result = format!("{}/{}", normalize_path(parent), name);
                let str_ref = string::from_rust_str(call.gc(), &result);
                call.ret_ref(0, str_ref);
                write_nil_error(call, 1);
                return ExternResult::Ok;
            }
        }
        let str_ref = string::from_rust_str(call.gc(), "");
        call.ret_ref(0, str_ref);
        write_error_to(call, 1, "no such file or directory");
        return ExternResult::Ok;
    }
    
    let result = normalize_path(&path);
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

pub fn register_externs(registry: &mut ExternRegistry, externs: &[ExternDef]) {
    for (id, def) in externs.iter().enumerate() {
        match def.name.as_str() {
            "path_filepath_evalSymlinks" => registry.register(id as u32, eval_symlinks),
            _ => {}
        }
    }
}
