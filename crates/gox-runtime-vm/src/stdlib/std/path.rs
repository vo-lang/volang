//! Native implementations for the path package.

use gox_vm::gc::GcRef;
use gox_vm::extern_fn::{ExternCtx, ExternResult, ExternRegistry};
use gox_vm::objects::{slice, string};
use std::path::Path;

pub fn register(registry: &mut ExternRegistry) {
    registry.register("path.Base", native_base);
    registry.register("path.Dir", native_dir);
    registry.register("path.Ext", native_ext);
    registry.register("path.Clean", native_clean);
    registry.register("path.Join", native_join);
    registry.register("path.Split", native_split);
    registry.register("path.IsAbs", native_is_abs);
    registry.register("path.Match", native_match);
}

fn native_base(ctx: &mut ExternCtx) -> ExternResult {
    let path = ctx.arg_str(0).to_string();
    let base = Path::new(&path)
        .file_name()
        .map(|s| s.to_string_lossy().to_string())
        .unwrap_or_else(|| ".".to_string());
    ctx.ret_string(0, &base);
    ExternResult::Ok(1)
}

fn native_dir(ctx: &mut ExternCtx) -> ExternResult {
    let path = ctx.arg_str(0).to_string();
    let dir = Path::new(&path)
        .parent()
        .map(|p| p.to_string_lossy().to_string())
        .unwrap_or_else(|| ".".to_string());
    ctx.ret_string(0, &dir);
    ExternResult::Ok(1)
}

fn native_ext(ctx: &mut ExternCtx) -> ExternResult {
    let path = ctx.arg_str(0).to_string();
    let ext = Path::new(&path)
        .extension()
        .map(|s| format!(".{}", s.to_string_lossy()))
        .unwrap_or_default();
    ctx.ret_string(0, &ext);
    ExternResult::Ok(1)
}

fn native_clean(ctx: &mut ExternCtx) -> ExternResult {
    let path = ctx.arg_str(0).to_string();
    // Simple path cleaning (normalize separators, remove redundant ..)
    let cleaned = clean_path(&path);
    ctx.ret_string(0, &cleaned);
    ExternResult::Ok(1)
}

fn clean_path(path: &str) -> String {
    if path.is_empty() {
        return ".".to_string();
    }
    
    let mut parts: Vec<&str> = Vec::new();
    let is_abs = path.starts_with('/');
    
    for part in path.split('/') {
        match part {
            "" | "." => continue,
            ".." => {
                if !parts.is_empty() && parts.last() != Some(&"..") {
                    parts.pop();
                } else if !is_abs {
                    parts.push("..");
                }
            }
            _ => parts.push(part),
        }
    }
    
    if parts.is_empty() {
        if is_abs { "/" } else { "." }.to_string()
    } else if is_abs {
        format!("/{}", parts.join("/"))
    } else {
        parts.join("/")
    }
}

fn native_join(ctx: &mut ExternCtx) -> ExternResult {
    let slice_ref = ctx.arg_ref(0);
    
    if slice_ref.is_null() {
        ctx.ret_string(0, "");
        return ExternResult::Ok(1);
    }
    
    let len = slice::len(slice_ref);
    let mut parts: Vec<String> = Vec::with_capacity(len);
    
    for i in 0..len {
        let str_ref = slice::get(slice_ref, i) as GcRef;
        if !str_ref.is_null() {
            let s = string::as_str(str_ref);
            if !s.is_empty() {
                parts.push(s.to_string());
            }
        }
    }
    
    let joined = parts.join("/");
    let cleaned = clean_path(&joined);
    ctx.ret_string(0, &cleaned);
    ExternResult::Ok(1)
}

fn native_split(ctx: &mut ExternCtx) -> ExternResult {
    let path = ctx.arg_str(0).to_string();
    let p = Path::new(&path);
    
    let dir = p.parent()
        .map(|p| {
            let s = p.to_string_lossy().to_string();
            if s.is_empty() { "".to_string() } else { format!("{}/", s) }
        })
        .unwrap_or_default();
    
    let file = p.file_name()
        .map(|s| s.to_string_lossy().to_string())
        .unwrap_or_default();
    
    ctx.ret_string(0, &dir);
    ctx.ret_string(1, &file);
    ExternResult::Ok(2)
}

fn native_is_abs(ctx: &mut ExternCtx) -> ExternResult {
    let path = ctx.arg_str(0);
    ctx.ret_bool(0, path.starts_with('/'));
    ExternResult::Ok(1)
}

fn native_match(ctx: &mut ExternCtx) -> ExternResult {
    let pattern = ctx.arg_str(0).to_string();
    let name = ctx.arg_str(1).to_string();
    
    // Simple glob matching
    let matched = glob_match(&pattern, &name);
    ctx.ret_bool(0, matched);
    ExternResult::Ok(1)
}

fn glob_match(pattern: &str, name: &str) -> bool {
    let pattern_chars: Vec<char> = pattern.chars().collect();
    let name_chars: Vec<char> = name.chars().collect();
    glob_match_impl(&pattern_chars, &name_chars)
}

fn glob_match_impl(pattern: &[char], name: &[char]) -> bool {
    let mut pi = 0;
    let mut ni = 0;
    let mut star_pi = None;
    let mut star_ni = 0;
    
    while ni < name.len() {
        if pi < pattern.len() && (pattern[pi] == '?' || pattern[pi] == name[ni]) {
            pi += 1;
            ni += 1;
        } else if pi < pattern.len() && pattern[pi] == '*' {
            star_pi = Some(pi);
            star_ni = ni;
            pi += 1;
        } else if let Some(spi) = star_pi {
            pi = spi + 1;
            star_ni += 1;
            ni = star_ni;
        } else {
            return false;
        }
    }
    
    while pi < pattern.len() && pattern[pi] == '*' {
        pi += 1;
    }
    
    pi == pattern.len()
}

