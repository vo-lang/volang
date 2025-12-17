//! Native implementations for the regexp package.

use gox_vm::native::{NativeCtx, NativeResult, NativeRegistry};
use gox_vm::objects::{array, slice, string};
use gox_vm::types::builtin;
use regex::Regex;

pub fn register(registry: &mut NativeRegistry) {
    registry.register("regexp.Match", native_match);
    registry.register("regexp.QuoteMeta", native_quote_meta);
    registry.register("regexp.MatchString", native_match_string);
    registry.register("regexp.FindString", native_find_string);
    registry.register("regexp.FindAllString", native_find_all_string);
    registry.register("regexp.ReplaceAllString", native_replace_all_string);
    registry.register("regexp.Split", native_split);
}

/// regexp.Match(pattern, s string) (bool, error)
fn native_match(ctx: &mut NativeCtx) -> NativeResult {
    let pattern = ctx.arg_str(0).to_string();
    let s = ctx.arg_str(1).to_string();
    
    match Regex::new(&pattern) {
        Ok(re) => {
            ctx.ret_bool(0, re.is_match(&s));
            ctx.ret_nil(1);
        }
        Err(e) => {
            ctx.ret_bool(0, false);
            ctx.ret_string(1, &e.to_string());
        }
    }
    NativeResult::Ok(2)
}

/// regexp.QuoteMeta(s string) string
fn native_quote_meta(ctx: &mut NativeCtx) -> NativeResult {
    let s = ctx.arg_str(0);
    let result = regex::escape(s);
    ctx.ret_string(0, &result);
    NativeResult::Ok(1)
}

/// (re *Regexp) MatchString(s string) bool
fn native_match_string(ctx: &mut NativeCtx) -> NativeResult {
    let pattern = ctx.arg_str(0).to_string();
    let s = ctx.arg_str(1).to_string();
    
    let matched = Regex::new(&pattern)
        .map(|re| re.is_match(&s))
        .unwrap_or(false);
    
    ctx.ret_bool(0, matched);
    NativeResult::Ok(1)
}

/// (re *Regexp) FindString(s string) string
fn native_find_string(ctx: &mut NativeCtx) -> NativeResult {
    let pattern = ctx.arg_str(0).to_string();
    let s = ctx.arg_str(1).to_string();
    
    let result = Regex::new(&pattern)
        .ok()
        .and_then(|re| re.find(&s))
        .map(|m| m.as_str().to_string())
        .unwrap_or_default();
    
    ctx.ret_string(0, &result);
    NativeResult::Ok(1)
}

/// (re *Regexp) FindAllString(s string, n int) []string
fn native_find_all_string(ctx: &mut NativeCtx) -> NativeResult {
    let pattern = ctx.arg_str(0).to_string();
    let s = ctx.arg_str(1).to_string();
    let n = ctx.arg_i64(2);
    
    let matches: Vec<String> = match Regex::new(&pattern) {
        Ok(re) => {
            if n < 0 {
                re.find_iter(&s).map(|m| m.as_str().to_string()).collect()
            } else {
                re.find_iter(&s).take(n as usize).map(|m| m.as_str().to_string()).collect()
            }
        }
        Err(_) => vec![],
    };
    
    let gc = ctx.gc();
    let arr = array::create(gc, builtin::ARRAY, builtin::STRING, 1, matches.len());
    for (i, m) in matches.iter().enumerate() {
        let str_ref = string::from_rust_str(gc, builtin::STRING, m);
        array::set(arr, i, str_ref as u64);
    }
    let result = slice::from_array(gc, builtin::SLICE, arr);
    ctx.ret_ref(0, result);
    NativeResult::Ok(1)
}

/// (re *Regexp) ReplaceAllString(src, repl string) string
fn native_replace_all_string(ctx: &mut NativeCtx) -> NativeResult {
    let pattern = ctx.arg_str(0).to_string();
    let src = ctx.arg_str(1).to_string();
    let repl = ctx.arg_str(2).to_string();
    
    let result = Regex::new(&pattern)
        .map(|re| re.replace_all(&src, repl.as_str()).to_string())
        .unwrap_or(src);
    
    ctx.ret_string(0, &result);
    NativeResult::Ok(1)
}

/// (re *Regexp) Split(s string, n int) []string
fn native_split(ctx: &mut NativeCtx) -> NativeResult {
    let pattern = ctx.arg_str(0).to_string();
    let s = ctx.arg_str(1).to_string();
    let n = ctx.arg_i64(2);
    
    let parts: Vec<String> = match Regex::new(&pattern) {
        Ok(re) => {
            if n < 0 {
                re.split(&s).map(|p| p.to_string()).collect()
            } else if n == 0 {
                vec![]
            } else {
                re.splitn(&s, n as usize).map(|p| p.to_string()).collect()
            }
        }
        Err(_) => vec![s],
    };
    
    let gc = ctx.gc();
    let arr = array::create(gc, builtin::ARRAY, builtin::STRING, 1, parts.len());
    for (i, p) in parts.iter().enumerate() {
        let str_ref = string::from_rust_str(gc, builtin::STRING, p);
        array::set(arr, i, str_ref as u64);
    }
    let result = slice::from_array(gc, builtin::SLICE, arr);
    ctx.ret_ref(0, result);
    NativeResult::Ok(1)
}
