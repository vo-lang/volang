//! regexp package native function implementations.
//!
//! Regular expression matching using Rust's regex crate.
//! This module requires std for regex operations.
//! In no_std mode, all functions panic with "requires std".

#[cfg(feature = "std")]
use regex::Regex;

use vo_ffi_macro::vostd_fn;
use vo_runtime::ffi::{ExternCallContext, ExternResult};

// ==================== Matching ====================

#[vostd_fn("regexp", "matchString", std)]
fn match_string(call: &mut ExternCallContext) -> ExternResult {
    let pattern = call.arg_str(slots::ARG_PATTERN);
    let s = call.arg_str(slots::ARG_S);
    let (matched, valid) = match Regex::new(pattern) {
        Ok(re) => (re.is_match(s), true),
        Err(_) => (false, false),
    };
    call.ret_bool(0, matched);
    call.ret_bool(1, valid);
    ExternResult::Ok
}

#[vostd_fn("regexp", "matchBytes", std)]
fn match_bytes(call: &mut ExternCallContext) -> ExternResult {
    use vo_runtime::objects::slice;
    let pattern = call.arg_str(slots::ARG_PATTERN);
    let b_ref = call.arg_ref(slots::ARG_B);
    let b_len = slice::len(b_ref);
    let b_ptr = slice::data_ptr(b_ref);
    let bytes = unsafe { std::slice::from_raw_parts(b_ptr, b_len) };
    let (matched, valid) = match Regex::new(pattern) {
        Ok(re) => {
            match std::str::from_utf8(bytes) {
                Ok(s) => (re.is_match(s), true),
                Err(_) => (false, true),
            }
        }
        Err(_) => (false, false),
    };
    call.ret_bool(0, matched);
    call.ret_bool(1, valid);
    ExternResult::Ok
}

// ==================== Find ====================

#[vostd_fn("regexp", "findString", std)]
fn find_string(call: &mut ExternCallContext) -> ExternResult {
    let pattern = call.arg_str(slots::ARG_PATTERN);
    let s = call.arg_str(slots::ARG_S);
    let result = match Regex::new(pattern) {
        Ok(re) => re.find(s).map(|m| m.as_str()).unwrap_or("").to_string(),
        Err(_) => String::new(),
    };
    let gc = call.gc();
    let str_ref = vo_runtime::objects::string::from_rust_str(gc, &result);
    call.ret_ref(0, str_ref);
    ExternResult::Ok
}

#[vostd_fn("regexp", "findStringIndex", std)]
fn find_string_index(call: &mut ExternCallContext) -> ExternResult {
    let pattern = call.arg_str(slots::ARG_PATTERN);
    let s = call.arg_str(slots::ARG_S);
    let (start, end) = match Regex::new(pattern) {
        Ok(re) => re.find(s).map(|m| (m.start() as i64, m.end() as i64)).unwrap_or((-1, -1)),
        Err(_) => (-1, -1),
    };
    call.ret_i64(0, start);
    call.ret_i64(1, end);
    ExternResult::Ok
}

#[vostd_fn("regexp", "findAllString", std)]
fn find_all_string(call: &mut ExternCallContext) -> ExternResult {
    use vo_runtime::objects::{array, slice, string as str_obj};
    use vo_common_core::types::ValueMeta;
    let pattern = call.arg_str(slots::ARG_PATTERN);
    let s = call.arg_str(slots::ARG_S);
    let n = call.arg_i64(slots::ARG_N);
    
    let results: Vec<String> = match Regex::new(pattern) {
        Ok(re) => {
            if n < 0 {
                re.find_iter(s).map(|m| m.as_str().to_string()).collect()
            } else if n == 0 {
                Vec::new()
            } else {
                re.find_iter(s).take(n as usize).map(|m| m.as_str().to_string()).collect()
            }
        }
        Err(_) => Vec::new(),
    };
    
    let gc = call.gc();
    let elem_meta = ValueMeta::new(0, vo_common_core::types::ValueKind::String);
    let arr = array::create(gc, elem_meta, 8, results.len());
    for (i, r) in results.iter().enumerate() {
        let str_ref = str_obj::from_rust_str(gc, r);
        array::set(arr, i, str_ref as u64, 8);
    }
    let slice_ref = slice::from_array(gc, arr);
    call.ret_ref(0, slice_ref);
    ExternResult::Ok
}

// ==================== Replace ====================

#[vostd_fn("regexp", "replaceAllString", std)]
fn replace_all_string(call: &mut ExternCallContext) -> ExternResult {
    let pattern = call.arg_str(slots::ARG_PATTERN);
    let src = call.arg_str(slots::ARG_SRC);
    let repl = call.arg_str(slots::ARG_REPL);
    let result = match Regex::new(pattern) {
        Ok(re) => re.replace_all(src, repl).into_owned(),
        Err(_) => src.to_string(),
    };
    let gc = call.gc();
    let str_ref = vo_runtime::objects::string::from_rust_str(gc, &result);
    call.ret_ref(0, str_ref);
    ExternResult::Ok
}

#[vostd_fn("regexp", "replaceAllLiteralString", std)]
fn replace_all_literal_string(call: &mut ExternCallContext) -> ExternResult {
    let pattern = call.arg_str(slots::ARG_PATTERN);
    let src = call.arg_str(slots::ARG_SRC);
    let repl = call.arg_str(slots::ARG_REPL);
    let result = match Regex::new(pattern) {
        Ok(re) => re.replace_all(src, regex::NoExpand(repl)).into_owned(),
        Err(_) => src.to_string(),
    };
    let gc = call.gc();
    let str_ref = vo_runtime::objects::string::from_rust_str(gc, &result);
    call.ret_ref(0, str_ref);
    ExternResult::Ok
}

// ==================== Split ====================

#[vostd_fn("regexp", "splitString", std)]
fn split_string(call: &mut ExternCallContext) -> ExternResult {
    use vo_runtime::objects::{array, slice, string as str_obj};
    use vo_common_core::types::ValueMeta;
    let pattern = call.arg_str(slots::ARG_PATTERN);
    let s = call.arg_str(slots::ARG_S);
    let n = call.arg_i64(slots::ARG_N);
    
    let results: Vec<String> = match Regex::new(pattern) {
        Ok(re) => {
            if n < 0 {
                re.split(s).map(|s| s.to_string()).collect()
            } else if n == 0 {
                Vec::new()
            } else {
                re.splitn(s, n as usize).map(|s| s.to_string()).collect()
            }
        }
        Err(_) => vec![s.to_string()],
    };
    
    let gc = call.gc();
    let elem_meta = ValueMeta::new(0, vo_common_core::types::ValueKind::String);
    let arr = array::create(gc, elem_meta, 8, results.len());
    for (i, r) in results.iter().enumerate() {
        let str_ref = str_obj::from_rust_str(gc, r);
        array::set(arr, i, str_ref as u64, 8);
    }
    let slice_ref = slice::from_array(gc, arr);
    call.ret_ref(0, slice_ref);
    ExternResult::Ok
}

// ==================== Submatch ====================

#[vostd_fn("regexp", "findStringSubmatch", std)]
fn find_string_submatch(call: &mut ExternCallContext) -> ExternResult {
    use vo_runtime::objects::{array, slice, string as str_obj};
    use vo_common_core::types::ValueMeta;
    let pattern = call.arg_str(slots::ARG_PATTERN);
    let s = call.arg_str(slots::ARG_S);
    
    let results: Vec<String> = match Regex::new(pattern) {
        Ok(re) => {
            re.captures(s)
                .map(|caps| {
                    caps.iter()
                        .map(|m| m.map(|m| m.as_str().to_string()).unwrap_or_default())
                        .collect()
                })
                .unwrap_or_default()
        }
        Err(_) => Vec::new(),
    };
    
    let gc = call.gc();
    let elem_meta = ValueMeta::new(0, vo_common_core::types::ValueKind::String);
    let arr = array::create(gc, elem_meta, 8, results.len());
    for (i, r) in results.iter().enumerate() {
        let str_ref = str_obj::from_rust_str(gc, r);
        array::set(arr, i, str_ref as u64, 8);
    }
    let slice_ref = slice::from_array(gc, arr);
    call.ret_ref(0, slice_ref);
    ExternResult::Ok
}

// ==================== Quote ====================

#[vostd_fn("regexp", "quoteMeta", std)]
fn quote_meta(call: &mut ExternCallContext) -> ExternResult {
    let s = call.arg_str(slots::ARG_S);
    let result = regex::escape(s);
    let gc = call.gc();
    let str_ref = vo_runtime::objects::string::from_rust_str(gc, &result);
    call.ret_ref(0, str_ref);
    ExternResult::Ok
}

vo_runtime::stdlib_register!(regexp: matchString, matchBytes, findString, findStringIndex, findAllString, replaceAllString, replaceAllLiteralString, splitString, findStringSubmatch, quoteMeta);
