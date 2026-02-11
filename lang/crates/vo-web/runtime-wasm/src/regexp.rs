//! regexp package WASM implementation using JavaScript RegExp.

use js_sys::RegExp;
use vo_runtime::bytecode::ExternDef;
use vo_runtime::ffi::{ExternCallContext, ExternRegistry, ExternResult};
use vo_runtime::objects::{array, slice, string};
use vo_runtime::core_types::{ValueKind, ValueMeta};
use wasm_bindgen::JsValue;

fn create_regexp(pattern: &str) -> Option<RegExp> {
    Some(RegExp::new(pattern, ""))
}

fn regexp_match_string(call: &mut ExternCallContext) -> ExternResult {
    let pattern = call.arg_str(0);
    let s = call.arg_str(1);
    
    let (matched, valid) = if let Some(re) = create_regexp(pattern) {
        (re.test(s), true)
    } else {
        (false, false)
    };
    
    call.ret_bool(0, matched);
    call.ret_bool(1, valid);
    ExternResult::Ok
}

fn regexp_match_bytes(call: &mut ExternCallContext) -> ExternResult {
    let pattern = call.arg_str(0);
    let b_ref = call.arg_ref(1);
    let b_len = slice::len(b_ref);
    let b_ptr = slice::data_ptr(b_ref);
    let bytes = unsafe { core::slice::from_raw_parts(b_ptr, b_len) };
    
    let (matched, valid) = match core::str::from_utf8(bytes) {
        Ok(s) => {
            if let Some(re) = create_regexp(pattern) {
                (re.test(s), true)
            } else {
                (false, false)
            }
        }
        Err(_) => (false, true),
    };
    
    call.ret_bool(0, matched);
    call.ret_bool(1, valid);
    ExternResult::Ok
}

fn regexp_find_string(call: &mut ExternCallContext) -> ExternResult {
    let pattern = call.arg_str(0);
    let s = call.arg_str(1);
    
    let result = if let Some(re) = create_regexp(pattern) {
        let exec_result = re.exec(s);
        if let Some(arr) = exec_result {
            arr.get(0).as_string().unwrap_or_default()
        } else {
            String::new()
        }
    } else {
        String::new()
    };
    
    let gc = call.gc();
    let str_ref = string::from_rust_str(gc, &result);
    call.ret_ref(0, str_ref);
    ExternResult::Ok
}

fn regexp_find_string_index(call: &mut ExternCallContext) -> ExternResult {
    let pattern = call.arg_str(0);
    let s = call.arg_str(1);
    
    let (start, end) = if let Some(re) = create_regexp(pattern) {
        let exec_result = re.exec(s);
        if let Some(arr) = exec_result {
            let matched = arr.get(0).as_string().unwrap_or_default();
            let index = js_sys::Reflect::get(&arr, &JsValue::from_str("index"))
                .ok()
                .and_then(|v| v.as_f64())
                .unwrap_or(-1.0) as i64;
            if index >= 0 {
                (index, index + matched.len() as i64)
            } else {
                (-1, -1)
            }
        } else {
            (-1, -1)
        }
    } else {
        (-1, -1)
    };
    
    call.ret_i64(0, start);
    call.ret_i64(1, end);
    ExternResult::Ok
}

fn regexp_find_all_string(call: &mut ExternCallContext) -> ExternResult {
    let pattern = call.arg_str(0);
    let s = call.arg_str(1);
    let n = call.arg_i64(2);
    
    let results: Vec<String> = if create_regexp(pattern).is_some() {
        let re = RegExp::new(pattern, "g");
        let mut matches = Vec::new();
        let limit = if n < 0 { usize::MAX } else { n as usize };
        
        loop {
            if matches.len() >= limit {
                break;
            }
            let exec_result = re.exec(s);
            if let Some(arr) = exec_result {
                if let Some(m) = arr.get(0).as_string() {
                    matches.push(m);
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        matches
    } else {
        Vec::new()
    };
    
    let gc = call.gc();
    let elem_meta = ValueMeta::new(0, ValueKind::String);
    let arr = array::create(gc, elem_meta, 8, results.len());
    for (i, r) in results.iter().enumerate() {
        let str_ref = string::from_rust_str(gc, r);
        array::set(arr, i, str_ref as u64, 8);
    }
    let slice_ref = slice::from_array(gc, arr);
    call.ret_ref(0, slice_ref);
    ExternResult::Ok
}

fn regexp_replace_all_string(call: &mut ExternCallContext) -> ExternResult {
    let pattern = call.arg_str(0);
    let src = call.arg_str(1);
    let repl = call.arg_str(2);
    
    let result = if create_regexp(pattern).is_some() {
        let re = RegExp::new(pattern, "g");
        let js_src = JsValue::from_str(src);
        js_sys::JsString::from(js_src)
            .replace_by_pattern(&re, repl)
            .as_string()
            .unwrap_or_else(|| src.to_string())
    } else {
        src.to_string()
    };
    
    let gc = call.gc();
    let str_ref = string::from_rust_str(gc, &result);
    call.ret_ref(0, str_ref);
    ExternResult::Ok
}

fn regexp_replace_all_literal_string(call: &mut ExternCallContext) -> ExternResult {
    let pattern = call.arg_str(0);
    let src = call.arg_str(1);
    let repl = call.arg_str(2);
    
    // For literal replacement, escape special replacement characters
    let escaped_repl = repl.replace('$', "$$");
    
    let result = if create_regexp(pattern).is_some() {
        let re = RegExp::new(pattern, "g");
        let js_src = JsValue::from_str(src);
        js_sys::JsString::from(js_src)
            .replace_by_pattern(&re, &escaped_repl)
            .as_string()
            .unwrap_or_else(|| src.to_string())
    } else {
        src.to_string()
    };
    
    let gc = call.gc();
    let str_ref = string::from_rust_str(gc, &result);
    call.ret_ref(0, str_ref);
    ExternResult::Ok
}

fn regexp_split_string(call: &mut ExternCallContext) -> ExternResult {
    let pattern = call.arg_str(0);
    let s = call.arg_str(1);
    let n = call.arg_i64(2);
    
    let results: Vec<String> = if create_regexp(pattern).is_some() {
        let re = RegExp::new(pattern, "");
        let js_s = js_sys::JsString::from(JsValue::from_str(s));
        let limit = if n < 0 { u32::MAX } else { n as u32 };
        let arr = js_s.split_by_pattern_limit(&re, limit);
        
        let mut parts = Vec::new();
        for i in 0..arr.length() {
            if let Some(part) = arr.get(i).as_string() {
                parts.push(part);
            }
        }
        parts
    } else {
        vec![s.to_string()]
    };
    
    let gc = call.gc();
    let elem_meta = ValueMeta::new(0, ValueKind::String);
    let arr = array::create(gc, elem_meta, 8, results.len());
    for (i, r) in results.iter().enumerate() {
        let str_ref = string::from_rust_str(gc, r);
        array::set(arr, i, str_ref as u64, 8);
    }
    let slice_ref = slice::from_array(gc, arr);
    call.ret_ref(0, slice_ref);
    ExternResult::Ok
}

fn regexp_find_string_submatch(call: &mut ExternCallContext) -> ExternResult {
    let pattern = call.arg_str(0);
    let s = call.arg_str(1);
    
    let results: Vec<String> = if let Some(re) = create_regexp(pattern) {
        let exec_result = re.exec(s);
        if let Some(arr) = exec_result {
            let mut matches = Vec::new();
            for i in 0..arr.length() {
                let val = arr.get(i);
                if val.is_undefined() {
                    matches.push(String::new());
                } else if let Some(m) = val.as_string() {
                    matches.push(m);
                } else {
                    matches.push(String::new());
                }
            }
            matches
        } else {
            Vec::new()
        }
    } else {
        Vec::new()
    };
    
    let gc = call.gc();
    let elem_meta = ValueMeta::new(0, ValueKind::String);
    let arr = array::create(gc, elem_meta, 8, results.len());
    for (i, r) in results.iter().enumerate() {
        let str_ref = string::from_rust_str(gc, r);
        array::set(arr, i, str_ref as u64, 8);
    }
    let slice_ref = slice::from_array(gc, arr);
    call.ret_ref(0, slice_ref);
    ExternResult::Ok
}

fn regexp_quote_meta(call: &mut ExternCallContext) -> ExternResult {
    let s = call.arg_str(0);
    
    // Escape regex metacharacters
    let mut result = String::with_capacity(s.len() * 2);
    for c in s.chars() {
        match c {
            '\\' | '.' | '+' | '*' | '?' | '(' | ')' | '|' | '[' | ']' | '{' | '}' | '^' | '$' => {
                result.push('\\');
                result.push(c);
            }
            _ => result.push(c),
        }
    }
    
    let gc = call.gc();
    let str_ref = string::from_rust_str(gc, &result);
    call.ret_ref(0, str_ref);
    ExternResult::Ok
}

pub fn register_externs(registry: &mut ExternRegistry, externs: &[ExternDef]) {
    for (id, def) in externs.iter().enumerate() {
        match def.name.as_str() {
            "regexp_matchString" => registry.register(id as u32, regexp_match_string),
            "regexp_matchBytes" => registry.register(id as u32, regexp_match_bytes),
            "regexp_findString" => registry.register(id as u32, regexp_find_string),
            "regexp_findStringIndex" => registry.register(id as u32, regexp_find_string_index),
            "regexp_findAllString" => registry.register(id as u32, regexp_find_all_string),
            "regexp_replaceAllString" => registry.register(id as u32, regexp_replace_all_string),
            "regexp_replaceAllLiteralString" => registry.register(id as u32, regexp_replace_all_literal_string),
            "regexp_splitString" => registry.register(id as u32, regexp_split_string),
            "regexp_findStringSubmatch" => registry.register(id as u32, regexp_find_string_submatch),
            "regexp_quoteMeta" => registry.register(id as u32, regexp_quote_meta),
            _ => {}
        }
    }
}
