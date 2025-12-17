//! Native implementations for the encoding/json package.

use gox_vm::native::{NativeCtx, NativeResult, NativeRegistry};
use gox_vm::objects::slice;

pub fn register(registry: &mut NativeRegistry) {
    registry.register("json.Valid", native_valid);
    registry.register("json.MarshalString", native_marshal_string);
    registry.register("json.UnmarshalString", native_unmarshal_string);
}

/// json.Valid(data []byte) bool
fn native_valid(ctx: &mut NativeCtx) -> NativeResult {
    let data_ref = ctx.arg_ref(0);
    
    if data_ref.is_null() {
        ctx.ret_bool(0, false);
        return NativeResult::Ok(1);
    }
    
    let len = slice::len(data_ref);
    let data: Vec<u8> = (0..len).map(|i| slice::get(data_ref, i) as u8).collect();
    let s = String::from_utf8_lossy(&data);
    
    // Simple JSON validation - check basic structure
    let valid = is_valid_json(&s);
    ctx.ret_bool(0, valid);
    NativeResult::Ok(1)
}

/// Simple JSON string marshaling (for basic types)
fn native_marshal_string(ctx: &mut NativeCtx) -> NativeResult {
    let s = ctx.arg_str(0).to_string();
    let result = format!("\"{}\"", escape_json_string(&s));
    ctx.ret_string(0, &result);
    NativeResult::Ok(1)
}

/// Simple JSON string unmarshaling
fn native_unmarshal_string(ctx: &mut NativeCtx) -> NativeResult {
    let data = ctx.arg_str(0).to_string();
    let trimmed = data.trim();
    
    if trimmed.starts_with('"') && trimmed.ends_with('"') && trimmed.len() >= 2 {
        let inner = &trimmed[1..trimmed.len()-1];
        let unescaped = unescape_json_string(inner);
        ctx.ret_string(0, &unescaped);
        ctx.ret_nil(1);
    } else {
        ctx.ret_string(0, "");
        ctx.ret_string(1, "invalid JSON string");
    }
    NativeResult::Ok(2)
}

/// Simple JSON validation (basic structure check)
fn is_valid_json(s: &str) -> bool {
    let s = s.trim();
    if s.is_empty() {
        return false;
    }
    
    match s.chars().next() {
        Some('{') => s.ends_with('}'),
        Some('[') => s.ends_with(']'),
        Some('"') => s.ends_with('"') && s.len() >= 2,
        Some(c) if c.is_ascii_digit() || c == '-' => {
            s.chars().all(|c| c.is_ascii_digit() || c == '.' || c == '-' || c == 'e' || c == 'E' || c == '+')
        }
        Some('t') => s == "true",
        Some('f') => s == "false",
        Some('n') => s == "null",
        _ => false,
    }
}

/// Escape a string for JSON
fn escape_json_string(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '"' => result.push_str("\\\""),
            '\\' => result.push_str("\\\\"),
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            c if c.is_control() => {
                result.push_str(&format!("\\u{:04x}", c as u32));
            }
            c => result.push(c),
        }
    }
    result
}

/// Unescape a JSON string
fn unescape_json_string(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();
    
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('"') => result.push('"'),
                Some('\\') => result.push('\\'),
                Some('/') => result.push('/'),
                Some('n') => result.push('\n'),
                Some('r') => result.push('\r'),
                Some('t') => result.push('\t'),
                Some('b') => result.push('\x08'),
                Some('f') => result.push('\x0C'),
                Some('u') => {
                    let hex: String = chars.by_ref().take(4).collect();
                    if let Ok(code) = u32::from_str_radix(&hex, 16) {
                        if let Some(c) = char::from_u32(code) {
                            result.push(c);
                        }
                    }
                }
                Some(c) => {
                    result.push('\\');
                    result.push(c);
                }
                None => result.push('\\'),
            }
        } else {
            result.push(c);
        }
    }
    result
}
