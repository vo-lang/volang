//! fmt package native function implementations.
//!
//! Provides print and format functions for the fmt standard library package.
//!
//! Native layer handles:
//! - nativeSprint: format []interface{} with default format (space-separated)
//! - nativeSprintln: format []interface{} with newline
//! - nativeSprintf: format with format string
//! - nativeWrite: output string to stdout/buffer
//!
//! Vo layer (fmt.vo) provides Print, Println, Printf, Sprint, Sprintln, Sprintf
//! which call these native functions.

#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::format;

use vo_common_core::types::ValueKind;
use vo_ffi_macro::vostd_fn;
use vo_runtime::ffi::{ExternCallContext, ExternResult};
use vo_runtime::gc::GcRef;
use vo_runtime::objects::{interface, slice, string as str_obj};

// =============================================================================
// Format interface{} values - re-export from vo_runtime::builtins
// =============================================================================

pub use vo_runtime::builtins::{format_interface, format_interface_with_ctx};

/// Convert ValueKind to Go type name string for %T verb.
fn value_kind_to_type_name(vk: ValueKind) -> String {
    match vk {
        ValueKind::Void => "<nil>".to_string(),
        ValueKind::Bool => "bool".to_string(),
        ValueKind::Int => "int".to_string(),
        ValueKind::Int8 => "int8".to_string(),
        ValueKind::Int16 => "int16".to_string(),
        ValueKind::Int32 => "int32".to_string(),
        ValueKind::Int64 => "int64".to_string(),
        ValueKind::Uint => "uint".to_string(),
        ValueKind::Uint8 => "uint8".to_string(),
        ValueKind::Uint16 => "uint16".to_string(),
        ValueKind::Uint32 => "uint32".to_string(),
        ValueKind::Uint64 => "uint64".to_string(),
        ValueKind::Float32 => "float32".to_string(),
        ValueKind::Float64 => "float64".to_string(),
        ValueKind::String => "string".to_string(),
        ValueKind::Slice => "[]...".to_string(),
        ValueKind::Map => "map[...]...".to_string(),
        ValueKind::Channel => "chan ...".to_string(),
        ValueKind::Closure => "func(...)".to_string(),
        ValueKind::Pointer => "*...".to_string(),
        ValueKind::Array => "[...]...".to_string(),
        ValueKind::Struct => "struct{...}".to_string(),
        ValueKind::Interface => "interface{}".to_string(),
        ValueKind::Port => "port ...".to_string(),
        ValueKind::Island => "island".to_string(),
    }
}

/// Format all elements in a []interface{} slice with space separator.
fn format_args_slice_with_ctx(slice_ref: GcRef, call: Option<&ExternCallContext>) -> String {
    if slice_ref.is_null() {
        return String::new();
    }
    
    let len = slice::len(slice_ref);
    if len == 0 {
        return String::new();
    }
    
    let data_ptr = slice::data_ptr(slice_ref) as *const u64;
    let mut result = String::new();
    
    for i in 0..len {
        if i > 0 {
            result.push(' ');
        }
        // Each interface{} is 2 slots (16 bytes)
        let slot0 = unsafe { *data_ptr.add(i * 2) };
        let slot1 = unsafe { *data_ptr.add(i * 2 + 1) };
        result.push_str(&format_interface_with_ctx(slot0, slot1, call));
    }
    
    result
}

// =============================================================================
// Printf format string parsing and formatting
// =============================================================================

#[derive(Clone, Copy, Default)]
struct FormatFlags {
    left: bool,
    plus: bool,
    zero: bool,
}

#[derive(Clone, Copy, Default)]
struct FormatSpec {
    flags: FormatFlags,
    width: Option<usize>,
    precision: Option<usize>,
    verb: char,
}

fn parse_number(chars: &mut core::iter::Peekable<core::str::Chars<'_>>) -> Option<usize> {
    let mut n: usize = 0;
    let mut any = false;
    while let Some(&c) = chars.peek() {
        if !c.is_ascii_digit() {
            break;
        }
        any = true;
        chars.next();
        n = n * 10 + (c as u8 - b'0') as usize;
    }
    if any { Some(n) } else { None }
}

fn parse_format_spec(chars: &mut core::iter::Peekable<core::str::Chars<'_>>) -> Option<FormatSpec> {
    let mut flags = FormatFlags::default();

    loop {
        match chars.peek().copied() {
            Some('-') => {
                flags.left = true;
                let _ = chars.next();
            }
            Some('+') => {
                flags.plus = true;
                let _ = chars.next();
            }
            Some('0') => {
                flags.zero = true;
                let _ = chars.next();
            }
            _ => break,
        }
    }

    let width = parse_number(chars);

    let precision = if chars.peek().copied() == Some('.') {
        let _ = chars.next();
        Some(parse_number(chars).unwrap_or(0))
    } else {
        None
    };

    let verb = chars.next()?;
    Some(FormatSpec {
        flags,
        width,
        precision,
        verb,
    })
}

fn pad_width(mut s: String, spec: FormatSpec, default_pad: char) -> String {
    let Some(width) = spec.width else {
        return s;
    };

    let len = s.chars().count();
    if len >= width {
        return s;
    }

    let pad_len = width - len;
    let pad_char = if spec.flags.zero && !spec.flags.left { '0' } else { default_pad };
    if spec.flags.left {
        for _ in 0..pad_len {
            s.push(pad_char);
        }
        return s;
    }

    let mut out = String::new();
    for _ in 0..pad_len {
        out.push(pad_char);
    }
    out.push_str(&s);
    out
}

fn pad_numeric(mut s: String, spec: FormatSpec) -> String {
    let Some(width) = spec.width else {
        return s;
    };

    let len = s.chars().count();
    if len >= width {
        return s;
    }

    let pad_len = width - len;
    if spec.flags.left {
        for _ in 0..pad_len {
            s.push(' ');
        }
        return s;
    }

    let pad_char = if spec.flags.zero { '0' } else { ' ' };

    let mut out = String::new();
    let mut it = s.chars();
    if pad_char == '0' {
        if let Some(first) = it.next() {
            if first == '-' || first == '+' {
                out.push(first);
                for _ in 0..pad_len {
                    out.push('0');
                }
                out.extend(it);
                return out;
            }
        }
    }

    for _ in 0..pad_len {
        out.push(pad_char);
    }
    out.push_str(&s);
    out
}

/// Format with printf-style format string.
fn sprintf_impl(format_str: &str, args_ref: GcRef, call: Option<&ExternCallContext>) -> String {
    let args_len = if args_ref.is_null() { 0 } else { slice::len(args_ref) };
    let data_ptr = if args_ref.is_null() { 
        core::ptr::null() 
    } else { 
        slice::data_ptr(args_ref) as *const u64 
    };
    
    let mut result = String::new();
    let mut chars = format_str.chars().peekable();
    let mut arg_idx = 0usize;
    
    while let Some(c) = chars.next() {
        if c != '%' {
            result.push(c);
            continue;
        }

        if chars.peek().copied() == Some('%') {
            let _ = chars.next();
            result.push('%');
            continue;
        }

        let Some(spec) = parse_format_spec(&mut chars) else {
            result.push('%');
            break;
        };

        if arg_idx >= args_len {
            result.push_str("%!");
            result.push(spec.verb);
            result.push_str("(MISSING)");
            continue;
        }

        let slot0 = unsafe { *data_ptr.add(arg_idx * 2) };
        let slot1 = unsafe { *data_ptr.add(arg_idx * 2 + 1) };
        result.push_str(&format_with_spec(spec, slot0, slot1, call));
        arg_idx += 1;
    }
    
    result
}

fn format_with_spec(spec: FormatSpec, slot0: u64, slot1: u64, call: Option<&ExternCallContext>) -> String {
    let vk = interface::unpack_value_kind(slot0);

    match spec.verb {
        'v' => format_interface_with_ctx(slot0, slot1, call),
        'd' => {
            if vk.is_integer() {
                let mut s = (slot1 as i64).to_string();
                if spec.flags.plus && !s.starts_with('-') {
                    s = format!("+{}", s);
                }

                if let Some(prec) = spec.precision {
                    let (sign, digits) = if s.starts_with('-') {
                        ('-', &s[1..])
                    } else if s.starts_with('+') {
                        ('+', &s[1..])
                    } else {
                        ('\0', s.as_str())
                    };

                    let digit_len = digits.chars().count();
                    if digit_len < prec {
                        let mut out = String::new();
                        if sign != '\0' {
                            out.push(sign);
                        }
                        for _ in 0..(prec - digit_len) {
                            out.push('0');
                        }
                        out.push_str(digits);
                        s = out;
                    }
                }

                pad_numeric(s, spec)
            } else {
                format!("%!d({})", format_interface(slot0, slot1))
            }
        }
        's' => {
            match vk {
                ValueKind::String => {
                    let mut s = str_obj::as_str(slot1 as GcRef).to_string();
                    if let Some(prec) = spec.precision {
                        s = s.chars().take(prec).collect();
                    }
                    pad_width(s, spec, ' ')
                }
                _ => pad_width(format_interface_with_ctx(slot0, slot1, call), spec, ' '),
            }
        }
        'f' => {
            match vk {
                ValueKind::Float32 | ValueKind::Float64 => {
                    let f = match vk {
                        ValueKind::Float32 => f32::from_bits(slot1 as u32) as f64,
                        ValueKind::Float64 => f64::from_bits(slot1),
                        _ => unreachable!(),
                    };

                    let prec = spec.precision.unwrap_or(6);
                    let mut s = if spec.flags.plus {
                        format!("{:+.*}", prec, f)
                    } else {
                        format!("{:.*}", prec, f)
                    };
                    s = pad_numeric(s, spec);
                    s
                }
                _ => format!("%!f({})", format_interface(slot0, slot1)),
            }
        }
        't' => {
            match vk {
                ValueKind::Bool => if slot1 != 0 { "true" } else { "false" }.to_string(),
                _ => format!("%!t({})", format_interface(slot0, slot1)),
            }
        }
        'x' => {
            match vk {
                ValueKind::Int | ValueKind::Int64 | ValueKind::Uint | ValueKind::Uint64 => {
                    format!("{:x}", slot1)
                }
                ValueKind::Int8 | ValueKind::Uint8 => format!("{:x}", slot1 as u8),
                ValueKind::Int16 | ValueKind::Uint16 => format!("{:x}", slot1 as u16),
                ValueKind::Int32 | ValueKind::Uint32 => format!("{:x}", slot1 as u32),
                ValueKind::String => {
                    let s = str_obj::as_str(slot1 as GcRef);
                    s.bytes().map(|b| format!("{:02x}", b)).collect()
                }
                _ => format!("%!x({})", format_interface(slot0, slot1)),
            }
        }
        'X' => {
            match vk {
                ValueKind::Int | ValueKind::Int64 | ValueKind::Uint | ValueKind::Uint64 => {
                    format!("{:X}", slot1)
                }
                ValueKind::Int8 | ValueKind::Uint8 => format!("{:X}", slot1 as u8),
                ValueKind::Int16 | ValueKind::Uint16 => format!("{:X}", slot1 as u16),
                ValueKind::Int32 | ValueKind::Uint32 => format!("{:X}", slot1 as u32),
                ValueKind::String => {
                    let s = str_obj::as_str(slot1 as GcRef);
                    s.bytes().map(|b| format!("{:02X}", b)).collect()
                }
                _ => format!("%!X({})", format_interface(slot0, slot1)),
            }
        }
        'p' => format!("0x{:x}", slot1),
        'q' => {
            match vk {
                ValueKind::String => {
                    let s = str_obj::as_str(slot1 as GcRef);
                    format!("{:?}", s)
                }
                _ => format!("%!q({})", format_interface(slot0, slot1)),
            }
        }
        'T' => value_kind_to_type_name(vk),
        _ => format!("%!{}({})", spec.verb, format_interface(slot0, slot1)),
    }
}

// =============================================================================
// Native extern functions
// =============================================================================

/// nativeWrite - write string to output (uses output.rs for std/no_std)
#[vostd_fn("fmt", "nativeWrite")]
fn native_write(call: &mut ExternCallContext) -> ExternResult {
    let s = call.arg_str(slots::ARG_S);
    vo_runtime::output::write(s);
    ExternResult::Ok
}

/// nativeSprint - format []interface{} with default format
#[vostd_fn("fmt", "nativeSprint")]
fn native_sprint(call: &mut ExternCallContext) -> ExternResult {
    let args_ref = call.arg_ref(slots::ARG_A);
    let formatted = format_args_slice_with_ctx(args_ref, Some(call));
    let gc = call.gc();
    let s = str_obj::from_rust_str(gc, &formatted);
    call.ret_ref(slots::RET_0, s);
    ExternResult::Ok
}

/// nativeSprintln - format []interface{} with newline
#[vostd_fn("fmt", "nativeSprintln")]
fn native_sprintln(call: &mut ExternCallContext) -> ExternResult {
    let args_ref = call.arg_ref(slots::ARG_A);
    let mut formatted = format_args_slice_with_ctx(args_ref, Some(call));
    formatted.push('\n');
    let gc = call.gc();
    let s = str_obj::from_rust_str(gc, &formatted);
    call.ret_ref(slots::RET_0, s);
    ExternResult::Ok
}

/// nativeSprintf - format with format string
#[vostd_fn("fmt", "nativeSprintf")]
fn native_sprintf(call: &mut ExternCallContext) -> ExternResult {
    let format_str = call.arg_str(slots::ARG_FORMAT);
    let args_ref = call.arg_ref(slots::ARG_A);
    let formatted = sprintf_impl(format_str, args_ref, Some(call));
    let gc = call.gc();
    let s = str_obj::from_rust_str(gc, &formatted);
    call.ret_ref(slots::RET_0, s);
    ExternResult::Ok
}

vo_runtime::stdlib_register!(fmt: nativeWrite, nativeSprint, nativeSprintln, nativeSprintf);
