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

/// Format []interface{} for Sprint: space only between non-string adjacent operands.
fn sprint_format_args(slice_ref: GcRef, call: Option<&ExternCallContext>) -> String {
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
        let slot0 = unsafe { *data_ptr.add(i * 2) };
        let slot1 = unsafe { *data_ptr.add(i * 2 + 1) };
        if i > 0 {
            let prev_slot0 = unsafe { *data_ptr.add((i - 1) * 2) };
            let prev_vk = interface::unpack_value_kind(prev_slot0);
            let cur_vk = interface::unpack_value_kind(slot0);
            if prev_vk != ValueKind::String && cur_vk != ValueKind::String {
                result.push(' ');
            }
        }
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
    hash: bool,
    space: bool,
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
            Some('-') => { flags.left = true; let _ = chars.next(); }
            Some('+') => { flags.plus = true; let _ = chars.next(); }
            Some('0') => { flags.zero = true; let _ = chars.next(); }
            Some('#') => { flags.hash = true; let _ = chars.next(); }
            Some(' ') => { flags.space = true; let _ = chars.next(); }
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

/// Extract (is_negative, magnitude) for base-format verbs (%b/%o/%x/%X) on integers.
/// Signed types use signed magnitude (Go semantics); unsigned types use raw truncated value.
fn int_magnitude(slot1: u64, vk: ValueKind) -> (bool, u64) {
    if vk.is_signed_int() {
        let v: i64 = match vk {
            ValueKind::Int8  => (slot1 as i8)  as i64,
            ValueKind::Int16 => (slot1 as i16) as i64,
            ValueKind::Int32 => (slot1 as i32) as i64,
            _ => slot1 as i64,
        };
        if v < 0 { (true, v.unsigned_abs()) } else { (false, v as u64) }
    } else {
        let m: u64 = match vk {
            ValueKind::Uint8  => slot1 as u8  as u64,
            ValueKind::Uint16 => slot1 as u16 as u64,
            ValueKind::Uint32 => slot1 as u32 as u64,
            _ => slot1,
        };
        (false, m)
    }
}

/// Post-process Rust's "{:.*e}" output to match Go's exponent format (e+00 / e-02).
/// Rust produces "1.2e0" or "1.2e-10"; Go requires sign + at least 2 exponent digits.
fn fmt_exp_go(s: &str, verb: char) -> String {
    let e_char = if verb == 'E' { 'E' } else { 'e' };
    if let Some(pos) = s.find(|c| c == 'e' || c == 'E') {
        let mantissa = &s[..pos];
        let exp: i32 = s[pos + 1..].parse().unwrap_or(0);
        format!("{}{}{:+03}", mantissa, e_char, exp)
    } else {
        s.to_string()
    }
}

fn format_with_spec(spec: FormatSpec, slot0: u64, slot1: u64, call: Option<&ExternCallContext>) -> String {
    let vk = interface::unpack_value_kind(slot0);

    match spec.verb {
        'v' => pad_width(format_interface_with_ctx(slot0, slot1, call), spec, ' '),
        'd' => {
            if vk.is_integer() {
                // Use signed or unsigned representation based on type.
                let mut s = if vk.is_signed_int() {
                    (slot1 as i64).to_string()
                } else {
                    slot1.to_string()
                };
                if spec.flags.plus && !s.starts_with('-') {
                    s = format!("+{}", s);
                } else if spec.flags.space && !s.starts_with('-') {
                    s = format!(" {}", s);
                }

                if let Some(prec) = spec.precision {
                    let (sign, digits) = if s.starts_with('-') {
                        ('-', &s[1..])
                    } else if s.starts_with('+') {
                        ('+', &s[1..])
                    } else if s.starts_with(' ') {
                        (' ', &s[1..])
                    } else {
                        ('\0', s.as_str())
                    };
                    let digit_len = digits.chars().count();
                    if digit_len < prec {
                        let mut out = String::new();
                        if sign != '\0' { out.push(sign); }
                        for _ in 0..(prec - digit_len) { out.push('0'); }
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
            let mut s = match vk {
                ValueKind::String => str_obj::as_str(slot1 as GcRef).to_string(),
                _ => format_interface_with_ctx(slot0, slot1, call),
            };
            if let Some(prec) = spec.precision {
                s = s.chars().take(prec).collect();
            }
            pad_width(s, spec, ' ')
        }
        'f' => {
            match vk {
                ValueKind::Float32 | ValueKind::Float64 => {
                    let f = match vk {
                        ValueKind::Float32 => f32::from_bits(slot1 as u32) as f64,
                        _ => f64::from_bits(slot1),
                    };
                    let prec = spec.precision.unwrap_or(6);
                    let mut s = if spec.flags.plus { format!("{:+.*}", prec, f) }
                                else if spec.flags.space && f >= 0.0 { format!(" {:.*}", prec, f) }
                                else { format!("{:.*}", prec, f) };
                    s = pad_numeric(s, spec);
                    s
                }
                _ => format!("%!f({})", format_interface(slot0, slot1)),
            }
        }
        'e' | 'E' => {
            match vk {
                ValueKind::Float32 | ValueKind::Float64 => {
                    let f = match vk {
                        ValueKind::Float32 => f32::from_bits(slot1 as u32) as f64,
                        _ => f64::from_bits(slot1),
                    };
                    let prec = spec.precision.unwrap_or(6);
                    // Build mantissa with optional sign prefix, then fix exponent to Go format.
                    let raw = if spec.verb == 'E' { format!("{:.*E}", prec, f) }
                              else { format!("{:.*e}", prec, f) };
                    let mut s = fmt_exp_go(&raw, spec.verb);
                    if spec.flags.plus && !s.starts_with('-') && !s.starts_with('+') {
                        s = format!("+{}", s);
                    } else if spec.flags.space && !s.starts_with('-') && !s.starts_with('+') {
                        s = format!(" {}", s);
                    }
                    pad_numeric(s, spec)
                }
                _ => format!("%!{}({})", spec.verb, format_interface(slot0, slot1)),
            }
        }
        'g' | 'G' => {
            match vk {
                ValueKind::Float32 | ValueKind::Float64 => {
                    let f = match vk {
                        ValueKind::Float32 => f32::from_bits(slot1 as u32) as f64,
                        _ => f64::from_bits(slot1),
                    };
                    // Go %g: use %e if exponent < -4 or >= prec, else %f. Strip trailing zeros.
                    let prec = spec.precision.unwrap_or(6).max(1);
                    let exp = if f == 0.0 { 0i32 } else { f.abs().log10().floor() as i32 };
                    let s = if exp < -4 || exp >= prec as i32 {
                        // Use %e format with prec-1 decimal places, then strip trailing zeros.
                        let raw = if spec.verb == 'G' { format!("{:.*E}", prec - 1, f) }
                                  else { format!("{:.*e}", prec - 1, f) };
                        let s = fmt_exp_go(&raw, spec.verb);
                        // Strip trailing zeros from mantissa.
                        if let Some(e_pos) = s.find(|c| c == 'e' || c == 'E') {
                            let mant = s[..e_pos].trim_end_matches('0').trim_end_matches('.');
                            format!("{}{}", mant, &s[e_pos..])
                        } else { s }
                    } else {
                        // Use %f format with enough sig digits, strip trailing zeros.
                        let dec_places = (prec as i32 - 1 - exp).max(0) as usize;
                        let raw = format!("{:.*}", dec_places, f);
                        // Strip trailing zeros after decimal.
                        if raw.contains('.') {
                            raw.trim_end_matches('0').trim_end_matches('.').to_string()
                        } else { raw }
                    };
                    let mut s = s;
                    if spec.flags.plus && !s.starts_with('-') && !s.starts_with('+') {
                        s = format!("+{}", s);
                    } else if spec.flags.space && !s.starts_with('-') && !s.starts_with('+') {
                        s = format!(" {}", s);
                    }
                    pad_numeric(s, spec)
                }
                _ => format!("%!{}({})", spec.verb, format_interface(slot0, slot1)),
            }
        }
        't' => {
            match vk {
                ValueKind::Bool => pad_width(
                    if slot1 != 0 { "true" } else { "false" }.to_string(),
                    spec, ' ',
                ),
                _ => format!("%!t({})", format_interface(slot0, slot1)),
            }
        }
        'b' => {
            if vk.is_integer() {
                let (neg, mag) = int_magnitude(slot1, vk);
                let raw = format!("{:b}", mag);
                let s = match (neg, spec.flags.hash) {
                    (true,  true)  => format!("-0b{}", raw),
                    (false, true)  => format!("0b{}", raw),
                    (true,  false) => format!("-{}", raw),
                    (false, false) => raw,
                };
                pad_numeric(s, spec)
            } else {
                match vk {
                    ValueKind::Float32 | ValueKind::Float64 => {
                        // %b on float: exponent-free binary
                        let f = match vk {
                            ValueKind::Float32 => f32::from_bits(slot1 as u32) as f64,
                            _ => f64::from_bits(slot1),
                        };
                        let s = format!("{:e}", f).replace("e", "p");
                        pad_width(s, spec, ' ')
                    }
                    _ => format!("%!b({})", format_interface(slot0, slot1)),
                }
            }
        }
        'o' | 'O' => {
            if vk.is_integer() {
                let (neg, mag) = int_magnitude(slot1, vk);
                let raw = format!("{:o}", mag);
                let use_prefix = spec.flags.hash || spec.verb == 'O';
                let s = match (neg, use_prefix) {
                    (true,  true)  => format!("-0o{}", raw),
                    (false, true)  => format!("0o{}", raw),
                    (true,  false) => format!("-{}", raw),
                    (false, false) => raw,
                };
                pad_numeric(s, spec)
            } else {
                format!("%!{}({})", spec.verb, format_interface(slot0, slot1))
            }
        }
        'x' => {
            match vk {
                ValueKind::String => {
                    let s = str_obj::as_str(slot1 as GcRef);
                    let hex: String = s.bytes().map(|b| format!("{:02x}", b)).collect();
                    pad_width(hex, spec, ' ')
                }
                _ if vk.is_integer() => {
                    let (neg, mag) = int_magnitude(slot1, vk);
                    let raw = format!("{:x}", mag);
                    let s = match (neg, spec.flags.hash) {
                        (true,  true)  => format!("-0x{}", raw),
                        (false, true)  => format!("0x{}", raw),
                        (true,  false) => format!("-{}", raw),
                        (false, false) => raw,
                    };
                    pad_numeric(s, spec)
                }
                _ => format!("%!x({})", format_interface(slot0, slot1)),
            }
        }
        'X' => {
            match vk {
                ValueKind::String => {
                    let s = str_obj::as_str(slot1 as GcRef);
                    let hex: String = s.bytes().map(|b| format!("{:02X}", b)).collect();
                    pad_width(hex, spec, ' ')
                }
                _ if vk.is_integer() => {
                    let (neg, mag) = int_magnitude(slot1, vk);
                    let raw = format!("{:X}", mag);
                    let s = match (neg, spec.flags.hash) {
                        (true,  true)  => format!("-0X{}", raw),
                        (false, true)  => format!("0X{}", raw),
                        (true,  false) => format!("-{}", raw),
                        (false, false) => raw,
                    };
                    pad_numeric(s, spec)
                }
                _ => format!("%!X({})", format_interface(slot0, slot1)),
            }
        }
        'c' => {
            if vk.is_integer() {
                let ch = char::from_u32(slot1 as u32).unwrap_or('\u{FFFD}');
                pad_width(ch.to_string(), spec, ' ')
            } else {
                format!("%!c({})", format_interface(slot0, slot1))
            }
        }
        'p' => pad_width(format!("0x{:x}", slot1), spec, ' '),
        'q' => {
            match vk {
                ValueKind::String => {
                    let s = str_obj::as_str(slot1 as GcRef);
                    pad_width(format!("{:?}", s), spec, ' ')
                }
                _ if vk.is_integer() => {
                    let ch = char::from_u32(slot1 as u32).unwrap_or('\u{FFFD}');
                    pad_width(format!("{:?}", ch), spec, ' ')
                }
                _ => format!("%!q({})", format_interface(slot0, slot1)),
            }
        }
        'T' => pad_width(value_kind_to_type_name(vk), spec, ' '),
        _ => format!("%!{}({})", spec.verb, format_interface(slot0, slot1)),
    }
}

// =============================================================================
// Scan support functions
// =============================================================================

/// Create a []any slice from a Vec of InterfaceSlots.
fn create_any_slice(gc: &mut vo_runtime::gc::Gc, items: &[interface::InterfaceSlot]) -> GcRef {
    use vo_common_core::types::ValueMeta;
    use vo_runtime::objects::slice;

    let len = items.len();
    let elem_meta = ValueMeta::new(0, ValueKind::Interface);
    let new_slice = slice::create(gc, elem_meta, 16, len, len);
    if len > 0 {
        let dst_ptr = slice::data_ptr(new_slice) as *mut u64;
        for (i, item) in items.iter().enumerate() {
            unsafe {
                *dst_ptr.add(i * 2) = item.slot0;
                *dst_ptr.add(i * 2 + 1) = item.slot1;
            }
        }
    }
    new_slice
}

/// Parse a format string and scan input accordingly, returning typed InterfaceSlots.
fn sscanf_impl_scan(
    input: &str,
    format: &str,
    gc: &mut vo_runtime::gc::Gc,
) -> Result<Vec<interface::InterfaceSlot>, String> {
    let mut result = Vec::new();
    let mut chars = format.chars().peekable();
    let mut input_pos = 0;
    let input_bytes = input.as_bytes();

    while let Some(c) = chars.next() {
        if c == '%' {
            if chars.peek() == Some(&'%') {
                chars.next();
                if input_pos >= input.len() || input_bytes[input_pos] != b'%' {
                    return Err(format!("expected '%%' at position {}", input_pos));
                }
                input_pos += 1;
                continue;
            }

            let verb = match chars.next() {
                Some(v) => v,
                None => return Err("incomplete format verb".into()),
            };

            // Skip whitespace in input before parsing
            while input_pos < input.len() && input_bytes[input_pos].is_ascii_whitespace() {
                input_pos += 1;
            }

            if input_pos >= input.len() {
                return Err(format!("unexpected end of input for %{}", verb));
            }

            match verb {
                'd' => {
                    let start = input_pos;
                    if input_pos < input.len()
                        && (input_bytes[input_pos] == b'-' || input_bytes[input_pos] == b'+')
                    {
                        input_pos += 1;
                    }
                    while input_pos < input.len() && input_bytes[input_pos].is_ascii_digit() {
                        input_pos += 1;
                    }
                    let token = &input[start..input_pos];
                    let val: i64 = token
                        .parse()
                        .map_err(|e| format!("expected integer for %d: {}", e))?;
                    result.push(interface::InterfaceSlot::from_i64(val));
                }
                'f' | 'e' | 'g' => {
                    let start = input_pos;
                    if input_pos < input.len()
                        && (input_bytes[input_pos] == b'-' || input_bytes[input_pos] == b'+')
                    {
                        input_pos += 1;
                    }
                    while input_pos < input.len() {
                        let b = input_bytes[input_pos];
                        if b.is_ascii_digit()
                            || b == b'.'
                            || b == b'e'
                            || b == b'E'
                            || ((b == b'-' || b == b'+') && input_pos > start)
                        {
                            input_pos += 1;
                        } else {
                            break;
                        }
                    }
                    let token = &input[start..input_pos];
                    let val: f64 = token
                        .parse()
                        .map_err(|e| format!("expected float for %{}: {}", verb, e))?;
                    result.push(interface::InterfaceSlot::from_f64(val));
                }
                's' => {
                    let start = input_pos;
                    while input_pos < input.len()
                        && !input_bytes[input_pos].is_ascii_whitespace()
                    {
                        input_pos += 1;
                    }
                    let token = &input[start..input_pos];
                    let str_ref = str_obj::from_rust_str(gc, token);
                    result.push(interface::InterfaceSlot::from_ref(
                        str_ref,
                        ValueKind::String as u32,
                        ValueKind::String,
                    ));
                }
                't' => {
                    let start = input_pos;
                    while input_pos < input.len()
                        && !input_bytes[input_pos].is_ascii_whitespace()
                    {
                        input_pos += 1;
                    }
                    let token = &input[start..input_pos];
                    let val = match token {
                        "true" | "TRUE" | "1" => true,
                        "false" | "FALSE" | "0" => false,
                        _ => return Err(format!("expected bool for %t, got '{}'", token)),
                    };
                    result.push(interface::InterfaceSlot::from_bool(val));
                }
                'c' => {
                    let ch = input[input_pos..]
                        .chars()
                        .next()
                        .ok_or_else(|| "expected character for %c".to_string())?;
                    input_pos += ch.len_utf8();
                    result.push(interface::InterfaceSlot::from_i64(ch as i64));
                }
                'x' | 'X' => {
                    let start = input_pos;
                    if input_pos + 1 < input.len()
                        && input_bytes[input_pos] == b'0'
                        && (input_bytes[input_pos + 1] == b'x'
                            || input_bytes[input_pos + 1] == b'X')
                    {
                        input_pos += 2;
                    }
                    while input_pos < input.len()
                        && input_bytes[input_pos].is_ascii_hexdigit()
                    {
                        input_pos += 1;
                    }
                    let token = &input[start..input_pos];
                    let clean = token
                        .strip_prefix("0x")
                        .or_else(|| token.strip_prefix("0X"))
                        .unwrap_or(token);
                    let val = i64::from_str_radix(clean, 16)
                        .map_err(|e| format!("expected hex for %x: {}", e))?;
                    result.push(interface::InterfaceSlot::from_i64(val));
                }
                'o' => {
                    let start = input_pos;
                    if input_pos + 1 < input.len()
                        && input_bytes[input_pos] == b'0'
                        && (input_bytes[input_pos + 1] == b'o'
                            || input_bytes[input_pos + 1] == b'O')
                    {
                        input_pos += 2;
                    } else if input_pos < input.len() && input_bytes[input_pos] == b'0' {
                        input_pos += 1;
                    }
                    while input_pos < input.len()
                        && input_bytes[input_pos] >= b'0'
                        && input_bytes[input_pos] <= b'7'
                    {
                        input_pos += 1;
                    }
                    let token = &input[start..input_pos];
                    let clean = token
                        .strip_prefix("0o")
                        .or_else(|| token.strip_prefix("0O"))
                        .unwrap_or(token)
                        .trim_start_matches('0');
                    let val = if clean.is_empty() {
                        0
                    } else {
                        i64::from_str_radix(clean, 8)
                            .map_err(|e| format!("expected octal for %o: {}", e))?
                    };
                    result.push(interface::InterfaceSlot::from_i64(val));
                }
                'b' => {
                    let start = input_pos;
                    if input_pos + 1 < input.len()
                        && input_bytes[input_pos] == b'0'
                        && (input_bytes[input_pos + 1] == b'b'
                            || input_bytes[input_pos + 1] == b'B')
                    {
                        input_pos += 2;
                    }
                    while input_pos < input.len()
                        && (input_bytes[input_pos] == b'0' || input_bytes[input_pos] == b'1')
                    {
                        input_pos += 1;
                    }
                    let token = &input[start..input_pos];
                    let clean = token
                        .strip_prefix("0b")
                        .or_else(|| token.strip_prefix("0B"))
                        .unwrap_or(token);
                    let val = if clean.is_empty() {
                        0
                    } else {
                        i64::from_str_radix(clean, 2)
                            .map_err(|e| format!("expected binary for %b: {}", e))?
                    };
                    result.push(interface::InterfaceSlot::from_i64(val));
                }
                'v' => {
                    let start = input_pos;
                    while input_pos < input.len()
                        && !input_bytes[input_pos].is_ascii_whitespace()
                    {
                        input_pos += 1;
                    }
                    let token = &input[start..input_pos];
                    let str_ref = str_obj::from_rust_str(gc, token);
                    result.push(interface::InterfaceSlot::from_ref(
                        str_ref,
                        ValueKind::String as u32,
                        ValueKind::String,
                    ));
                }
                _ => return Err(format!("unsupported scan verb '%{}'", verb)),
            }
        } else if c.is_ascii_whitespace() {
            // Skip whitespace in both format and input
            while input_pos < input.len() && input_bytes[input_pos].is_ascii_whitespace() {
                input_pos += 1;
            }
        } else {
            // Match literal character
            if input_pos >= input.len() {
                return Err(format!("expected '{}' at position {}", c, input_pos));
            }
            let input_char = input[input_pos..].chars().next().unwrap();
            if input_char != c {
                return Err(format!("expected '{}', got '{}'", c, input_char));
            }
            input_pos += input_char.len_utf8();
        }
    }

    Ok(result)
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

/// nativeSprint - format []interface{} with default format (Sprint semantics)
#[vostd_fn("fmt", "nativeSprint")]
fn native_sprint(call: &mut ExternCallContext) -> ExternResult {
    let args_ref = call.arg_ref(slots::ARG_A);
    let formatted = sprint_format_args(args_ref, Some(call));
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

/// nativeSscan - split string by whitespace, return []any of strings
#[vostd_fn("fmt", "nativeSscan")]
fn native_sscan(call: &mut ExternCallContext) -> ExternResult {
    let input = call.arg_str(slots::ARG_STR).to_string();
    let tokens: Vec<&str> = input.split_whitespace().collect();
    let gc = call.gc();
    let items: Vec<interface::InterfaceSlot> = tokens
        .iter()
        .map(|t| {
            let str_ref = str_obj::from_rust_str(gc, t);
            interface::InterfaceSlot::from_ref(str_ref, ValueKind::String as u32, ValueKind::String)
        })
        .collect();
    let slice_ref = create_any_slice(gc, &items);
    call.ret_ref(slots::RET_0, slice_ref);
    ExternResult::Ok
}

/// nativeSscanf - scan string with format, return ([]any, error)
#[vostd_fn("fmt", "nativeSscanf")]
fn native_sscanf(call: &mut ExternCallContext) -> ExternResult {
    let input = call.arg_str(slots::ARG_STR).to_string();
    let format = call.arg_str(slots::ARG_FORMAT).to_string();
    let gc = call.gc();
    match sscanf_impl_scan(&input, &format, gc) {
        Ok(items) => {
            let slice_ref = create_any_slice(gc, &items);
            call.ret_ref(slots::RET_0, slice_ref);
            call.ret_nil_error(slots::RET_1);
        }
        Err(msg) => {
            call.ret_ref(slots::RET_0, core::ptr::null_mut());
            call.ret_error_msg(slots::RET_1, &msg);
        }
    }
    ExternResult::Ok
}

/// nativeReadLine - read a line from stdin
#[cfg(feature = "std")]
#[vostd_fn("fmt", "nativeReadLine", std)]
fn native_read_line(call: &mut ExternCallContext) -> ExternResult {
    use std::io::BufRead;
    let stdin = std::io::stdin();
    let mut line = String::new();
    match stdin.lock().read_line(&mut line) {
        Ok(0) => {
            // EOF: stdin closed
            let gc = call.gc();
            let empty = str_obj::from_rust_str(gc, "");
            call.ret_ref(slots::RET_0, empty);
            call.ret_error_msg(slots::RET_1, "EOF");
        }
        Ok(_) => {
            // Trim trailing newline
            if line.ends_with('\n') {
                line.pop();
                if line.ends_with('\r') {
                    line.pop();
                }
            }
            let gc = call.gc();
            let str_ref = str_obj::from_rust_str(gc, &line);
            call.ret_ref(slots::RET_0, str_ref);
            call.ret_nil_error(slots::RET_1);
        }
        Err(e) => {
            let gc = call.gc();
            let empty = str_obj::from_rust_str(gc, "");
            call.ret_ref(slots::RET_0, empty);
            call.ret_error_msg(slots::RET_1, &e.to_string());
        }
    }
    ExternResult::Ok
}

pub fn register_externs(
    registry: &mut vo_runtime::ffi::ExternRegistry,
    externs: &[vo_runtime::bytecode::ExternDef],
) {
    const BASE_TABLE: &[vo_runtime::ffi::StdlibEntry] = &[
        __STDLIB_fmt_nativeWrite,
        __STDLIB_fmt_nativeSprint,
        __STDLIB_fmt_nativeSprintln,
        __STDLIB_fmt_nativeSprintf,
        __STDLIB_fmt_nativeSscan,
        __STDLIB_fmt_nativeSscanf,
    ];
    for (id, def) in externs.iter().enumerate() {
        for entry in BASE_TABLE {
            if def.name == entry.name() {
                entry.register(registry, id as u32);
                break;
            }
        }
        #[cfg(feature = "std")]
        if def.name == __STDLIB_fmt_nativeReadLine.name() {
            __STDLIB_fmt_nativeReadLine.register(registry, id as u32);
        }
    }
}
