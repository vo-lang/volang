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
use alloc::format;
#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use vo_common_core::types::ValueKind;
use vo_ffi_macro::vostd_fn;
use vo_runtime::ffi::{ExternCallContext, ExternResult};
use vo_runtime::gc::GcRef;
use vo_runtime::objects::{interface, slice, string as str_obj};

// =============================================================================
// Format interface{} values - re-export from vo_runtime::builtins
// =============================================================================

pub use vo_runtime::builtins::{
    format_interface, format_interface_bytes, format_interface_bytes_with_ctx,
    format_interface_with_ctx,
};

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
        ValueKind::Port => "port ...".to_string(),
        ValueKind::Closure => "func(...)".to_string(),
        ValueKind::Pointer => "*...".to_string(),
        ValueKind::Array => "[...]...".to_string(),
        ValueKind::Struct => "struct{...}".to_string(),
        ValueKind::Interface => "interface{}".to_string(),
        ValueKind::Island => "island".to_string(),
    }
}

/// Format all elements in a []interface{} slice with space separator.
fn format_args_slice_with_ctx(slice_ref: GcRef, call: Option<&ExternCallContext>) -> Vec<u8> {
    if slice_ref.is_null() {
        return Vec::new();
    }

    // Safety: callers pass a rooted []any argument from the extern ABI.
    let len = unsafe { slice::len(slice_ref) };
    if len == 0 {
        return Vec::new();
    }

    let data_ptr = unsafe { slice::data_ptr(slice_ref) } as *const u64;
    let mut result = Vec::new();

    for i in 0..len {
        if i > 0 {
            result.push(b' ');
        }
        // Each interface{} is 2 slots (16 bytes)
        let slot0 = unsafe { *data_ptr.add(i * 2) };
        let slot1 = unsafe { *data_ptr.add(i * 2 + 1) };
        result.extend_from_slice(&format_interface_bytes_with_ctx(slot0, slot1, call));
    }

    result
}

/// Format []interface{} for Sprint: space only between non-string adjacent operands.
fn sprint_format_args(slice_ref: GcRef, call: Option<&ExternCallContext>) -> Vec<u8> {
    if slice_ref.is_null() {
        return Vec::new();
    }
    // Safety: callers pass a rooted []any argument from the extern ABI.
    let len = unsafe { slice::len(slice_ref) };
    if len == 0 {
        return Vec::new();
    }
    let data_ptr = unsafe { slice::data_ptr(slice_ref) } as *const u64;
    let mut result = Vec::new();
    for i in 0..len {
        let slot0 = unsafe { *data_ptr.add(i * 2) };
        let slot1 = unsafe { *data_ptr.add(i * 2 + 1) };
        if i > 0 {
            let prev_slot0 = unsafe { *data_ptr.add((i - 1) * 2) };
            let prev_vk = interface::unpack_value_kind(prev_slot0);
            let cur_vk = interface::unpack_value_kind(slot0);
            if prev_vk != ValueKind::String && cur_vk != ValueKind::String {
                result.push(b' ');
            }
        }
        result.extend_from_slice(&format_interface_bytes_with_ctx(slot0, slot1, call));
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

/// Match Go's formatter safety bound. A literal width or precision above this
/// limit is diagnosed instead of being allowed to drive an unbounded
/// allocation.
const MAX_FORMAT_SIZE: usize = 1_000_000;

#[derive(Clone, Copy, Default)]
struct ParsedFormatSpec {
    spec: FormatSpec,
    bad_width: bool,
    bad_precision: bool,
}

fn parse_number(format: &[u8], offset: &mut usize) -> (Option<usize>, bool) {
    let mut n: usize = 0;
    let mut any = false;
    let mut too_large = false;
    while let Some(&byte) = format.get(*offset) {
        if !byte.is_ascii_digit() {
            break;
        }
        any = true;
        *offset += 1;
        if !too_large {
            n = n
                .checked_mul(10)
                .and_then(|value| value.checked_add((byte - b'0') as usize))
                .unwrap_or(MAX_FORMAT_SIZE + 1);
            too_large = n > MAX_FORMAT_SIZE;
        }
    }
    if any {
        (if too_large { None } else { Some(n) }, too_large)
    } else {
        (None, false)
    }
}

fn parse_format_spec(format: &[u8], offset: &mut usize) -> Option<ParsedFormatSpec> {
    let mut flags = FormatFlags::default();

    loop {
        match format.get(*offset).copied() {
            Some(b'-') => {
                flags.left = true;
                *offset += 1;
            }
            Some(b'+') => {
                flags.plus = true;
                *offset += 1;
            }
            Some(b'0') => {
                flags.zero = true;
                *offset += 1;
            }
            Some(b'#') => {
                flags.hash = true;
                *offset += 1;
            }
            Some(b' ') => {
                flags.space = true;
                *offset += 1;
            }
            _ => break,
        }
    }

    let (width, bad_width) = parse_number(format, offset);

    let (precision, bad_precision) = if format.get(*offset).copied() == Some(b'.') {
        *offset += 1;
        let (precision, too_large) = parse_number(format, offset);
        (
            if too_large {
                None
            } else {
                Some(precision.unwrap_or(0))
            },
            too_large,
        )
    } else {
        (None, false)
    };

    let verb = char::from(*format.get(*offset)?);
    *offset += 1;
    Some(ParsedFormatSpec {
        spec: FormatSpec {
            flags,
            width,
            precision,
            verb,
        },
        bad_width,
        bad_precision,
    })
}

fn truncate_runes(bytes: &[u8], limit: usize) -> &[u8] {
    let mut offset = 0;
    for _ in 0..limit {
        if offset == bytes.len() {
            break;
        }
        let (_, width) = crate::raw_utf8::decode_first(&bytes[offset..]);
        offset += width;
    }
    &bytes[..offset]
}

fn pad_bytes(bytes: &[u8], spec: FormatSpec, default_pad: u8) -> Vec<u8> {
    let Some(width) = spec.width else {
        return bytes.to_vec();
    };
    let len = crate::raw_utf8::rune_count(bytes);
    if len >= width {
        return bytes.to_vec();
    }
    let pad = if spec.flags.zero && !spec.flags.left {
        b'0'
    } else {
        default_pad
    };
    let pad_len = width - len;
    let mut result = Vec::with_capacity(bytes.len() + pad_len);
    if spec.flags.left {
        result.extend_from_slice(bytes);
        result.resize(result.len() + pad_len, pad);
    } else {
        result.resize(pad_len, pad);
        result.extend_from_slice(bytes);
    }
    result
}

fn push_hex_escape(result: &mut Vec<u8>, byte: u8) {
    const HEX: &[u8; 16] = b"0123456789abcdef";
    result.extend_from_slice(b"\\x");
    result.push(HEX[(byte >> 4) as usize]);
    result.push(HEX[(byte & 0x0f) as usize]);
}

fn quote_bytes(bytes: &[u8]) -> Vec<u8> {
    let mut result = Vec::with_capacity(bytes.len() + 2);
    result.push(b'"');
    let mut offset = 0;
    while offset < bytes.len() {
        let byte = bytes[offset];
        if byte.is_ascii() {
            match byte {
                b'"' => result.extend_from_slice(b"\\\""),
                b'\\' => result.extend_from_slice(b"\\\\"),
                b'\n' => result.extend_from_slice(b"\\n"),
                b'\r' => result.extend_from_slice(b"\\r"),
                b'\t' => result.extend_from_slice(b"\\t"),
                0x20..=0x7e => result.push(byte),
                _ => push_hex_escape(&mut result, byte),
            }
            offset += 1;
            continue;
        }

        let (_, width) = crate::raw_utf8::decode_first(&bytes[offset..]);
        let encoded = &bytes[offset..offset + width];
        if core::str::from_utf8(encoded).is_ok() {
            result.extend_from_slice(encoded);
        } else {
            push_hex_escape(&mut result, byte);
        }
        offset += width;
    }
    result.push(b'"');
    result
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
    let pad_char = if spec.flags.zero && !spec.flags.left {
        '0'
    } else {
        default_pad
    };
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
            if first == '-' || first == '+' || first == ' ' {
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
fn sprintf_impl(format_bytes: &[u8], args_ref: GcRef, call: Option<&ExternCallContext>) -> Vec<u8> {
    let args_len = if args_ref.is_null() {
        0
    } else {
        // Safety: `args_ref` is the rooted []any argument for this format call.
        unsafe { slice::len(args_ref) }
    };
    let data_ptr = if args_ref.is_null() {
        core::ptr::null()
    } else {
        (unsafe { slice::data_ptr(args_ref) }) as *const u64
    };

    let mut result = Vec::new();
    let mut offset = 0;
    let mut arg_idx = 0usize;

    while let Some(&byte) = format_bytes.get(offset) {
        offset += 1;
        if byte != b'%' {
            result.push(byte);
            continue;
        }

        if format_bytes.get(offset).copied() == Some(b'%') {
            offset += 1;
            result.push(b'%');
            continue;
        }

        let Some(parsed) = parse_format_spec(format_bytes, &mut offset) else {
            result.extend_from_slice(b"%!(NOVERB)");
            break;
        };
        let spec = parsed.spec;

        if parsed.bad_width {
            result.extend_from_slice(b"%!(BADWIDTH)");
        }
        if parsed.bad_precision {
            result.extend_from_slice(b"%!(BADPREC)");
        }

        if arg_idx >= args_len {
            result.extend_from_slice(b"%!");
            result.push(spec.verb as u8);
            result.extend_from_slice(b"(MISSING)");
            continue;
        }

        let slot0 = unsafe { *data_ptr.add(arg_idx * 2) };
        let slot1 = unsafe { *data_ptr.add(arg_idx * 2 + 1) };
        result.extend_from_slice(&format_with_spec_bytes(spec, slot0, slot1, call));
        arg_idx += 1;
    }

    if arg_idx < args_len {
        result.extend_from_slice(b"%!(EXTRA ");
        for i in arg_idx..args_len {
            if i > arg_idx {
                result.extend_from_slice(b", ");
            }
            let slot0 = unsafe { *data_ptr.add(i * 2) };
            let slot1 = unsafe { *data_ptr.add(i * 2 + 1) };
            if interface::is_nil(slot0) {
                result.extend_from_slice(b"<nil>");
                continue;
            }
            let vk = interface::unpack_value_kind(slot0);
            result.extend_from_slice(value_kind_to_type_name(vk).as_bytes());
            result.push(b'=');
            result.extend_from_slice(&format_interface_bytes_with_ctx(slot0, slot1, call));
        }
        result.push(b')');
    }

    result
}

fn format_with_spec_bytes(
    spec: FormatSpec,
    slot0: u64,
    slot1: u64,
    call: Option<&ExternCallContext>,
) -> Vec<u8> {
    let vk = interface::unpack_value_kind(slot0);
    if !spec.verb.is_ascii() {
        let value = format_interface_bytes_with_ctx(slot0, slot1, call);
        let mut result = Vec::with_capacity(value.len() + 5);
        result.extend_from_slice(b"%!");
        result.push(spec.verb as u8);
        result.push(b'(');
        result.extend_from_slice(&value);
        result.push(b')');
        return result;
    }
    if spec.verb == 'v' || (spec.verb == 's' && vk != ValueKind::String) {
        let bytes = format_interface_bytes_with_ctx(slot0, slot1, call);
        let bytes = spec.precision.map_or(bytes.as_slice(), |precision| {
            truncate_runes(&bytes, precision)
        });
        return pad_bytes(bytes, spec, b' ');
    }
    if vk != ValueKind::String {
        return format_with_spec(spec, slot0, slot1, call).into_bytes();
    }

    // Safety: the interface metadata identifies `slot1` as a live string.
    let bytes = unsafe { str_obj::to_bytes(slot1 as GcRef) };
    match spec.verb {
        's' => {
            let bytes = spec.precision.map_or(bytes.as_slice(), |precision| {
                truncate_runes(&bytes, precision)
            });
            pad_bytes(bytes, spec, b' ')
        }
        'q' => {
            let bytes = spec.precision.map_or(bytes.as_slice(), |precision| {
                truncate_runes(&bytes, precision)
            });
            pad_bytes(&quote_bytes(bytes), spec, b' ')
        }
        'x' | 'X' => {
            let uppercase = spec.verb == 'X';
            let digits = if uppercase {
                b"0123456789ABCDEF"
            } else {
                b"0123456789abcdef"
            };
            let bytes = spec.precision.map_or(bytes.as_slice(), |precision| {
                &bytes[..bytes.len().min(precision)]
            });
            let per_byte = 2 + usize::from(spec.flags.space) + if spec.flags.hash { 2 } else { 0 };
            let mut encoded = Vec::with_capacity(bytes.len().saturating_mul(per_byte));
            if spec.flags.hash && !spec.flags.space && !bytes.is_empty() {
                encoded.extend_from_slice(if uppercase { b"0X" } else { b"0x" });
            }
            for (index, &byte) in bytes.iter().enumerate() {
                if spec.flags.space && index > 0 {
                    encoded.push(b' ');
                }
                if spec.flags.hash && spec.flags.space {
                    encoded.extend_from_slice(if uppercase { b"0X" } else { b"0x" });
                }
                encoded.push(digits[(byte >> 4) as usize]);
                encoded.push(digits[(byte & 0x0f) as usize]);
            }
            pad_bytes(&encoded, spec, b' ')
        }
        'T' => pad_bytes(value_kind_to_type_name(vk).as_bytes(), spec, b' '),
        _ => {
            let mut result = Vec::with_capacity(bytes.len() + 5);
            result.extend_from_slice(b"%!");
            result.push(spec.verb as u8);
            result.push(b'(');
            result.extend_from_slice(&bytes);
            result.push(b')');
            result
        }
    }
}

/// Normalize the slot representation to the integer value Go's formatter sees.
/// Narrow signed values are sign-extended; narrow unsigned values are truncated.
fn integer_bits(slot1: u64, vk: ValueKind) -> u64 {
    match vk {
        ValueKind::Int8 => (slot1 as i8 as i64) as u64,
        ValueKind::Int16 => (slot1 as i16 as i64) as u64,
        ValueKind::Int32 => (slot1 as i32 as i64) as u64,
        ValueKind::Uint8 => slot1 as u8 as u64,
        ValueKind::Uint16 => slot1 as u16 as u64,
        ValueKind::Uint32 => slot1 as u32 as u64,
        _ => slot1,
    }
}

/// Extract (is_negative, magnitude) for base-format verbs (%b/%o/%x/%X) on integers.
/// Signed types use signed magnitude (Go semantics); unsigned types use raw truncated value.
fn int_magnitude(slot1: u64, vk: ValueKind) -> (bool, u64) {
    let bits = integer_bits(slot1, vk);
    if vk.is_signed_int() {
        let v = bits as i64;
        if v < 0 {
            (true, v.unsigned_abs())
        } else {
            (false, v as u64)
        }
    } else {
        (false, bits)
    }
}

fn normalized_integer(slot1: u64, vk: ValueKind) -> (bool, u64) {
    int_magnitude(slot1, vk)
}

fn format_integer(
    spec: FormatSpec,
    slot1: u64,
    vk: ValueKind,
    base: u32,
    uppercase: bool,
) -> String {
    let (negative, magnitude) = normalized_integer(slot1, vk);
    let mut digits = match (base, uppercase) {
        (2, _) => format!("{:b}", magnitude),
        (8, _) => format!("{:o}", magnitude),
        (10, _) => magnitude.to_string(),
        (16, false) => format!("{:x}", magnitude),
        (16, true) => format!("{:X}", magnitude),
        _ => unreachable!("formatter only supports bases 2, 8, 10, and 16"),
    };

    if spec.precision == Some(0) && magnitude == 0 {
        digits.clear();
    } else if let Some(precision) = spec.precision {
        if digits.len() < precision {
            let mut padded = String::with_capacity(precision);
            for _ in 0..precision - digits.len() {
                padded.push('0');
            }
            padded.push_str(&digits);
            digits = padded;
        }
    }

    let sign = if digits.is_empty() {
        ""
    } else if negative {
        "-"
    } else if spec.flags.plus {
        "+"
    } else if spec.flags.space {
        " "
    } else {
        ""
    };
    let prefix = match (base, spec.verb, spec.flags.hash, digits.is_empty()) {
        (_, _, _, true) => "",
        (2, _, true, false) => "0b",
        (8, 'O', _, false) => "0o",
        // Go's %#o uses a single leading zero and never duplicates one
        // already produced by precision.
        (8, _, true, false) if !digits.starts_with('0') => "0",
        (16, _, true, false) if uppercase => "0X",
        (16, _, true, false) => "0x",
        _ => "",
    };

    let core_len = sign.len() + prefix.len() + digits.len();
    let width = spec.width.unwrap_or(0);
    let padding = width.saturating_sub(core_len);
    let zero_pad = spec.flags.zero && !spec.flags.left && spec.precision.is_none();
    let mut result = String::with_capacity(core_len + padding);
    if !spec.flags.left && !zero_pad {
        for _ in 0..padding {
            result.push(' ');
        }
    }
    result.push_str(sign);
    result.push_str(prefix);
    if zero_pad {
        for _ in 0..padding {
            result.push('0');
        }
    }
    result.push_str(&digits);
    if spec.flags.left {
        for _ in 0..padding {
            result.push(' ');
        }
    }
    result
}

fn integer_code_point(slot1: u64, vk: ValueKind) -> char {
    let (negative, magnitude) = normalized_integer(slot1, vk);
    if negative {
        return '\u{FFFD}';
    }
    u32::try_from(magnitude)
        .ok()
        .and_then(char::from_u32)
        .unwrap_or('\u{FFFD}')
}

fn format_unicode_code_point(slot1: u64, vk: ValueKind, spec: FormatSpec) -> String {
    let value = integer_bits(slot1, vk);
    let digits = format!("{value:X}");
    let precision = spec.precision.unwrap_or(4).max(4);
    let leading_zeroes = precision.saturating_sub(digits.len());
    let mut result = String::with_capacity(
        2 + leading_zeroes + digits.len() + if spec.flags.hash { 7 } else { 0 },
    );
    result.push_str("U+");
    result.extend(core::iter::repeat_n('0', leading_zeroes));
    result.push_str(&digits);

    if spec.flags.hash && value <= char::MAX as u64 && crate::unicode::is_print(value as i32) {
        // Go's %#U form deliberately places the printable rune between raw
        // single quotes; the rune itself is not escaped.
        let ch = char::from_u32(value as u32)
            .expect("printable Unicode value must be a valid scalar value");
        result.push(' ');
        result.push('\'');
        result.push(ch);
        result.push('\'');
    }

    // The zero, plus, and space flags have no effect for %U. Width and left
    // alignment still apply to the complete representation.
    let mut padding_spec = spec;
    padding_spec.flags.zero = false;
    pad_width(result, padding_spec, ' ')
}

fn format_binary_float(slot1: u64, vk: ValueKind, spec: FormatSpec) -> String {
    let (negative, exponent_bits, mantissa_bits, exponent_bias, bits) = match vk {
        ValueKind::Float32 => {
            let bits = slot1 as u32;
            (bits >> 31 != 0, 8u32, 23u32, -127i32, bits as u64)
        }
        ValueKind::Float64 => (slot1 >> 63 != 0, 11u32, 52u32, -1023i32, slot1),
        _ => unreachable!("binary float formatting requires a floating-point value"),
    };
    let exponent_mask = (1u64 << exponent_bits) - 1;
    let fraction_mask = (1u64 << mantissa_bits) - 1;
    let mut exponent = ((bits >> mantissa_bits) & exponent_mask) as i32;
    let mut mantissa = bits & fraction_mask;

    let mut value = if exponent == exponent_mask as i32 {
        if mantissa != 0 {
            if spec.flags.plus {
                "+NaN".to_string()
            } else if spec.flags.space {
                " NaN".to_string()
            } else {
                "NaN".to_string()
            }
        } else if negative {
            "-Inf".to_string()
        } else if spec.flags.plus {
            "+Inf".to_string()
        } else if spec.flags.space {
            " Inf".to_string()
        } else {
            "+Inf".to_string()
        }
    } else {
        if exponent == 0 {
            exponent = 1;
        } else {
            mantissa |= 1u64 << mantissa_bits;
        }
        exponent += exponent_bias - mantissa_bits as i32;
        let sign = if negative {
            "-"
        } else if spec.flags.plus {
            "+"
        } else if spec.flags.space {
            " "
        } else {
            ""
        };
        let exponent_sign = if exponent >= 0 { "+" } else { "" };
        format!("{}{}p{}{}", sign, mantissa, exponent_sign, exponent)
    };

    // Inf and NaN are never zero padded. Finite values follow numeric padding.
    if value.ends_with("Inf") || value == "NaN" {
        let mut no_zero_spec = spec;
        no_zero_spec.flags.zero = false;
        value = pad_numeric(value, no_zero_spec);
    } else {
        value = pad_numeric(value, spec);
    }
    value
}

/// Post-process Rust's "{:.*e}" output to match Go's exponent format (e+00 / e-02).
/// Rust produces "1.2e0" or "1.2e-10"; Go requires sign + at least 2 exponent digits.
fn fmt_exp_go(s: &str, verb: char) -> String {
    let e_char = if verb == 'E' { 'E' } else { 'e' };
    if let Some(pos) = s.find(['e', 'E']) {
        let mantissa = &s[..pos];
        let exp: i32 = s[pos + 1..].parse().unwrap_or(0);
        format!("{}{}{:+03}", mantissa, e_char, exp)
    } else {
        s.to_string()
    }
}

fn format_with_spec(
    spec: FormatSpec,
    slot0: u64,
    slot1: u64,
    call: Option<&ExternCallContext>,
) -> String {
    let vk = interface::unpack_value_kind(slot0);

    match spec.verb {
        'v' => pad_width(format_interface_with_ctx(slot0, slot1, call), spec, ' '),
        'd' => {
            if vk.is_integer() {
                format_integer(spec, slot1, vk, 10, false)
            } else {
                format!("%!d({})", format_interface(slot0, slot1))
            }
        }
        's' => {
            // String values take the byte-preserving path in
            // `format_with_spec_bytes`; this path formats scalar diagnostics.
            let mut s = format_interface_with_ctx(slot0, slot1, call);
            if let Some(prec) = spec.precision {
                s = s.chars().take(prec).collect();
            }
            pad_width(s, spec, ' ')
        }
        'f' => match vk {
            ValueKind::Float32 | ValueKind::Float64 => {
                let f = match vk {
                    ValueKind::Float32 => f32::from_bits(slot1 as u32) as f64,
                    _ => f64::from_bits(slot1),
                };
                let prec = spec.precision.unwrap_or(6);
                let mut s = if spec.flags.plus {
                    format!("{:+.*}", prec, f)
                } else if spec.flags.space && f >= 0.0 {
                    format!(" {:.*}", prec, f)
                } else {
                    format!("{:.*}", prec, f)
                };
                s = pad_numeric(s, spec);
                s
            }
            _ => format!("%!f({})", format_interface(slot0, slot1)),
        },
        'e' | 'E' => {
            match vk {
                ValueKind::Float32 | ValueKind::Float64 => {
                    let f = match vk {
                        ValueKind::Float32 => f32::from_bits(slot1 as u32) as f64,
                        _ => f64::from_bits(slot1),
                    };
                    let prec = spec.precision.unwrap_or(6);
                    // Build mantissa with optional sign prefix, then fix exponent to Go format.
                    let raw = if spec.verb == 'E' {
                        format!("{:.*E}", prec, f)
                    } else {
                        format!("{:.*e}", prec, f)
                    };
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
                    let exp = if f == 0.0 {
                        0i32
                    } else {
                        f.abs().log10().floor() as i32
                    };
                    let s = if exp < -4 || exp >= prec as i32 {
                        // Use %e format with prec-1 decimal places, then strip trailing zeros.
                        let raw = if spec.verb == 'G' {
                            format!("{:.*E}", prec - 1, f)
                        } else {
                            format!("{:.*e}", prec - 1, f)
                        };
                        let s = fmt_exp_go(&raw, spec.verb);
                        // Strip trailing zeros from mantissa.
                        if let Some(e_pos) = s.find(['e', 'E']) {
                            let mant = s[..e_pos].trim_end_matches('0').trim_end_matches('.');
                            format!("{}{}", mant, &s[e_pos..])
                        } else {
                            s
                        }
                    } else {
                        // Use %f format with enough sig digits, strip trailing zeros.
                        let dec_places = (prec as i32 - 1 - exp).max(0) as usize;
                        let raw = format!("{:.*}", dec_places, f);
                        // Strip trailing zeros after decimal.
                        if raw.contains('.') {
                            raw.trim_end_matches('0').trim_end_matches('.').to_string()
                        } else {
                            raw
                        }
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
        't' => match vk {
            ValueKind::Bool => pad_width(
                if slot1 != 0 { "true" } else { "false" }.to_string(),
                spec,
                ' ',
            ),
            _ => format!("%!t({})", format_interface(slot0, slot1)),
        },
        'b' => {
            if vk.is_integer() {
                format_integer(spec, slot1, vk, 2, false)
            } else {
                match vk {
                    ValueKind::Float32 | ValueKind::Float64 => format_binary_float(slot1, vk, spec),
                    _ => format!("%!b({})", format_interface(slot0, slot1)),
                }
            }
        }
        'o' | 'O' => {
            if vk.is_integer() {
                format_integer(spec, slot1, vk, 8, false)
            } else {
                format!("%!{}({})", spec.verb, format_interface(slot0, slot1))
            }
        }
        'x' => match vk {
            ValueKind::String => {
                // Safety: the interface metadata identifies `slot1` as a live string.
                let bytes = unsafe { str_obj::to_bytes(slot1 as GcRef) };
                let hex: String = bytes.iter().map(|b| format!("{:02x}", b)).collect();
                pad_width(hex, spec, ' ')
            }
            _ if vk.is_integer() => format_integer(spec, slot1, vk, 16, false),
            _ => format!("%!x({})", format_interface(slot0, slot1)),
        },
        'X' => match vk {
            ValueKind::String => {
                // Safety: the interface metadata identifies `slot1` as a live string.
                let bytes = unsafe { str_obj::to_bytes(slot1 as GcRef) };
                let hex: String = bytes.iter().map(|b| format!("{:02X}", b)).collect();
                pad_width(hex, spec, ' ')
            }
            _ if vk.is_integer() => format_integer(spec, slot1, vk, 16, true),
            _ => format!("%!X({})", format_interface(slot0, slot1)),
        },
        'c' => {
            if vk.is_integer() {
                let ch = integer_code_point(slot1, vk);
                pad_width(ch.to_string(), spec, ' ')
            } else {
                format!("%!c({})", format_interface(slot0, slot1))
            }
        }
        'U' => {
            if vk.is_integer() {
                format_unicode_code_point(slot1, vk, spec)
            } else {
                format!("%!U({})", format_interface(slot0, slot1))
            }
        }
        'p' => pad_width(format!("0x{:x}", slot1), spec, ' '),
        'q' => match vk {
            ValueKind::String => {
                // String values take the byte-preserving path in
                // `format_with_spec_bytes`.
                let s = format_interface_with_ctx(slot0, slot1, call);
                pad_width(format!("{:?}", s), spec, ' ')
            }
            _ if vk.is_integer() => {
                let ch = integer_code_point(slot1, vk);
                pad_width(format!("{:?}", ch), spec, ' ')
            }
            _ => format!("%!q({})", format_interface(slot0, slot1)),
        },
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
        // Safety: `new_slice` is freshly allocated above.
        let dst_ptr = unsafe { slice::data_ptr(new_slice) } as *mut u64;
        for (i, item) in items.iter().enumerate() {
            unsafe {
                *dst_ptr.add(i * 2) = item.slot0;
                *dst_ptr.add(i * 2 + 1) = item.slot1;
            }
        }
    }
    gc.mark_allocated_for_scan(unsafe { slice::owner_ref(new_slice) });
    new_slice
}

/// Parse a format string and scan input accordingly, returning typed InterfaceSlots.
fn sscanf_impl_scan(
    input: &[u8],
    format: &[u8],
    gc: &mut vo_runtime::gc::Gc,
) -> Result<Vec<interface::InterfaceSlot>, String> {
    let mut result = Vec::new();
    let mut format_pos = 0;
    let mut input_pos = 0;

    while format_pos < format.len() {
        let (format_rune, format_width) = crate::raw_utf8::decode_first(&format[format_pos..]);
        if format[format_pos] == b'%' {
            format_pos += 1;
            if format.get(format_pos).copied() == Some(b'%') {
                format_pos += 1;
                if input.get(input_pos).copied() != Some(b'%') {
                    return Err(format!("expected '%%' at position {}", input_pos));
                }
                input_pos += 1;
                continue;
            }

            let Some(&verb_byte) = format.get(format_pos) else {
                return Err("incomplete format verb".into());
            };
            if !verb_byte.is_ascii() {
                return Err(format!("non-ASCII scan verb at byte {}", format_pos));
            }
            format_pos += 1;
            let verb = char::from(verb_byte);

            // All supported scan verbs except %c consume leading whitespace.
            // %c reads the next rune verbatim, matching Go's scanner contract.
            if verb != 'c' {
                skip_whitespace(input, &mut input_pos);
            }

            if input_pos >= input.len() {
                return Err(format!("unexpected end of input for %{}", verb));
            }

            match verb {
                'd' => {
                    let start = input_pos;
                    if input_pos < input.len()
                        && (input[input_pos] == b'-' || input[input_pos] == b'+')
                    {
                        input_pos += 1;
                    }
                    while input_pos < input.len() && input[input_pos].is_ascii_digit() {
                        input_pos += 1;
                    }
                    let token = ascii_scan_token(&input[start..input_pos], "%d")?;
                    let val: i64 = token
                        .parse()
                        .map_err(|e| format!("expected integer for %d: {}", e))?;
                    result.push(interface::InterfaceSlot::from_i64(val));
                }
                'f' | 'e' | 'g' => {
                    let start = input_pos;
                    if input_pos < input.len()
                        && (input[input_pos] == b'-' || input[input_pos] == b'+')
                    {
                        input_pos += 1;
                    }
                    while input_pos < input.len() {
                        let b = input[input_pos];
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
                    let token = ascii_scan_token(&input[start..input_pos], "float")?;
                    let val: f64 = token
                        .parse()
                        .map_err(|e| format!("expected float for %{}: {}", verb, e))?;
                    result.push(interface::InterfaceSlot::from_f64(val));
                }
                's' => {
                    let start = input_pos;
                    take_until_whitespace(input, &mut input_pos);
                    let token = &input[start..input_pos];
                    let str_ref = str_obj::create(gc, token);
                    result.push(interface::InterfaceSlot::from_ref(
                        str_ref,
                        ValueKind::String as u32,
                        ValueKind::String,
                    ));
                }
                't' => {
                    let start = input_pos;
                    take_until_whitespace(input, &mut input_pos);
                    let token = &input[start..input_pos];
                    let val = match token {
                        b"true" | b"TRUE" | b"1" => true,
                        b"false" | b"FALSE" | b"0" => false,
                        _ => return Err("expected bool for %t".to_string()),
                    };
                    result.push(interface::InterfaceSlot::from_bool(val));
                }
                'c' => {
                    let (ch, width) = crate::raw_utf8::decode_first(&input[input_pos..]);
                    input_pos += width;
                    result.push(interface::InterfaceSlot::from_i64(ch as i64));
                }
                'x' | 'X' => {
                    let val = scan_radix_integer(input, &mut input_pos, 16, b'x', "%x")?;
                    result.push(interface::InterfaceSlot::from_i64(val));
                }
                'o' => {
                    let val = scan_radix_integer(input, &mut input_pos, 8, b'o', "%o")?;
                    result.push(interface::InterfaceSlot::from_i64(val));
                }
                'b' => {
                    let val = scan_radix_integer(input, &mut input_pos, 2, b'b', "%b")?;
                    result.push(interface::InterfaceSlot::from_i64(val));
                }
                'v' => {
                    let start = input_pos;
                    take_until_whitespace(input, &mut input_pos);
                    let token = &input[start..input_pos];
                    let str_ref = str_obj::create(gc, token);
                    result.push(interface::InterfaceSlot::from_ref(
                        str_ref,
                        ValueKind::String as u32,
                        ValueKind::String,
                    ));
                }
                _ => return Err(format!("unsupported scan verb '%{}'", verb)),
            }
        } else if crate::unicode::is_space_char(format_rune) {
            format_pos += format_width;
            // Skip whitespace in both format and input
            skip_whitespace(input, &mut input_pos);
        } else {
            let literal = &format[format_pos..format_pos + format_width];
            if input.get(input_pos..input_pos + format_width) != Some(literal) {
                return Err(format!("format literal mismatch at byte {}", input_pos));
            }
            format_pos += format_width;
            input_pos += format_width;
        }
    }

    Ok(result)
}

fn ascii_scan_token<'a>(token: &'a [u8], description: &str) -> Result<&'a str, String> {
    core::str::from_utf8(token)
        .map_err(|_| format!("{} requires an ASCII numeric token", description))
}

fn scan_radix_integer(
    input: &[u8],
    offset: &mut usize,
    radix: u32,
    prefix_letter: u8,
    description: &str,
) -> Result<i64, String> {
    let negative = match input.get(*offset).copied() {
        Some(b'-') => {
            *offset += 1;
            true
        }
        Some(b'+') => {
            *offset += 1;
            false
        }
        _ => false,
    };

    if input.get(*offset).copied() == Some(b'0')
        && input
            .get(*offset + 1)
            .copied()
            .is_some_and(|byte| byte.eq_ignore_ascii_case(&prefix_letter))
    {
        *offset += 2;
    }

    let digits_start = *offset;
    while let Some(&byte) = input.get(*offset) {
        let valid = match radix {
            2 => matches!(byte, b'0' | b'1'),
            8 => matches!(byte, b'0'..=b'7'),
            16 => byte.is_ascii_hexdigit(),
            _ => false,
        };
        if !valid {
            break;
        }
        *offset += 1;
    }
    if *offset == digits_start {
        return Err(format!("expected digits for {}", description));
    }

    let digits = ascii_scan_token(&input[digits_start..*offset], description)?;
    let magnitude = u64::from_str_radix(digits, radix)
        .map_err(|error| format!("expected integer for {}: {}", description, error))?;
    if negative {
        if magnitude == (1u64 << 63) {
            Ok(i64::MIN)
        } else {
            i64::try_from(magnitude)
                .map(|value| -value)
                .map_err(|_| format!("{} overflows int", description))
        }
    } else {
        i64::try_from(magnitude).map_err(|_| format!("{} overflows int", description))
    }
}

fn skip_whitespace(input: &[u8], offset: &mut usize) {
    while *offset < input.len() {
        let (rune, width) = crate::raw_utf8::decode_first(&input[*offset..]);
        if !crate::unicode::is_space_char(rune) {
            break;
        }
        *offset += width;
    }
}

fn take_until_whitespace(input: &[u8], offset: &mut usize) {
    while *offset < input.len() {
        let (rune, width) = crate::raw_utf8::decode_first(&input[*offset..]);
        if crate::unicode::is_space_char(rune) {
            break;
        }
        *offset += width;
    }
}

fn split_whitespace_bytes(input: &[u8]) -> Vec<&[u8]> {
    let mut result = Vec::new();
    let mut offset = 0;
    while offset < input.len() {
        skip_whitespace(input, &mut offset);
        if offset == input.len() {
            break;
        }
        let start = offset;
        take_until_whitespace(input, &mut offset);
        result.push(&input[start..offset]);
    }
    result
}

// =============================================================================
// Native extern functions
// =============================================================================

/// nativeWrite - write string to output (uses output.rs for std/no_std)
#[vostd_fn("fmt", "nativeWrite")]
fn native_write(call: &mut ExternCallContext) -> ExternResult {
    let bytes = call.arg_string_bytes(slots::ARG_S);
    call.write_output_bytes(&bytes);
    ExternResult::Ok
}

/// nativeSprint - format []interface{} with default format (Sprint semantics)
#[vostd_fn("fmt", "nativeSprint")]
fn native_sprint(call: &mut ExternCallContext) -> ExternResult {
    let args_ref = call.arg_ref(slots::ARG_A);
    let formatted = sprint_format_args(args_ref, Some(call));
    call.ret_string_bytes(slots::RET_0, &formatted);
    ExternResult::Ok
}

/// nativeSprintln - format []interface{} with newline
#[vostd_fn("fmt", "nativeSprintln")]
fn native_sprintln(call: &mut ExternCallContext) -> ExternResult {
    let args_ref = call.arg_ref(slots::ARG_A);
    let mut formatted = format_args_slice_with_ctx(args_ref, Some(call));
    formatted.push(b'\n');
    call.ret_string_bytes(slots::RET_0, &formatted);
    ExternResult::Ok
}

/// nativeSprintf - format with format string
#[vostd_fn("fmt", "nativeSprintf")]
fn native_sprintf(call: &mut ExternCallContext) -> ExternResult {
    let format_bytes = call.arg_string_bytes(slots::ARG_FORMAT);
    let args_ref = call.arg_ref(slots::ARG_A);
    let formatted = sprintf_impl(&format_bytes, args_ref, Some(call));
    call.ret_string_bytes(slots::RET_0, &formatted);
    ExternResult::Ok
}

/// nativeSscan - split string by whitespace, return []any of strings
#[vostd_fn("fmt", "nativeSscan")]
fn native_sscan(call: &mut ExternCallContext) -> ExternResult {
    let input = call.arg_string_bytes(slots::ARG_STR);
    let tokens = split_whitespace_bytes(&input);
    let gc = call.gc();
    let items: Vec<interface::InterfaceSlot> = tokens
        .iter()
        .map(|t| {
            let str_ref = str_obj::create(gc, t);
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
    let input = call.arg_string_bytes(slots::ARG_STR);
    let format = call.arg_string_bytes(slots::ARG_FORMAT);
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
#[vostd_fn("fmt", "nativeReadLine", std)]
fn native_read_line(call: &mut ExternCallContext) -> ExternResult {
    use std::io::BufRead;
    let stdin = std::io::stdin();
    let mut line = Vec::new();
    match stdin.lock().read_until(b'\n', &mut line) {
        Ok(0) => {
            // EOF: stdin closed
            call.ret_string_bytes(slots::RET_0, b"");
            call.ret_error_msg(slots::RET_1, "EOF");
        }
        Ok(_) => {
            // Trim trailing newline
            if line.last() == Some(&b'\n') {
                line.pop();
                if line.last() == Some(&b'\r') {
                    line.pop();
                }
            }
            call.ret_string_bytes(slots::RET_0, &line);
            call.ret_nil_error(slots::RET_1);
        }
        Err(e) => {
            call.ret_string_bytes(slots::RET_0, b"");
            call.ret_error_msg(slots::RET_1, &e.to_string());
        }
    }
    ExternResult::Ok
}

vo_ffi_macro::vostd_register!("fmt":
    nativeWrite,
    nativeSprint,
    nativeSprintln,
    nativeSprintf,
    nativeSscan,
    nativeSscanf,
    nativeReadLine,
);

#[cfg(test)]
mod tests {
    use super::*;
    use vo_runtime::gc::Gc;

    fn string_slot(gc: &mut Gc, bytes: &[u8]) -> interface::InterfaceSlot {
        let value = str_obj::create(gc, bytes);
        interface::InterfaceSlot::from_ref(value, ValueKind::String as u32, ValueKind::String)
    }

    fn scalar_slot(vk: ValueKind, raw: u64) -> interface::InterfaceSlot {
        interface::InterfaceSlot::new(interface::pack_slot0(0, vk as u32, vk), raw)
    }

    #[test]
    fn sprint_and_sprintf_preserve_arbitrary_string_bytes() {
        let mut gc = Gc::new();
        let raw = b"a\xffz";
        let value = string_slot(&mut gc, raw);
        let sprint_args = create_any_slice(&mut gc, &[value]);
        assert_eq!(sprint_format_args(sprint_args, None), raw);

        let format_args = create_any_slice(&mut gc, &[value, value, value, value]);
        assert_eq!(
            sprintf_impl(b"%s|%q|%.2x|%\xfe", format_args, None),
            b"a\xffz|\"a\\xffz\"|61ff|%!\xfe(a\xffz)"
        );
    }

    #[test]
    fn formatting_width_counts_malformed_bytes_as_single_runes() {
        let mut gc = Gc::new();
        let value = string_slot(&mut gc, b"a\xff");
        let args = create_any_slice(&mut gc, &[value]);
        assert_eq!(sprintf_impl(b"%3s", args, None), b" a\xff");
    }

    #[test]
    fn string_hex_flags_match_go_without_losing_bytes() {
        let mut gc = Gc::new();
        let value = string_slot(&mut gc, b"a\xff");
        let args = create_any_slice(&mut gc, &[value, value, value]);
        assert_eq!(
            sprintf_impl(b"%#x|% X|%# X", args, None),
            b"0x61ff|61 FF|0X61 0XFF"
        );
    }

    #[test]
    fn scan_string_verbs_preserve_arbitrary_bytes() {
        let mut gc = Gc::new();
        let items = sscanf_impl_scan(b"a\xffz", b"%s", &mut gc).unwrap();
        assert_eq!(items.len(), 1);
        let value = items[0].slot1 as GcRef;
        // Safety: `%s` created this live string in `gc` above.
        assert_eq!(unsafe { str_obj::to_bytes(value) }, b"a\xffz");
        assert_eq!(
            split_whitespace_bytes(b"a\xff b"),
            vec![&b"a\xff"[..], &b"b"[..]]
        );
    }

    #[test]
    fn scanning_whitespace_uses_the_pinned_unicode_profile() {
        let nel = "left\u{0085}right";
        assert_eq!(
            split_whitespace_bytes(nel.as_bytes()),
            vec![&b"left"[..], &b"right"[..]]
        );

        let zero_width_space = "left\u{200b}right";
        assert_eq!(
            split_whitespace_bytes(zero_width_space.as_bytes()),
            vec![zero_width_space.as_bytes()]
        );

        let mut gc = Gc::new();
        let items = sscanf_impl_scan("alpha\u{0085}beta".as_bytes(), b"%s %s", &mut gc).unwrap();
        assert_eq!(items.len(), 2);
        for (item, expected) in items.iter().zip([b"alpha".as_slice(), b"beta".as_slice()]) {
            // Safety: `%s` created both live strings in `gc` above.
            assert_eq!(unsafe { str_obj::to_bytes(item.slot1 as GcRef) }, expected);
        }
    }

    #[test]
    fn raw_format_literals_round_trip() {
        let mut gc = Gc::new();
        let value = string_slot(&mut gc, b"x\xfe");
        let args = create_any_slice(&mut gc, &[value]);
        assert_eq!(sprintf_impl(b"\xff:%s", args, None), b"\xff:x\xfe");
    }

    #[test]
    fn formatting_rejects_attack_sized_width_and_precision() {
        let mut gc = Gc::new();
        let value = interface::InterfaceSlot::from_i64(7);
        let args = create_any_slice(&mut gc, &[value, value]);
        assert_eq!(
            sprintf_impl(b"%1000001d|%.1000001d", args, None),
            b"%!(BADWIDTH)7|%!(BADPREC)7"
        );
    }

    #[test]
    fn integer_flags_precision_and_narrow_values_match_go() {
        let mut gc = Gc::new();
        let zero = interface::InterfaceSlot::from_i64(0);
        let value = interface::InterfaceSlot::from_i64(42);
        let octal = interface::InterfaceSlot::from_i64(0o1234);
        let narrow = scalar_slot(ValueKind::Int8, 0x80);
        let large_rune = scalar_slot(ValueKind::Uint64, 1u64 << 32);
        let args = create_any_slice(
            &mut gc,
            &[
                zero, zero, zero, value, value, octal, octal, value, value, narrow, large_rune,
            ],
        );
        assert_eq!(
            sprintf_impl(
                b"%.0d|%06.0d|%+.0d|%#08x|%#8.4x|%#o|%O|%+08d|% d|%d|%c",
                args,
                None,
            ),
            "|      ||0x00002a|  0x002a|01234|0o1234|+0000042| 42|-128|�".as_bytes()
        );
    }

    #[test]
    fn unicode_integer_format_matches_go_flags_precision_and_width() {
        let mut gc = Gc::new();
        let zero = interface::InterfaceSlot::from_i64(0);
        let negative = interface::InterfaceSlot::from_i64(-1);
        let newline = scalar_slot(ValueKind::Int32, '\n' as u64);
        let x = scalar_slot(ValueKind::Int32, 'x' as u64);
        let smile = scalar_slot(ValueKind::Int32, '\u{263a}' as u64);
        let alpha = scalar_slot(ValueKind::Int32, '\u{1d6c2}' as u64);
        let command = scalar_slot(ValueKind::Int32, '\u{2318}' as u64);
        let args = create_any_slice(
            &mut gc,
            &[
                zero, negative, newline, newline, x, x, x, smile, smile, alpha, alpha, command,
                command, command, command,
            ],
        );

        assert_eq!(
            sprintf_impl(
                b"%U|%U|%U|%#U|%+U|%# U|%#.2U|%U|%#U|%U|%#U|%#14.6U|%#-14.6U|%#014.6U|%#-014.6U",
                args,
                None,
            ),
            "U+0000|U+FFFFFFFFFFFFFFFF|U+000A|U+000A|U+0078|U+0078 'x'|U+0078 'x'|U+263A|U+263A '☺'|U+1D6C2|U+1D6C2 '𝛂'|  U+002318 '⌘'|U+002318 '⌘'  |  U+002318 '⌘'|U+002318 '⌘'  ".as_bytes(),
        );
    }

    #[test]
    fn unicode_integer_format_handles_large_precision_and_invalid_scalars() {
        let mut gc = Gc::new();
        let forty_two = scalar_slot(ValueKind::Uint, 42);
        let sun = scalar_slot(ValueKind::Int32, '日' as u64);
        let negative_int8 = scalar_slot(ValueKind::Int8, u8::MAX as u64);
        let surrogate = scalar_slot(ValueKind::Uint32, 0xd800);
        let args = create_any_slice(&mut gc, &[forty_two, sun, negative_int8, surrogate]);
        let actual = sprintf_impl(b"%.68U|%#.68U|%U|%#U", args, None);
        let expected = format!(
            "U+{}2A|U+{}65E5 '日'|U+FFFFFFFFFFFFFFFF|U+D800",
            "0".repeat(66),
            "0".repeat(64),
        );
        assert_eq!(actual, expected.as_bytes());
    }

    #[test]
    fn binary_float_format_matches_go_exact_representation() {
        let mut gc = Gc::new();
        let one32 = scalar_slot(ValueKind::Float32, 1.0f32.to_bits() as u64);
        let one64 = scalar_slot(ValueKind::Float64, 1.0f64.to_bits());
        let negative = scalar_slot(ValueKind::Float64, (-1.0f64).to_bits());
        let zero = scalar_slot(ValueKind::Float64, 0.0f64.to_bits());
        let args = create_any_slice(&mut gc, &[one32, one64, negative, zero]);
        assert_eq!(
            sprintf_impl(b"%b|%b|%.4b|%b", args, None),
            b"8388608p-23|4503599627370496p-52|-4503599627370496p-52|0p-1074"
        );
    }

    #[test]
    fn sprintf_reports_unused_arguments() {
        let mut gc = Gc::new();
        let raw = string_slot(&mut gc, b"raw\xff");
        let args = create_any_slice(&mut gc, &[interface::InterfaceSlot::from_i64(1), raw]);
        assert_eq!(
            sprintf_impl(b"%d", args, None),
            b"1%!(EXTRA string=raw\xff)"
        );
    }

    #[test]
    fn radix_scan_accepts_signs_and_rejects_bare_prefixes() {
        let mut gc = Gc::new();
        let values = sscanf_impl_scan(b"-0x80 +0o17 -0b10", b"%x %o %b", &mut gc).unwrap();
        assert_eq!(
            values
                .iter()
                .map(|value| value.as_i64())
                .collect::<Vec<_>>(),
            vec![-128, 15, -2]
        );

        for (input, format) in [
            (b"0x".as_slice(), b"%x".as_slice()),
            (b"0o", b"%o"),
            (b"0b", b"%b"),
        ] {
            assert!(sscanf_impl_scan(input, format, &mut gc).is_err());
        }
        let value = sscanf_impl_scan(b" ", b"%c", &mut gc).unwrap();
        assert_eq!(value[0].as_i64(), ' ' as i64);
    }
}
