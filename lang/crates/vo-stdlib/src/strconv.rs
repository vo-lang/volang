//! Native support for `strconv` floating-point conversion.
//!
//! Vo strings are byte strings, so parsing uses the manual FFI boundary and
//! rejects malformed UTF-8 as ordinary syntax instead of imposing a Rust
//! `&str` contract. Decimal conversion delegates to Rust's correctly-rounded
//! IEEE parser/formatter. Hexadecimal conversion is performed directly on the
//! input bit stream with round-to-nearest, ties-to-even.

#[cfg(not(feature = "std"))]
use alloc::format;
#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::{vec, vec::Vec};

use vo_ffi_macro::vostd_fn;
use vo_runtime::ffi::{ExternCallContext, ExternResult};

// ==================== Float parsing ====================

#[vostd_fn("strconv", "parseFloat")]
fn parse_float(call: &mut ExternCallContext) -> ExternResult {
    let input = call.arg_string_bytes(slots::ARG_S);
    let bit_size = call.arg_i64(slots::ARG_BIT_SIZE);
    let (value, status) = parse_float_status_bytes(&input, bit_size);
    call.ret_f64(slots::RET_0, value);
    call.ret_i64(slots::RET_1, status);
    ExternResult::Ok
}

fn parse_float_status_bytes(input: &[u8], bit_size: i64) -> (f64, i64) {
    let (value, ok) = parse_float_bytes(input, bit_size);
    // The facade needs to distinguish malformed syntax from range overflow in
    // order to construct the public *strconv.NumError. Every range failure
    // returns a signed infinity; syntax failures return zero.
    let status = if ok {
        0
    } else if value.is_infinite() {
        2
    } else {
        1
    };
    (value, status)
}

fn parse_float_bytes(input: &[u8], bit_size: i64) -> (f64, bool) {
    let Ok(text) = core::str::from_utf8(input) else {
        return (0.0, false);
    };
    if let Some(value) = parse_special(text, bit_size) {
        return (value, true);
    }
    if is_hex_float(text) {
        return parse_hex_float(text, bit_size);
    }
    parse_decimal_float(text, bit_size)
}

fn parse_special(text: &str, bit_size: i64) -> Option<f64> {
    if text.eq_ignore_ascii_case("nan") {
        return Some(if bit_size == 32 {
            f32::from_bits(0x7fc0_0000) as f64
        } else {
            f64::from_bits(0x7ff8_0000_0000_0001)
        });
    }
    let (negative, magnitude) = match text.as_bytes().first() {
        Some(b'+') => (false, &text[1..]),
        Some(b'-') => (true, &text[1..]),
        _ => (false, text),
    };
    if magnitude.eq_ignore_ascii_case("inf") || magnitude.eq_ignore_ascii_case("infinity") {
        Some(if negative {
            f64::NEG_INFINITY
        } else {
            f64::INFINITY
        })
    } else {
        None
    }
}

fn is_hex_float(text: &str) -> bool {
    let bytes = text.as_bytes();
    let start = usize::from(matches!(bytes.first(), Some(b'+') | Some(b'-')));
    bytes.get(start) == Some(&b'0')
        && bytes
            .get(start + 1)
            .is_some_and(|byte| byte.eq_ignore_ascii_case(&b'x'))
}

fn parse_decimal_float(text: &str, bit_size: i64) -> (f64, bool) {
    if !valid_decimal_syntax(text) || text.as_bytes().contains(&b'_') && !underscore_ok(text) {
        return (0.0, false);
    }
    let mut clean = String::with_capacity(text.len());
    clean.extend(text.chars().filter(|ch| *ch != '_'));
    let parse_text = clean.strip_prefix('+').unwrap_or(&clean);

    if bit_size == 32 {
        match parse_text.parse::<f32>() {
            Ok(value) => (value as f64, !value.is_infinite()),
            Err(_) => (0.0, false),
        }
    } else {
        match parse_text.parse::<f64>() {
            Ok(value) => (value, !value.is_infinite()),
            Err(_) => (0.0, false),
        }
    }
}

fn valid_decimal_syntax(text: &str) -> bool {
    let bytes = text.as_bytes();
    let mut index = 0;
    if matches!(bytes.first(), Some(b'+') | Some(b'-')) {
        index += 1;
    }

    let mut saw_digit = false;
    let mut saw_dot = false;
    while index < bytes.len() {
        match bytes[index] {
            b'0'..=b'9' => saw_digit = true,
            b'_' => {}
            b'.' if !saw_dot => saw_dot = true,
            _ => break,
        }
        index += 1;
    }
    if !saw_digit {
        return false;
    }

    if bytes
        .get(index)
        .is_some_and(|byte| byte.eq_ignore_ascii_case(&b'e'))
    {
        index += 1;
        if matches!(bytes.get(index), Some(b'+') | Some(b'-')) {
            index += 1;
        }
        let mut exponent_digit = false;
        while index < bytes.len() {
            match bytes[index] {
                b'0'..=b'9' => exponent_digit = true,
                b'_' => {}
                _ => return false,
            }
            index += 1;
        }
        if !exponent_digit {
            return false;
        }
    }
    index == bytes.len()
}

/// Go's underscore rule: separators occur only between digits, with the base
/// prefix counting as a digit for `0x_1p0`.
fn underscore_ok(text: &str) -> bool {
    let bytes = text.as_bytes();
    let mut index = 0;
    let mut previous = b'^';
    if matches!(bytes.first(), Some(b'+') | Some(b'-')) {
        index += 1;
    }

    let mut hexadecimal = false;
    if bytes.get(index) == Some(&b'0')
        && bytes
            .get(index + 1)
            .is_some_and(|byte| matches!(byte.to_ascii_lowercase(), b'b' | b'o' | b'x'))
    {
        hexadecimal = bytes[index + 1].eq_ignore_ascii_case(&b'x');
        index += 2;
        previous = b'0';
    }

    while index < bytes.len() {
        let byte = bytes[index];
        if byte.is_ascii_digit() || hexadecimal && matches!(byte.to_ascii_lowercase(), b'a'..=b'f')
        {
            previous = b'0';
        } else if byte == b'_' {
            if previous != b'0' {
                return false;
            }
            previous = b'_';
        } else {
            if previous == b'_' {
                return false;
            }
            previous = b'!';
        }
        index += 1;
    }
    previous != b'_'
}

fn parse_hex_float(text: &str, bit_size: i64) -> (f64, bool) {
    if text.as_bytes().contains(&b'_') && !underscore_ok(text) {
        return (0.0, false);
    }
    let bytes = text.as_bytes();
    let mut index = 0;
    let negative = match bytes.first() {
        Some(b'+') => {
            index += 1;
            false
        }
        Some(b'-') => {
            index += 1;
            true
        }
        _ => false,
    };
    if bytes.get(index) != Some(&b'0')
        || !bytes
            .get(index + 1)
            .is_some_and(|byte| byte.eq_ignore_ascii_case(&b'x'))
    {
        return (0.0, false);
    }
    index += 2;

    let mut digits = Vec::new();
    let mut point_digits = 0usize;
    let mut saw_dot = false;
    let mut saw_digit = false;
    while index < bytes.len() {
        let byte = bytes[index];
        if byte == b'_' {
            index += 1;
            continue;
        }
        if byte == b'.' && !saw_dot {
            saw_dot = true;
            index += 1;
            continue;
        }
        let Some(digit) = hex_value(byte) else {
            break;
        };
        saw_digit = true;
        digits.push(digit);
        if !saw_dot {
            point_digits += 1;
        }
        index += 1;
    }
    if !saw_digit
        || !bytes
            .get(index)
            .is_some_and(|byte| byte.eq_ignore_ascii_case(&b'p'))
    {
        return (0.0, false);
    }
    index += 1;

    let exponent_negative = match bytes.get(index) {
        Some(b'+') => {
            index += 1;
            false
        }
        Some(b'-') => {
            index += 1;
            true
        }
        _ => false,
    };
    let mut saw_exponent_digit = false;
    let mut exponent = 0i64;
    while index < bytes.len() {
        match bytes[index] {
            b'_' => {}
            b'0'..=b'9' => {
                saw_exponent_digit = true;
                exponent = (exponent * 10 + i64::from(bytes[index] - b'0')).min(1_000_000);
            }
            _ => return (0.0, false),
        }
        index += 1;
    }
    if !saw_exponent_digit {
        return (0.0, false);
    }
    if exponent_negative {
        exponent = -exponent;
    }

    let Some(first_nonzero) = digits.iter().position(|digit| *digit != 0) else {
        return (signed_zero(negative, bit_size), true);
    };
    let top_bit = 7i64 - i64::from(digits[first_nonzero].leading_zeros());
    let point = i64::try_from(point_digits).unwrap_or(i64::MAX / 8);
    let first = i64::try_from(first_nonzero).unwrap_or(i64::MAX / 8);
    let binary_exponent = exponent
        .saturating_add(point.saturating_sub(first + 1).saturating_mul(4))
        .saturating_add(top_bit);

    let format = if bit_size == 32 {
        BinaryFloatFormat::F32
    } else {
        BinaryFloatFormat::F64
    };
    rounded_hex_value(
        &digits,
        first_nonzero,
        top_bit as u8,
        binary_exponent,
        negative,
        format,
    )
}

#[derive(Clone, Copy)]
struct BinaryFloatFormat {
    precision: usize,
    minimum_normal_exponent: i64,
    maximum_exponent: i64,
    exponent_bias: i64,
    total_bits: u32,
}

impl BinaryFloatFormat {
    const F32: Self = Self {
        precision: 24,
        minimum_normal_exponent: -126,
        maximum_exponent: 127,
        exponent_bias: 127,
        total_bits: 32,
    };

    const F64: Self = Self {
        precision: 53,
        minimum_normal_exponent: -1022,
        maximum_exponent: 1023,
        exponent_bias: 1023,
        total_bits: 64,
    };
}

fn rounded_hex_value(
    digits: &[u8],
    first_nonzero: usize,
    top_bit: u8,
    mut exponent: i64,
    negative: bool,
    format: BinaryFloatFormat,
) -> (f64, bool) {
    if exponent > format.maximum_exponent {
        return (signed_infinity(negative, format.total_bits), false);
    }
    let quantum_exponent = format.minimum_normal_exponent - (format.precision as i64 - 1);

    let raw_bits = if exponent >= format.minimum_normal_exponent {
        let (mut significand, round, sticky) =
            extract_hex_bits(digits, first_nonzero, top_bit, format.precision);
        if round && (sticky || significand & 1 != 0) {
            significand += 1;
            if significand == 1u64 << format.precision {
                significand >>= 1;
                exponent += 1;
                if exponent > format.maximum_exponent {
                    return (signed_infinity(negative, format.total_bits), false);
                }
            }
        }
        let fraction_bits = format.precision - 1;
        let fraction_mask = (1u64 << fraction_bits) - 1;
        let exponent_field = (exponent + format.exponent_bias) as u64;
        exponent_field << fraction_bits | significand & fraction_mask
    } else if exponent < quantum_exponent - 1 {
        0
    } else {
        let keep = if exponent < quantum_exponent {
            0
        } else {
            usize::try_from(exponent - quantum_exponent + 1).unwrap_or(format.precision - 1)
        };
        let (mut significand, round, sticky) =
            extract_hex_bits(digits, first_nonzero, top_bit, keep);
        if round && (sticky || significand & 1 != 0) {
            significand += 1;
        }
        // A rounded subnormal can carry into the smallest normal value; the
        // bit pattern is the same integer in the combined exponent/fraction field.
        significand
    };

    let sign = u64::from(negative) << (format.total_bits - 1);
    let bits = sign | raw_bits;
    if format.total_bits == 32 {
        (f32::from_bits(bits as u32) as f64, true)
    } else {
        (f64::from_bits(bits), true)
    }
}

fn extract_hex_bits(
    digits: &[u8],
    first_nonzero: usize,
    top_bit: u8,
    keep: usize,
) -> (u64, bool, bool) {
    let mut kept = 0usize;
    let mut value = 0u64;
    let mut round = false;
    let mut sticky = false;
    let mut seen = 0usize;

    for (offset, digit) in digits[first_nonzero..].iter().copied().enumerate() {
        let highest = if offset == 0 { top_bit } else { 3 };
        for shift in (0..=highest).rev() {
            let bit = digit >> shift & 1;
            if seen < keep {
                value = value << 1 | u64::from(bit);
                kept += 1;
            } else if seen == keep {
                round = bit != 0;
            } else {
                sticky |= bit != 0;
            }
            seen += 1;
        }
    }
    if kept < keep {
        value <<= keep - kept;
    }
    (value, round, sticky)
}

fn hex_value(byte: u8) -> Option<u8> {
    match byte {
        b'0'..=b'9' => Some(byte - b'0'),
        b'a'..=b'f' => Some(byte - b'a' + 10),
        b'A'..=b'F' => Some(byte - b'A' + 10),
        _ => None,
    }
}

fn signed_zero(negative: bool, bit_size: i64) -> f64 {
    if bit_size == 32 {
        f32::from_bits(u32::from(negative) << 31) as f64
    } else {
        f64::from_bits(u64::from(negative) << 63)
    }
}

fn signed_infinity(negative: bool, total_bits: u32) -> f64 {
    if total_bits == 32 {
        let bits = (u32::from(negative) << 31) | 0x7f80_0000;
        f32::from_bits(bits) as f64
    } else {
        let bits = (u64::from(negative) << 63) | 0x7ff0_0000_0000_0000;
        f64::from_bits(bits)
    }
}

// ==================== Float formatting ====================

#[vostd_fn("strconv", "formatFloat")]
fn format_float(call: &mut ExternCallContext) -> ExternResult {
    let value = call.arg_f64(slots::ARG_F);
    let format = call.arg_u64(slots::ARG_FMT) as u8;
    let precision = call.arg_i64(slots::ARG_PREC);
    let bit_size = call.arg_i64(slots::ARG_BIT_SIZE);
    let result = format_float_bytes(value, format, precision, bit_size);
    call.ret_string_bytes(slots::RET_0, &result);
    ExternResult::Ok
}

fn format_float_bytes(value: f64, format: u8, precision: i64, bit_size: i64) -> Vec<u8> {
    let value = if bit_size == 32 {
        (value as f32) as f64
    } else {
        value
    };
    if value.is_nan() {
        return b"NaN".to_vec();
    }
    if value.is_infinite() {
        return if value.is_sign_negative() {
            b"-Inf".to_vec()
        } else {
            b"+Inf".to_vec()
        };
    }

    match format {
        b'b' => format_binary(value, bit_size).into_bytes(),
        b'e' | b'E' => format_exponent(value, precision, bit_size, format == b'E').into_bytes(),
        b'f' => format_fixed(value, precision, bit_size).into_bytes(),
        b'g' | b'G' => format_general(value, precision, bit_size, format == b'G').into_bytes(),
        b'x' | b'X' => format_hex(value, precision, bit_size, format == b'X').into_bytes(),
        // Go deliberately returns the two-byte diagnostic "%<fmt>" for an
        // unknown format. The byte may itself be malformed UTF-8.
        byte => vec![b'%', byte],
    }
}

#[derive(Clone, Copy)]
struct FloatParts {
    negative: bool,
    mantissa: u64,
    exponent: i64,
    mantissa_bits: u32,
}

fn float_parts(value: f64, bit_size: i64) -> FloatParts {
    if bit_size == 32 {
        let bits = (value as f32).to_bits();
        let negative = bits >> 31 != 0;
        let exponent_field = i64::from(bits >> 23 & 0xff);
        let mut mantissa = u64::from(bits & 0x007f_ffff);
        let exponent = if exponent_field == 0 {
            1 - 127
        } else {
            mantissa |= 1 << 23;
            exponent_field - 127
        };
        FloatParts {
            negative,
            mantissa,
            exponent,
            mantissa_bits: 23,
        }
    } else {
        let bits = value.to_bits();
        let negative = bits >> 63 != 0;
        let exponent_field = ((bits >> 52) & 0x7ff) as i64;
        let mut mantissa = bits & 0x000f_ffff_ffff_ffff;
        let exponent = if exponent_field == 0 {
            1 - 1023
        } else {
            mantissa |= 1 << 52;
            exponent_field - 1023
        };
        FloatParts {
            negative,
            mantissa,
            exponent,
            mantissa_bits: 52,
        }
    }
}

fn format_binary(value: f64, bit_size: i64) -> String {
    let parts = float_parts(value, bit_size);
    let sign = if parts.negative { "-" } else { "" };
    let exponent = parts.exponent - i64::from(parts.mantissa_bits);
    format!("{sign}{}p{exponent:+}", parts.mantissa)
}

fn format_exponent(value: f64, precision: i64, bit_size: i64, upper: bool) -> String {
    if precision < 0 {
        if value == 0.0 {
            return format!(
                "{}0{}+00",
                if value.is_sign_negative() { "-" } else { "" },
                if upper { 'E' } else { 'e' }
            );
        }
        let digits = shortest_digits_for_size(value, bit_size);
        return digits.to_exponent(upper);
    }
    let precision = usize::try_from(precision).expect("strconv precision does not fit usize");
    let raw = format!("{value:.*e}", precision);
    normalize_exponent(&raw, upper)
}

fn format_fixed(value: f64, precision: i64, bit_size: i64) -> String {
    if precision < 0 {
        if value == 0.0 {
            return if value.is_sign_negative() {
                "-0".to_string()
            } else {
                "0".to_string()
            };
        }
        return shortest_digits_for_size(value, bit_size).to_fixed();
    }
    let precision = usize::try_from(precision).expect("strconv precision does not fit usize");
    format!("{value:.*}", precision)
}

fn format_general(value: f64, precision: i64, bit_size: i64, upper: bool) -> String {
    if value == 0.0 {
        return if value.is_sign_negative() {
            "-0".to_string()
        } else {
            "0".to_string()
        };
    }
    if precision < 0 {
        let digits = shortest_digits_for_size(value, bit_size);
        let exponent = digits.decimal_point - 1;
        if !(-4..6).contains(&exponent) {
            return digits.to_exponent(upper);
        }
        return digits.to_fixed();
    }

    let significant = if precision == 0 { 1 } else { precision };
    let significant = usize::try_from(significant).expect("strconv precision does not fit usize");
    let scientific = format!("{value:.*e}", significant - 1);
    let mut digits = DecimalDigits::parse(&scientific);
    while digits.digits.len() > 1 && digits.digits.last() == Some(&b'0') {
        digits.digits.pop();
    }
    let exponent = digits.decimal_point - 1;
    if exponent < -4 || exponent >= significant as i32 {
        digits.to_exponent(upper)
    } else {
        digits.to_fixed()
    }
}

fn shortest_digits_for_size(value: f64, bit_size: i64) -> DecimalDigits {
    let mut buffer = ryu::Buffer::new();
    let text = if bit_size == 32 {
        buffer.format(value as f32)
    } else {
        buffer.format(value)
    };
    DecimalDigits::parse(text)
}

struct DecimalDigits {
    negative: bool,
    digits: Vec<u8>,
    decimal_point: i32,
}

impl DecimalDigits {
    fn parse(text: &str) -> Self {
        let bytes = text.as_bytes();
        let mut index = 0;
        let negative = bytes.first() == Some(&b'-');
        if negative || bytes.first() == Some(&b'+') {
            index += 1;
        }
        let exponent_index = bytes[index..]
            .iter()
            .position(|byte| matches!(byte, b'e' | b'E'))
            .map_or(bytes.len(), |offset| index + offset);
        let exponent = if exponent_index < bytes.len() {
            text[exponent_index + 1..].parse::<i32>().unwrap_or(0)
        } else {
            0
        };

        let mut digits = Vec::new();
        let mut before_dot = 0i32;
        let mut dot = false;
        for byte in bytes[index..exponent_index].iter().copied() {
            if byte == b'.' {
                dot = true;
            } else {
                digits.push(byte);
                if !dot {
                    before_dot += 1;
                }
            }
        }
        let mut decimal_point = before_dot + exponent;
        while digits.len() > 1 && digits.first() == Some(&b'0') {
            digits.remove(0);
            decimal_point -= 1;
        }
        while digits.len() > 1 && digits.last() == Some(&b'0') {
            digits.pop();
        }
        Self {
            negative,
            digits,
            decimal_point,
        }
    }

    fn to_exponent(&self, upper: bool) -> String {
        let mut result = String::new();
        if self.negative {
            result.push('-');
        }
        result.push(self.digits.first().copied().unwrap_or(b'0') as char);
        if self.digits.len() > 1 {
            result.push('.');
            for digit in &self.digits[1..] {
                result.push(*digit as char);
            }
        }
        result.push(if upper { 'E' } else { 'e' });
        push_decimal_exponent(&mut result, self.decimal_point - 1);
        result
    }

    fn to_fixed(&self) -> String {
        let mut result = String::new();
        if self.negative {
            result.push('-');
        }
        if self.decimal_point <= 0 {
            result.push('0');
            result.push('.');
            for _ in 0..-self.decimal_point {
                result.push('0');
            }
            for digit in &self.digits {
                result.push(*digit as char);
            }
        } else {
            let point = self.decimal_point as usize;
            for index in 0..point {
                result.push(self.digits.get(index).copied().unwrap_or(b'0') as char);
            }
            if point < self.digits.len() {
                result.push('.');
                for digit in &self.digits[point..] {
                    result.push(*digit as char);
                }
            }
        }
        result
    }
}

fn normalize_exponent(raw: &str, upper: bool) -> String {
    let split = raw
        .rfind(['e', 'E'])
        .expect("Rust exponential formatting always contains an exponent");
    let mut result = raw[..split].to_string();
    result.push(if upper { 'E' } else { 'e' });
    let exponent = raw[split + 1..]
        .parse::<i32>()
        .expect("Rust emitted a decimal exponent");
    push_decimal_exponent(&mut result, exponent);
    result
}

fn push_decimal_exponent(result: &mut String, exponent: i32) {
    if exponent < 0 {
        result.push('-');
    } else {
        result.push('+');
    }
    let magnitude = exponent.unsigned_abs();
    if magnitude < 10 {
        result.push('0');
    }
    result.push_str(&magnitude.to_string());
}

fn format_hex(value: f64, precision: i64, bit_size: i64, upper: bool) -> String {
    let parts = float_parts(value, bit_size);
    let mut mantissa = parts.mantissa << (60 - parts.mantissa_bits);
    let mut exponent = parts.exponent;
    if mantissa == 0 {
        exponent = 0;
    }
    while mantissa != 0 && mantissa & (1 << 60) == 0 {
        mantissa <<= 1;
        exponent -= 1;
    }

    if (0..15).contains(&precision) {
        let shift = precision as u32 * 4;
        let extra = mantissa.wrapping_shl(shift) & ((1 << 60) - 1);
        mantissa >>= 60 - shift;
        if extra | (mantissa & 1) > 1 << 59 {
            mantissa += 1;
        }
        mantissa <<= 60 - shift;
        if mantissa & (1 << 61) != 0 {
            mantissa >>= 1;
            exponent += 1;
        }
    }

    let mut result = String::new();
    if parts.negative {
        result.push('-');
    }
    result.push('0');
    result.push(if upper { 'X' } else { 'x' });
    result.push((b'0' + ((mantissa >> 60) & 1) as u8) as char);

    mantissa = mantissa.wrapping_shl(4);
    let alphabet = if upper {
        b"0123456789ABCDEF"
    } else {
        b"0123456789abcdef"
    };
    if precision < 0 && mantissa != 0 {
        result.push('.');
        while mantissa != 0 {
            result.push(alphabet[(mantissa >> 60) as usize & 15] as char);
            mantissa = mantissa.wrapping_shl(4);
        }
    } else if precision > 0 {
        result.push('.');
        let precision = usize::try_from(precision).expect("strconv precision does not fit usize");
        for _ in 0..precision {
            result.push(alphabet[(mantissa >> 60) as usize & 15] as char);
            mantissa = mantissa.wrapping_shl(4);
        }
    }

    result.push(if upper { 'P' } else { 'p' });
    push_decimal_exponent(&mut result, exponent as i32);
    result
}

vo_ffi_macro::vostd_register!("strconv": parseFloat, formatFloat);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_status_distinguishes_syntax_and_range() {
        assert_eq!(parse_float_status_bytes(b"1.25", 64), (1.25, 0));
        assert_eq!(parse_float_status_bytes(b"bad", 64), (0.0, 1));
        let (value, status) = parse_float_status_bytes(b"1e400", 64);
        assert!(value.is_infinite());
        assert_eq!(status, 2);
    }

    #[test]
    fn parse_rejects_whitespace_and_malformed_utf8_without_a_contract_error() {
        assert_eq!(parse_float_bytes(b" 1", 64), (0.0, false));
        assert_eq!(parse_float_bytes(b"1 ", 64), (0.0, false));
        assert_eq!(parse_float_bytes(&[0xff], 64), (0.0, false));
    }

    #[test]
    fn parse_special_decimal_and_range_contract() {
        assert_eq!(
            parse_float_bytes(b"nAn", 64).0.to_bits(),
            0x7ff8_0000_0000_0001
        );
        assert_eq!(
            parse_float_bytes(b"NaN", 32).0.to_bits(),
            0x7ff8_0000_0000_0000
        );
        assert_eq!(
            parse_float_bytes(b"NaN", 7).0.to_bits(),
            0x7ff8_0000_0000_0001
        );
        assert_eq!(
            parse_float_bytes(b"-INFINITY", 64),
            (f64::NEG_INFINITY, true)
        );
        assert_eq!(parse_float_bytes(b"+NaN", 64), (0.0, false));
        assert_eq!(parse_float_bytes(b"1_2.5e+1", 64), (125.0, true));
        let (overflow, ok) = parse_float_bytes(b"1e400", 64);
        assert!(overflow.is_infinite());
        assert!(!ok);
        assert_eq!(parse_float_bytes(b"1e-4000", 64), (0.0, true));
    }

    #[test]
    fn parse_hex_rounds_subnormals_and_ties_to_even() {
        assert_eq!(parse_float_bytes(b"0x1p+0", 64), (1.0, true));
        assert_eq!(parse_float_bytes(b"0x1p-1074", 64).0.to_bits(), 1);
        assert_eq!(parse_float_bytes(b"0x1p-1075", 64).0.to_bits(), 0);
        assert_eq!(parse_float_bytes(b"0x1.00000000000008p+0", 64).0, 1.0);
        assert_eq!(
            parse_float_bytes(b"0x1.00000000000018p+0", 64).0.to_bits(),
            1.0f64.to_bits() + 2
        );
        let (overflow, ok) = parse_float_bytes(b"0x1p+1024", 64);
        assert!(overflow.is_infinite());
        assert!(!ok);
    }

    #[test]
    fn parse_matches_go_hex_boundary_table() {
        let cases: &[(&[u8], u64)] = &[
            (b"0x1.fffffffffffffp1023", f64::MAX.to_bits()),
            (b"0x1.fffffffffffff7fffp1023", f64::MAX.to_bits()),
            (b"0x1.fffffffffffff7p-1010", 0x00df_ffff_ffff_ffff),
            (b"0x1.fffffffffffff8p-1010", 0x00e0_0000_0000_0000),
            (b"0x0.fffffffffffff0p-1022", 0x000f_ffff_ffff_ffff),
            (b"0x0.ffffffffffffe7p-1022", 0x000f_ffff_ffff_fffe),
            (b"0x1.ffffffffffffe8p-1023", 0x000f_ffff_ffff_ffff),
            (b"0x0.00000003456788p-1022", 0x0000_0000_0034_5678),
            (b"0x0.00000003456789p-1022", 0x0000_0000_0034_5679),
            (b"0x0.00000000000058p-1022", 0x0000_0000_0000_0006),
            (b"0x0.00000000000057p-1022", 0x0000_0000_0000_0005),
            (b"0x0.000000000000081p-1022", 1),
            (b"0x0.00000000000008p-1022", 0),
        ];
        for (input, expected_bits) in cases {
            let (value, ok) = parse_float_bytes(input, 64);
            assert!(ok, "Go accepts {}", String::from_utf8_lossy(input));
            assert_eq!(
                value.to_bits(),
                *expected_bits,
                "input {}",
                String::from_utf8_lossy(input)
            );
        }
        for input in [
            b"0x1.fffffffffffff8p1023".as_slice(),
            b"0x1p1024".as_slice(),
            b"-0x1p2000000000".as_slice(),
        ] {
            let (value, ok) = parse_float_bytes(input, 64);
            assert!(!ok);
            assert!(value.is_infinite());
        }
    }

    #[test]
    fn parse_32_rounds_directly_to_float32() {
        let (value, ok) = parse_float_bytes(b"1.000000059604644775390625", 32);
        assert!(ok);
        assert_eq!(value, 1.0);
        assert_eq!(
            parse_float_bytes(b"0x1p-149", 32).0.to_bits(),
            (f32::from_bits(1) as f64).to_bits()
        );

        let cases: &[(&[u8], u32)] = &[
            (b"0x1.000001p0", 0x3f80_0000),
            (b"0x1.000002p0", 0x3f80_0001),
            (b"0x.ffffff7fp128", 0x7f7f_ffff),
            (b"0x0.1234568p-125", 0x0012_3456),
            (b"0x0.1234569p-125", 0x0012_3457),
            (b"0x0.0000010p-125", 1),
            (b"0x0.00000081p-125", 1),
            (b"0x0.0000008p-125", 0),
        ];
        for (input, expected_bits) in cases {
            let (value, ok) = parse_float_bytes(input, 32);
            assert!(ok, "Go accepts {}", String::from_utf8_lossy(input));
            assert_eq!((value as f32).to_bits(), *expected_bits);
        }
        let (overflow, ok) = parse_float_bytes(b"0x.ffffff8p128", 32);
        assert!(!ok);
        assert!(overflow.is_infinite());
    }

    #[test]
    fn formatting_matches_go_boundary_vectors() {
        assert_eq!(format_float_bytes(-0.0, b'g', -1, 64), b"-0");
        assert_eq!(format_float_bytes(1.25, b'f', 1, 64), b"1.2");
        assert_eq!(format_float_bytes(1.35, b'f', 1, 64), b"1.4");
        assert_eq!(format_float_bytes(1e20, b'e', 2, 64), b"1.00e+20");
        assert_eq!(format_float_bytes(1e-5, b'g', -1, 64), b"1e-05");
        assert_eq!(format_float_bytes(1e-4, b'g', -1, 64), b"0.0001");
        assert_eq!(
            format_float_bytes(1.0, b'b', -1, 64),
            b"4503599627370496p-52"
        );
        assert_eq!(
            format_float_bytes(f64::from_bits(1), b'x', -1, 64),
            b"0x1p-1074"
        );
        assert_eq!(format_float_bytes(1.96875, b'x', 1, 64), b"0x1.0p+01");
        assert_eq!(format_float_bytes(1.0, 0xff, 3, 64), vec![b'%', 0xff]);
    }

    #[test]
    fn formatting_matches_go_126_reference_table() {
        let cases: &[(f64, u8, i64, i64, &str)] = &[
            (1.0, b'e', 5, 64, "1.00000e+00"),
            (1.0, b'f', 5, 64, "1.00000"),
            (1.0, b'g', 5, 64, "1"),
            (1.0, b'x', 5, 64, "0x1.00000p+00"),
            (1_234_567.8, b'g', -1, 64, "1.2345678e+06"),
            (1_234_567.8, b'x', -1, 64, "0x1.2d687cccccccdp+20"),
            (200_000.0, b'g', -1, 64, "200000"),
            (2_000_000.0, b'g', -1, 64, "2e+06"),
            (0.015, b'f', 2, 64, "0.01"),
            (0.016, b'f', 2, 64, "0.02"),
            (0.996_644_984, b'f', 2, 64, "1.00"),
            (400.0, b'g', 2, 64, "4e+02"),
            (40.0, b'g', 2, 64, "40"),
            (0.0004, b'g', 2, 64, "0.0004"),
            (0.00004, b'g', 2, 64, "4e-05"),
            (1.2345e6, b'e', 3, 64, "1.234e+06"),
            (1.2355e6, b'e', 3, 64, "1.236e+06"),
            (1.2345, b'f', 3, 64, "1.234"),
            (1.2355, b'f', 3, 64, "1.236"),
            (1e23, b'e', 17, 64, "9.99999999999999916e+22"),
            (
                1e23,
                b'f',
                17,
                64,
                "99999999999999991611392.00000000000000000",
            ),
            (1e23, b'g', 17, 64, "9.9999999999999992e+22"),
            (1e23, b'f', -1, 64, "100000000000000000000000"),
            (f64::from_bits(1), b'g', -1, 64, "5e-324"),
            (32.0, b'g', 0, 64, "3e+01"),
            (-1.0, b'b', -1, 64, "-4503599627370496p-52"),
            (0.05, b'f', 1, 64, "0.1"),
            (0.05, b'f', 0, 64, "0"),
            (0.5, b'f', 0, 64, "0"),
            (1.5, b'f', 0, 64, "2"),
            (
                2.225_073_858_507_201_2e-308,
                b'g',
                -1,
                64,
                "2.2250738585072014e-308",
            ),
            (2.275_555_555_555_555, b'x', 2, 64, "0x1.23p+01"),
            (3.999_969_482_421_875, b'x', 3, 64, "0x1.000p+02"),
            (2.275_555_551_052_093_5, b'x', 6, 64, "0x1.234568p+01"),
            (200_000.0, b'X', -1, 64, "0X1.86AP+17"),
            (f32::from_bits(1) as f64, b'g', -1, 32, "1e-45"),
            (5.960_464_477_539_063e-8, b'g', -1, 32, "5.9604645e-08"),
        ];
        for (value, format, precision, bit_size, expected) in cases {
            assert_eq!(
                format_float_bytes(*value, *format, *precision, *bit_size),
                expected.as_bytes(),
                "value={value:?} format={} precision={precision} bit_size={bit_size}",
                *format as char
            );
        }
    }
}
