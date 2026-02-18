//! strconv package native function implementations.
//!
//! Native functions for float parsing/formatting (requires complex algorithms).
//! Integer parsing/formatting and quote/unquote are implemented in Vo.

#[cfg(not(feature = "std"))]
use alloc::string::String;
#[cfg(not(feature = "std"))]
use alloc::format;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;
#[cfg(not(feature = "std"))]
use alloc::string::ToString;

use vo_ffi_macro::vostd_fn;

// ==================== Float parsing ====================

#[vostd_fn("strconv", "ParseFloat")]
fn parse_float(s: &str, bit_size: i64) -> (f64, bool) {
    match s.trim().parse::<f64>() {
        Ok(v) => {
            if bit_size == 32 {
                // Check if value fits in f32
                let f32_val = v as f32;
                if f32_val.is_infinite() && !v.is_infinite() {
                    return (v, false); // overflow
                }
                (f32_val as f64, true)
            } else {
                (v, true)
            }
        }
        Err(_) => (0.0, false),
    }
}

// ==================== Float formatting ====================

#[vostd_fn("strconv", "FormatFloat")]
fn format_float(f: f64, fmt: u8, prec: i64, bit_size: i64) -> String {
    let f = if bit_size == 32 { (f as f32) as f64 } else { f };
    
    if f.is_nan() { return "NaN".to_string(); }
    if f.is_infinite() {
        return if f > 0.0 { "+Inf".to_string() } else { "-Inf".to_string() };
    }

    match fmt {
        b'b' => {
            // Go 'b' format: binary exponent notation, e.g. -ddddp±ddd
            // Decompose into significand * 2^exp
            let bits = f.to_bits();
            let sign = if bits >> 63 != 0 { "-" } else { "" };
            let biased_exp = ((bits >> 52) & 0x7ff) as i64;
            let mantissa = bits & 0x000fffffffffffff;
            if biased_exp == 0 {
                // Subnormal: significand = mantissa, exp = 1 - 1023 - 52 = -1074
                format!("{}{}p-1074", sign, mantissa)
            } else {
                // Normal: significand = (1<<52)|mantissa, exp = biased_exp - 1023 - 52
                let sig = (1u64 << 52) | mantissa;
                let exp = biased_exp - 1023 - 52;
                format!("{}{}p{:+}", sign, sig, exp)
            }
        }
        b'e' | b'E' => {
            let upper = fmt == b'E';
            format_e(f, prec, upper)
        }
        b'f' | b'F' => {
            format_f(f, prec)
        }
        b'g' => {
            // Go 'g': use 'e' for large/small exponents, 'f' otherwise
            format_g(f, prec, false)
        }
        b'G' => {
            format_g(f, prec, true)
        }
        b'x' => {
            format_hex_float(f, prec, false)
        }
        b'X' => {
            format_hex_float(f, prec, true)
        }
        _ => format!("{}", f),
    }
}

fn format_e(f: f64, prec: i64, upper: bool) -> String {
    if f == 0.0 {
        let negative = f.to_bits() >> 63 != 0;
        let sign = if negative { "-" } else { "" };
        let e_char = if upper { 'E' } else { 'e' };
        if prec < 0 {
            return format!("{}0{}+00", sign, e_char);
        }
        let p = prec as usize;
        if p == 0 {
            return format!("{}0{}+00", sign, e_char);
        }
        let zeros: String = core::iter::repeat('0').take(p).collect();
        return format!("{}0.{}{}+00", sign, zeros, e_char);
    }

    let mut buf = ryu::Buffer::new();
    let ryu_str = buf.format(f);
    let (negative, digits, exp10) = parse_ryu_output(ryu_str);

    if prec < 0 {
        // Shortest representation in e format
        return digits_to_e_format(negative, &digits, exp10, upper);
    }

    let p = prec as usize;
    // Need (p+1) significant digits total (1 before dot + p after)
    let total = p + 1;
    let rounded = round_digits(&digits, total);
    let (mut d, adj) = (rounded.0, rounded.1);
    let rexp = exp10 + adj;
    while d.len() < total {
        d.push(0);
    }
    d.truncate(total);
    digits_to_e_format(negative, &d, rexp, upper)
}

fn format_f(f: f64, prec: i64) -> String {
    if f == 0.0 {
        let negative = f.to_bits() >> 63 != 0;
        let sign = if negative { "-" } else { "" };
        if prec < 0 {
            return format!("{}0", sign);
        }
        let p = prec as usize;
        if p == 0 {
            return format!("{}0", sign);
        }
        let zeros: String = core::iter::repeat('0').take(p).collect();
        return format!("{}0.{}", sign, zeros);
    }

    let mut buf = ryu::Buffer::new();
    let ryu_str = buf.format(f);
    let (negative, digits, exp10) = parse_ryu_output(ryu_str);

    if prec < 0 {
        // Shortest representation in f format
        return digits_to_f_format(negative, &digits, exp10);
    }

    let p = prec as usize;
    // We need digits up to p places after the decimal point.
    // Number of significant digits needed = exp10 + p (if exp10 > 0)
    // or = p - (-exp10) ... but we need to handle carefully
    let total_sig = if exp10 > 0 { exp10 as usize + p } else { p };
    // But we need to account for leading zeros after decimal point
    let needed_digits = if exp10 <= 0 {
        // e.g., exp10=-2, digits start at position 3 after decimal point
        // We need p digits after decimal, but first (-exp10) are zeros
        let zeros_after_dot = (-exp10) as usize;
        if p > zeros_after_dot {
            p - zeros_after_dot
        } else {
            0
        }
    } else {
        total_sig
    };

    let rounded = round_digits(&digits, needed_digits);
    let (rd, adj) = (rounded.0, rounded.1);
    let rexp = exp10 + adj;

    // Build the f-format string with exact p decimal places
    let mut s = String::new();
    if negative { s.push('-'); }

    if rexp <= 0 {
        s.push('0');
        if p > 0 {
            s.push('.');
            let zeros_after_dot = (-rexp) as usize;
            let zeros_to_write = zeros_after_dot.min(p);
            for _ in 0..zeros_to_write {
                s.push('0');
            }
            let remaining = p - zeros_to_write;
            for i in 0..remaining {
                if i < rd.len() {
                    s.push((rd[i] + b'0') as char);
                } else {
                    s.push('0');
                }
            }
        }
    } else {
        let int_digits = rexp as usize;
        // Integer part
        for i in 0..int_digits {
            if i < rd.len() {
                s.push((rd[i] + b'0') as char);
            } else {
                s.push('0');
            }
        }
        // Decimal part
        if p > 0 {
            s.push('.');
            for i in 0..p {
                let idx = int_digits + i;
                if idx < rd.len() {
                    s.push((rd[idx] + b'0') as char);
                } else {
                    s.push('0');
                }
            }
        }
    }
    s
}

/// Parse ryu shortest output into (sign, digits, decimal_exponent).
/// ryu outputs formats like: "1.23E20", "0.5", "1E100", "0.0", "-3.14", etc.
/// Returns (negative, digit_chars_without_dot, base-10 exponent such that value = 0.digits * 10^exp)
fn parse_ryu_output(s: &str) -> (bool, Vec<u8>, i32) {
    let bytes = s.as_bytes();
    let mut i = 0;
    let negative = if bytes[i] == b'-' { i += 1; true } else { false };

    let mut digits = Vec::new();
    let mut dot_pos: Option<usize> = None;
    let mut exp_val: i32 = 0;
    let mut exp_start = bytes.len();

    // Find 'E' or 'e'
    for j in i..bytes.len() {
        if bytes[j] == b'E' || bytes[j] == b'e' {
            exp_start = j;
            // Parse exponent
            let exp_str = &s[j+1..];
            exp_val = exp_str.parse::<i32>().unwrap_or(0);
            break;
        }
    }

    // Collect digits and find dot position
    let mut digit_count_before_dot = 0;
    for j in i..exp_start {
        if bytes[j] == b'.' {
            dot_pos = Some(digits.len());
        } else {
            digits.push(bytes[j] - b'0');
            if dot_pos.is_none() {
                digit_count_before_dot += 1;
            }
        }
    }

    // Compute the exponent in the form: value = d1.d2d3d4... * 10^exp
    // where d1 is the first digit
    // If there was a dot, the implicit exponent from dot position
    // ryu outputs "1.23E20" meaning 1.23 * 10^20, so exp = 20
    // ryu outputs "0.5" meaning 0.5, which is 5 * 10^-1
    // ryu outputs "123.0" meaning 123.0
    // We want: exp10 such that the number = 0.d1d2d3... * 10^exp10
    // For "1.23E20": digits=[1,2,3], the value = 1.23 * 10^20 = 0.123 * 10^21, so exp10 = 21
    // For "0.5": digits=[0,5], but leading zero... actually we need to strip leading zeros.
    // For "123.456": digits=[1,2,3,4,5,6], dot_pos=3, exp_val=0
    //   value = 123.456 = 0.123456 * 10^3, so exp10 = 3
    // For "1.23E20": digits=[1,2,3], dot_pos=1, exp_val=20
    //   value = 1.23 * 10^20 = 0.123 * 10^21, so exp10 = digit_count_before_dot + exp_val = 1 + 20 = 21

    let mut exp10 = digit_count_before_dot as i32 + exp_val;

    // Strip leading zeros from digits (adjusting exponent)
    while digits.len() > 1 && digits[0] == 0 {
        digits.remove(0);
        exp10 -= 1;
    }

    // Strip trailing zeros from digits (for shortest repr)
    while digits.len() > 1 && *digits.last().unwrap() == 0 {
        digits.pop();
    }

    (negative, digits, exp10)
}

/// Format digits in Go 'e' notation: d.dddde±dd
fn digits_to_e_format(negative: bool, digits: &[u8], exp10: i32, upper: bool) -> String {
    let mut s = String::new();
    if negative { s.push('-'); }
    s.push((digits[0] + b'0') as char);
    if digits.len() > 1 {
        s.push('.');
        for &d in &digits[1..] {
            s.push((d + b'0') as char);
        }
    }
    // Go exponent: e±dd (at least 2 digits)
    let e_char = if upper { 'E' } else { 'e' };
    let actual_exp = exp10 - 1; // because we have d.ddd form
    s.push(e_char);
    if actual_exp >= 0 {
        s.push('+');
        s.push_str(&format!("{:02}", actual_exp));
    } else {
        s.push('-');
        s.push_str(&format!("{:02}", -actual_exp));
    }
    s
}

/// Format digits in Go 'f' notation (no exponent)
fn digits_to_f_format(negative: bool, digits: &[u8], exp10: i32) -> String {
    let mut s = String::new();
    if negative { s.push('-'); }

    if exp10 <= 0 {
        // 0.000...digits
        s.push('0');
        s.push('.');
        for _ in 0..(-exp10) {
            s.push('0');
        }
        for &d in digits {
            s.push((d + b'0') as char);
        }
    } else {
        let exp = exp10 as usize;
        if exp >= digits.len() {
            // All digits are before the decimal point
            for &d in digits {
                s.push((d + b'0') as char);
            }
            for _ in 0..(exp - digits.len()) {
                s.push('0');
            }
        } else {
            // Some digits before, some after decimal point
            for &d in &digits[..exp] {
                s.push((d + b'0') as char);
            }
            s.push('.');
            for &d in &digits[exp..] {
                s.push((d + b'0') as char);
            }
        }
    }
    s
}

fn format_g(f: f64, prec: i64, upper: bool) -> String {
    if f == 0.0 {
        // Go's g/G format always outputs "0" for zero, regardless of precision
        let negative = f.to_bits() >> 63 != 0;
        return if negative { "-0".to_string() } else { "0".to_string() };
    }

    // Get shortest representation via ryu
    let mut buf = ryu::Buffer::new();
    let ryu_str = buf.format(f);
    let (negative, digits, exp10) = parse_ryu_output(ryu_str);

    // Go's rule: exp = exp10 - 1 (standard exponent in d.ddd * 10^exp form)
    // For shortest (prec=-1): use e when exp < -4 || exp >= 6 (fixed threshold)
    // For explicit prec: use e when exp < -4 || exp >= prec

    if prec < 0 {
        let exp = exp10 - 1;
        // Go uses fixed eprec=6 for shortest mode
        if exp < -4 || exp >= 6 {
            return digits_to_e_format(negative, &digits, exp10, upper);
        }
        return digits_to_f_format(negative, &digits, exp10);
    }

    let p = if prec == 0 { 1usize } else { prec as usize };

    // Round digits to p significant digits
    let rounded = round_digits(&digits, p);
    let (rd, rexp) = (rounded.0, exp10 + rounded.1);

    // Go 'g': use 'e' when exp < -4 or exp >= p
    let exp = rexp - 1;
    if exp < -4 || exp >= p as i32 {
        let mut d = rd;
        while d.len() < p { d.push(0); }
        d.truncate(p);
        // Strip trailing zeros from fractional part
        while d.len() > 1 && *d.last().unwrap() == 0 { d.pop(); }
        digits_to_e_format(negative, &d, rexp, upper)
    } else {
        let mut d = rd;
        while d.len() < p { d.push(0); }
        d.truncate(p);
        // Strip trailing zeros after decimal point, keeping integer part
        let min_digits = if rexp > 0 { rexp as usize } else { 1 };
        while d.len() > min_digits && *d.last().unwrap() == 0 { d.pop(); }
        digits_to_f_format(negative, &d, rexp)
    }
}

/// Round digit array to n significant digits. Returns (rounded_digits, exponent_adjustment)
fn round_digits(digits: &[u8], n: usize) -> (Vec<u8>, i32) {
    if digits.len() <= n {
        return (digits.to_vec(), 0);
    }
    let mut d: Vec<u8> = digits[..n].to_vec();
    let round_up = digits[n] >= 5;
    if round_up {
        // Propagate carry
        let mut carry = true;
        for i in (0..d.len()).rev() {
            if carry {
                d[i] += 1;
                if d[i] >= 10 {
                    d[i] = 0;
                } else {
                    carry = false;
                }
            }
        }
        if carry {
            // Overflow: e.g. 999 rounded up = 1000
            d.insert(0, 1);
            d.truncate(n);
            return (d, 1); // exponent increased by 1
        }
    }
    (d, 0)
}

fn format_hex_float(f: f64, prec: i64, upper: bool) -> String {
    // Go 'x'/'X': hex float format -0xh.hhhhp±dd
    let bits = f.to_bits();
    let sign = if bits >> 63 != 0 { "-" } else { "" };
    let biased_exp = ((bits >> 52) & 0x7ff) as i64;
    let mantissa = bits & 0x000fffffffffffff;

    let (lead, exp) = if biased_exp == 0 {
        if mantissa == 0 {
            (0u64, 0i64)
        } else {
            // Subnormal: normalize
            (mantissa, -1022 - 52)
        }
    } else {
        ((1u64 << 52) | mantissa, biased_exp - 1023 - 52)
    };

    let p_char = if upper { 'P' } else { 'p' };

    // Convert mantissa to hex digits after the leading digit
    if f == 0.0 {
        let p = if prec < 0 { 0 } else { prec as usize };
        let prefix = if upper { "0X" } else { "0x" };
        return if p > 0 {
            format!("{}{}0.{:0>width$}{}+00", sign, prefix, "", p_char, width = p)
        } else {
            format!("{}{}0{}+00", sign, prefix, p_char)
        };
    }

    // Normalize: shift so leading bit is at position 52
    let mut sig = lead;
    let mut e = exp;
    while sig != 0 && sig < (1u64 << 52) {
        sig <<= 1;
        e -= 1;
    }
    let lead_digit = sig >> 52;
    let frac = sig & 0x000fffffffffffff;
    e += 52;

    let prefix = if upper { "0X" } else { "0x" };
    let hex_chars = if upper { "0123456789ABCDEF" } else { "0123456789abcdef" };

    // Convert fraction to hex string (13 hex digits for 52 bits)
    let mut hex_frac = String::new();
    let mut rem = frac;
    for _ in 0..13 {
        let digit = (rem >> 48) & 0xf;
        hex_frac.push(hex_chars.as_bytes()[digit as usize] as char);
        rem = (rem & 0x0000ffffffffffff) << 4;
    }

    // Trim trailing zeros if prec < 0
    let hex_frac = if prec < 0 {
        hex_frac.trim_end_matches('0').to_string()
    } else {
        let p = prec as usize;
        if p <= hex_frac.len() {
            hex_frac[..p].to_string()
        } else {
            format!("{:0<width$}", hex_frac, width = p)
        }
    };

    if hex_frac.is_empty() {
        format!("{}{}{}{}{}",sign, prefix, lead_digit, p_char, format_exp_signed(e))
    } else {
        format!("{}{}{}.{}{}{}",sign, prefix, lead_digit, hex_frac, p_char, format_exp_signed(e))
    }
}

fn format_exp_signed(e: i64) -> String {
    if e >= 0 { format!("+{:02}", e) } else { format!("-{:02}", -e) }
}

vo_runtime::stdlib_register!(strconv: ParseFloat, FormatFloat);
