//! Constant value representation and operations.
//!
//! This module implements compile-time constant values and operations
//! for constant folding during type checking.
//!
//! Aligned with Go's go/constant package:
//! - Int values use i64 when possible, BigInt otherwise
//! - Untyped float values use BigRational for exact arithmetic
//! - f64 is reserved for values that have crossed a typed/runtime boundary
//!
//! Vo doesn't support complex numbers.

use num_bigint::{BigInt, Sign};
use num_rational::BigRational;
use num_traits::cast::ToPrimitive;
use num_traits::sign::Signed;
use num_traits::{Num, Zero};
use std::fmt;
use vo_syntax::ast::{BinaryOp, UnaryOp};

// ============================================================================
// Part 1: Constants and Types
// ============================================================================

/// Maximum bit width of either side of an exact integer/rational constant.
/// Keeping this independent of the host pointer width makes constant folding
/// deterministic on native and wasm targets.
pub const MAX_CONSTANT_BITS: u64 = 1 << 16; // 65,536 bits (8 KiB per BigInt limb payload)

/// Maximum temporary width created by one operation on two already-bounded
/// exact values. Final values are still checked against `MAX_CONSTANT_BITS`.
const MAX_CONSTANT_WORK_BITS: u64 = MAX_CONSTANT_BITS * 2 + 1;

/// Maximum UTF-8 payload retained by one folded string constant.
pub const MAX_CONSTANT_STRING_BYTES: usize = 1 << 20; // 1 MiB

/// Aggregate input/output payload processed by constant folding in one package.
pub const MAX_CONSTANT_FOLD_WORK_BYTES: u64 = 64 << 20; // 64 MiB

/// Largest constant shift count accepted by the language implementation.
pub const MAX_CONSTANT_SHIFT: u32 = (MAX_CONSTANT_BITS - 1) as u32;

/// A valid constant expression that exceeds the compiler's bounded exact
/// arithmetic domain.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ConstantError {
    MagnitudeTooLarge { max_bits: u64 },
    StringTooLarge { max_bytes: usize },
    AllocationFailed,
    FloatingPointOverflow,
}

impl fmt::Display for ConstantError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::MagnitudeTooLarge { max_bits } => write!(
                f,
                "constant magnitude exceeds the exact-arithmetic limit of {max_bits} bits"
            ),
            Self::StringTooLarge { max_bytes } => write!(
                f,
                "string constant exceeds the constant-folding limit of {max_bytes} UTF-8 bytes"
            ),
            Self::AllocationFailed => {
                write!(f, "constant folding could not allocate its bounded result")
            }
            Self::FloatingPointOverflow => {
                write!(f, "constant floating-point operation overflows its type")
            }
        }
    }
}

impl std::error::Error for ConstantError {}

/// Value Kind - the type of a constant value.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kind {
    Unknown,
    Bool,
    String,
    Int,
    Float,
}

/// Constant values for compile-time evaluation.
///
/// Int values have two representations:
/// - `Int64(i64)` for values that fit in i64
/// - `IntBig(BigInt)` for larger values
///
/// Float values have two representations:
/// - `Rat(BigRational)` for exact rational values
/// - `Float(f64)` for approximate values
///
/// Vo doesn't support complex numbers.
#[derive(Clone, Debug, Default)]
pub enum Value {
    /// Unknown value (due to error).
    #[default]
    Unknown,
    /// Boolean constant.
    Bool(bool),
    /// String constant.
    Str(String),
    /// Integer constant that fits in i64.
    Int64(i64),
    /// Integer constant that doesn't fit in i64.
    IntBig(BigInt),
    /// Exact rational constant (for precise arithmetic).
    Rat(BigRational),
    /// Approximate float constant.
    Float(f64),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        compare(self, BinaryOp::Eq, other)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Unknown => write!(f, "unknown"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Str(s) => write!(f, "\"{}\"", short_quote_str(s, 72)),
            Value::Int64(i) => write!(f, "{}", i),
            Value::IntBig(i) => write!(f, "{}", i),
            Value::Rat(r) => {
                if r.is_integer() {
                    write!(f, "{}", r.numer())
                } else {
                    write!(f, "{}", r)
                }
            }
            Value::Float(v) => write!(f, "{}", v),
        }
    }
}

impl Value {
    /// Returns the kind of this value.
    pub fn kind(&self) -> Kind {
        match self {
            Value::Unknown => Kind::Unknown,
            Value::Bool(_) => Kind::Bool,
            Value::Str(_) => Kind::String,
            Value::Int64(_) | Value::IntBig(_) => Kind::Int,
            Value::Rat(_) | Value::Float(_) => Kind::Float,
        }
    }

    /// Returns true if this is an unknown value.
    pub fn is_unknown(&self) -> bool {
        matches!(self, Value::Unknown)
    }
}

// ============================================================================
// Part 3: Internal Conversions
// ============================================================================

fn i64_to_big(x: i64) -> BigInt {
    BigInt::from(x)
}

fn i64_to_rat(x: i64) -> BigRational {
    BigRational::from_integer(BigInt::from(x))
}

fn i64_to_f64(x: i64) -> f64 {
    x as f64
}

fn big_to_rat(x: &BigInt) -> BigRational {
    BigRational::from_integer(x.clone())
}

fn big_to_f64(x: &BigInt) -> f64 {
    x.to_f64().unwrap_or_else(|| {
        if x.is_negative() {
            f64::NEG_INFINITY
        } else {
            f64::INFINITY
        }
    })
}

fn rat_to_f64(x: &BigRational) -> f64 {
    x.to_f64().unwrap_or_else(|| {
        if x.is_negative() {
            f64::NEG_INFINITY
        } else {
            f64::INFINITY
        }
    })
}

// ============================================================================
// Part 4: Factory Functions (Make*)
// ============================================================================

/// Returns the Unknown value.
pub fn make_unknown() -> Value {
    Value::Unknown
}

/// Returns the Bool value for b.
pub fn make_bool(b: bool) -> Value {
    Value::Bool(b)
}

/// Copies a source string into a bounded folded constant.
pub fn try_make_string(s: &str) -> Result<Value, ConstantError> {
    ensure_string_bytes(s.len())?;
    let mut value = String::new();
    value
        .try_reserve_exact(s.len())
        .map_err(|_| ConstantError::AllocationFailed)?;
    value.push_str(s);
    Ok(Value::Str(value))
}

/// Returns the Int value for x.
pub fn make_int64(x: i64) -> Value {
    Value::Int64(x)
}

/// Returns the Int value for x.
pub fn make_uint64(x: u64) -> Value {
    if x <= i64::MAX as u64 {
        Value::Int64(x as i64)
    } else {
        Value::IntBig(BigInt::from(x))
    }
}

/// Returns the Float value for x.
/// If x is not finite, the result is Unknown.
pub fn make_float64(x: f64) -> Value {
    if x.is_infinite() || x.is_nan() {
        return Value::Unknown;
    }
    // Note: preserve -0.0 sign bit (IEEE 754 semantics)
    if x != 0.0 {
        // Use rational for exact representation
        if let Some(r) = BigRational::from_float(x) {
            return Value::Rat(r);
        }
    }
    Value::Float(x)
}

/// Internal: creates Int value, choosing Int64 or IntBig based on size.
fn make_int(x: BigInt) -> Value {
    if let Some(i) = x.to_i64() {
        Value::Int64(i)
    } else {
        Value::IntBig(x)
    }
}

fn magnitude_limit() -> ConstantError {
    ConstantError::MagnitudeTooLarge {
        max_bits: MAX_CONSTANT_BITS,
    }
}

fn ensure_bits(bits: u64) -> Result<(), ConstantError> {
    if bits > MAX_CONSTANT_BITS {
        Err(magnitude_limit())
    } else {
        Ok(())
    }
}

fn checked_bit_sum(left: u64, right: u64) -> Result<u64, ConstantError> {
    let bits = left.checked_add(right).ok_or_else(magnitude_limit)?;
    ensure_bits(bits)?;
    Ok(bits)
}

fn checked_work_bit_sum(left: u64, right: u64) -> Result<u64, ConstantError> {
    let bits = left.checked_add(right).ok_or_else(magnitude_limit)?;
    if bits > MAX_CONSTANT_WORK_BITS {
        Err(magnitude_limit())
    } else {
        Ok(bits)
    }
}

fn ensure_string_bytes(bytes: usize) -> Result<(), ConstantError> {
    if bytes > MAX_CONSTANT_STRING_BYTES {
        Err(ConstantError::StringTooLarge {
            max_bytes: MAX_CONSTANT_STRING_BYTES,
        })
    } else {
        Ok(())
    }
}

fn ensure_big_int(value: &BigInt) -> Result<(), ConstantError> {
    ensure_bits(value.bits())
}

fn ensure_rational(value: &BigRational) -> Result<(), ConstantError> {
    ensure_big_int(value.numer())?;
    ensure_big_int(value.denom())
}

fn ensure_value(value: &Value) -> Result<(), ConstantError> {
    match value {
        Value::IntBig(value) => ensure_big_int(value),
        Value::Rat(value) => ensure_rational(value),
        Value::Str(value) => ensure_string_bytes(value.len()),
        _ => Ok(()),
    }
}

fn bits_to_bytes(bits: u64) -> Result<u64, ConstantError> {
    bits.checked_add(7)
        .map(|rounded| rounded / 8)
        .ok_or_else(magnitude_limit)
}

/// Approximate retained payload used to charge deterministic package-level
/// constant-fold work. Container overhead is intentionally omitted; the
/// payload dominates the adversarial cases this budget bounds.
pub fn constant_storage_bytes(value: &Value) -> Result<u64, ConstantError> {
    ensure_value(value)?;
    match value {
        Value::Unknown => Ok(0),
        Value::Bool(_) => Ok(1),
        Value::Str(value) => u64::try_from(value.len()).map_err(|_| magnitude_limit()),
        Value::Int64(_) | Value::Float(_) => Ok(8),
        Value::IntBig(value) => bits_to_bytes(value.bits()),
        Value::Rat(value) => bits_to_bytes(value.numer().bits())?
            .checked_add(bits_to_bytes(value.denom().bits())?)
            .ok_or_else(magnitude_limit),
    }
}

/// Returns the deterministic payload charge for constant-fold inputs and
/// outputs. Compiler components that evaluate constants outside the main type
/// checker use this function to enforce the same package-level work budget.
pub fn constant_fold_work_bytes(values: &[&Value]) -> Result<u64, ConstantError> {
    values.iter().try_fold(0u64, |total, value| {
        total
            .checked_add(constant_storage_bytes(value)?)
            .ok_or_else(magnitude_limit)
    })
}

fn make_int_checked(value: BigInt) -> Result<Value, ConstantError> {
    ensure_big_int(&value)?;
    Ok(make_int(value))
}

fn make_rat_checked(value: BigRational) -> Result<Value, ConstantError> {
    ensure_rational(&value)?;
    Ok(make_rat(value))
}

fn make_float_checked(value: f64) -> Result<Value, ConstantError> {
    if value.is_finite() {
        Ok(Value::Float(value))
    } else {
        Err(ConstantError::FloatingPointOverflow)
    }
}

/// Rejects a literal before allocating its normalized digit buffer only when
/// every value with this many significant radix digits is outside the exact
/// constant domain. The final parsed integer still receives an exact bit-width
/// check, avoiding false rejections at the decimal boundary.
fn ensure_literal_digit_count(digits: u64, radix: u32) -> Result<(), ConstantError> {
    if digits == 0 {
        return Ok(());
    }

    let trailing_digits = digits - 1;
    let minimum_bits = match radix {
        2 => digits,
        8 => trailing_digits
            .checked_mul(3)
            .and_then(|bits| bits.checked_add(1))
            .ok_or_else(magnitude_limit)?,
        // floor((digits - 1) * log2(10)) + 1 is the minimum width of a
        // decimal value with `digits` significant digits. 3.321 is a strict
        // lower bound for log2(10), so this preflight can admit a few extra
        // digits but cannot reject an in-budget value.
        10 => trailing_digits
            .checked_mul(3_321)
            .map(|bits| bits / 1_000)
            .and_then(|bits| bits.checked_add(1))
            .ok_or_else(magnitude_limit)?,
        16 => trailing_digits
            .checked_mul(4)
            .and_then(|bits| bits.checked_add(1))
            .ok_or_else(magnitude_limit)?,
        _ => return Err(magnitude_limit()),
    };
    ensure_bits(minimum_bits)
}

#[derive(Debug)]
struct NormalizedSignificand {
    digits: String,
    fraction_digits: usize,
    discarded_trailing_digits: usize,
}

/// Validates a radix significand and constructs a compact, allocation-bounded
/// digit buffer. Underscores and leading zeroes never contribute to the
/// allocation. Float significands also discard radix trailing zeroes and
/// report their scale adjustment separately. A decimal point is accepted only
/// when `allow_point` is true.
fn normalize_significand(
    input: &str,
    radix: u32,
    allow_point: bool,
) -> Result<Option<NormalizedSignificand>, ConstantError> {
    let mut saw_digit = false;
    let mut saw_nonzero = false;
    let mut saw_point = false;
    let mut significant_digits = 0u64;
    let mut trailing_zero_digits = 0u64;
    let mut fraction_digits = 0usize;

    for byte in input.bytes() {
        if byte == b'_' {
            continue;
        }
        if byte == b'.' {
            if !allow_point || saw_point {
                return Ok(None);
            }
            saw_point = true;
            continue;
        }

        let Some(digit) = (byte as char).to_digit(radix) else {
            return Ok(None);
        };
        saw_digit = true;
        if saw_point {
            fraction_digits = fraction_digits.checked_add(1).ok_or_else(magnitude_limit)?;
        }
        if saw_nonzero || digit != 0 {
            saw_nonzero = true;
            significant_digits = significant_digits
                .checked_add(1)
                .ok_or_else(magnitude_limit)?;
            if digit == 0 {
                trailing_zero_digits = trailing_zero_digits
                    .checked_add(1)
                    .ok_or_else(magnitude_limit)?;
            } else {
                trailing_zero_digits = 0;
            }
        }
    }

    if !saw_digit {
        return Ok(None);
    }
    let discarded_trailing_digits = if allow_point { trailing_zero_digits } else { 0 };
    let retained_digits = significant_digits
        .checked_sub(discarded_trailing_digits)
        .ok_or_else(magnitude_limit)?;
    ensure_literal_digit_count(retained_digits, radix)?;

    let capacity = if retained_digits == 0 {
        1
    } else {
        usize::try_from(retained_digits).map_err(|_| magnitude_limit())?
    };
    let mut digits = String::new();
    digits
        .try_reserve_exact(capacity)
        .map_err(|_| ConstantError::AllocationFailed)?;

    let mut retaining = false;
    let mut remaining = retained_digits;
    for byte in input.bytes() {
        if byte == b'_' || byte == b'.' {
            continue;
        }
        if remaining > 0 && (retaining || byte != b'0') {
            retaining = true;
            digits.push(byte as char);
            remaining -= 1;
        }
    }
    if digits.is_empty() {
        digits.push('0');
    }

    Ok(Some(NormalizedSignificand {
        digits,
        fraction_digits,
        discarded_trailing_digits: usize::try_from(discarded_trailing_digits)
            .map_err(|_| magnitude_limit())?,
    }))
}

/// Internal: creates an exact untyped Float value from a rational.
fn make_rat(x: BigRational) -> Value {
    Value::Rat(x)
}

// ============================================================================
// Part 5: Literal Parsing
// ============================================================================

/// Literal kind for parsing.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LitKind {
    Int,
    Float,
    Char,
    String,
}

/// Returns the corresponding value for a literal string.
pub fn make_from_literal(lit: &str, kind: LitKind) -> Value {
    match kind {
        LitKind::Int => int_from_literal(lit),
        LitKind::Float => float_from_literal(lit),
        LitKind::Char => {
            // Char literals should already be parsed to their value
            Value::Unknown
        }
        LitKind::String => {
            // String literals should already be unquoted
            Value::Unknown
        }
    }
}

/// Detects the radix and digit start position for an integer literal.
/// Returns (radix, start_index).
fn detect_int_radix(lit: &str) -> (u32, usize) {
    let mut characters = lit
        .char_indices()
        .filter(|(_, character)| *character != '_');
    let Some((first_index, first)) = characters.next() else {
        return (10, 0);
    };
    let Some((second_index, second)) = characters.next() else {
        return (10, 0);
    };
    if first == '0' {
        match second {
            'x' | 'X' => return (16, second_index + second.len_utf8()),
            'o' | 'O' => return (8, second_index + second.len_utf8()),
            'b' | 'B' => return (2, second_index + second.len_utf8()),
            _ if second.is_ascii_digit() => {
                // Go-style octal: 0644. Keep separators between the leading
                // zero and the remaining digits in the scanned suffix.
                return (8, first_index + first.len_utf8());
            }
            _ => {}
        }
    }
    (10, 0)
}

fn radix_prefix_end(lit: &str, marker: char) -> Option<usize> {
    let mut characters = lit
        .char_indices()
        .filter(|(_, character)| *character != '_');
    let (_, first) = characters.next()?;
    let (second_index, second) = characters.next()?;
    (first == '0' && second.eq_ignore_ascii_case(&marker))
        .then_some(second_index + second.len_utf8())
}

/// Parses an integer literal string.
pub fn int_from_literal(lit: &str) -> Value {
    try_int_from_literal(lit).unwrap_or(Value::Unknown)
}

/// Parses an integer literal within the shared exact-constant bit budget.
pub fn try_int_from_literal(lit: &str) -> Result<Value, ConstantError> {
    let (negative, body) = if let Some(rest) = lit.strip_prefix('-') {
        (true, rest)
    } else if let Some(rest) = lit.strip_prefix('+') {
        (false, rest)
    } else {
        (false, lit)
    };
    let (radix, start) = detect_int_radix(body);
    let Some(normalized) = normalize_significand(&body[start..], radix, false)? else {
        return Ok(Value::Unknown);
    };
    let digits = normalized.digits;

    // Try parsing as i64 first (fast path)
    if let Ok(mut value) = i64::from_str_radix(&digits, radix) {
        if negative {
            value = -value;
        }
        return Ok(Value::Int64(value));
    }

    // Try parsing as BigInt for large numbers
    match BigInt::from_str_radix(&digits, radix) {
        Ok(mut value) => {
            if negative {
                value = -value;
            }
            make_int_checked(value)
        }
        Err(_) => Ok(Value::Unknown),
    }
}

/// Parses a float literal string.
///
/// Callers that own diagnostics should use [`try_float_from_literal`] so a
/// valid literal outside the bounded exact-arithmetic domain cannot be mistaken
/// for an ordinary invalid literal.
pub fn float_from_literal(lit: &str) -> Value {
    try_float_from_literal(lit).unwrap_or(Value::Unknown)
}

/// Parses a float literal without approximating an out-of-budget exact value.
pub fn try_float_from_literal(lit: &str) -> Result<Value, ConstantError> {
    // Check for hex float literal (0x...p...)
    let unsigned = lit.strip_prefix(['-', '+']).unwrap_or(lit);
    if radix_prefix_end(unsigned, 'x').is_some() {
        return parse_hex_float(lit);
    }

    parse_decimal_float(lit)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ParsedExponent {
    Finite(i64),
    PositiveOverflow,
    NegativeOverflow,
}

/// Parses a signed decimal exponent while retaining the direction of values
/// that exceed `i64`. The scan ignores separators and does not allocate, so an
/// adversarial exponent cannot force a same-sized temporary string.
fn parse_exponent(exp: &str) -> Option<ParsedExponent> {
    let mut bytes = exp.bytes().filter(|byte| *byte != b'_');
    let first = bytes.next()?;
    let (negative, first_digit) = match first {
        b'-' => (true, None),
        b'+' => (false, None),
        byte => (false, Some(byte)),
    };
    let limit = if negative {
        (i64::MAX as u64) + 1
    } else {
        i64::MAX as u64
    };
    let mut magnitude = 0u64;
    let mut overflow = false;
    let mut saw_digit = false;

    for byte in first_digit.into_iter().chain(bytes) {
        if !byte.is_ascii_digit() {
            return None;
        }
        saw_digit = true;
        if !overflow {
            match magnitude
                .checked_mul(10)
                .and_then(|value| value.checked_add(u64::from(byte - b'0')))
            {
                Some(value) if value <= limit => magnitude = value,
                _ => overflow = true,
            }
        }
    }
    if !saw_digit {
        return None;
    }
    if overflow {
        return Some(if negative {
            ParsedExponent::NegativeOverflow
        } else {
            ParsedExponent::PositiveOverflow
        });
    }
    if negative {
        if magnitude == limit {
            Some(ParsedExponent::Finite(i64::MIN))
        } else {
            Some(ParsedExponent::Finite(-(magnitude as i64)))
        }
    } else {
        Some(ParsedExponent::Finite(magnitude as i64))
    }
}

fn scaled_float_limit(significand: &BigInt) -> Result<Value, ConstantError> {
    if significand.is_zero() {
        return Ok(Value::Rat(BigRational::zero()));
    }
    Err(magnitude_limit())
}

fn adjusted_literal_exponent(
    exponent: i64,
    fraction_digits: usize,
    discarded_trailing_digits: usize,
    exponent_per_digit: i128,
) -> Result<i64, ConstantError> {
    let fraction = i128::try_from(fraction_digits).map_err(|_| magnitude_limit())?;
    let trailing = i128::try_from(discarded_trailing_digits).map_err(|_| magnitude_limit())?;
    let scale = i128::from(exponent)
        .checked_sub(
            fraction
                .checked_mul(exponent_per_digit)
                .ok_or_else(magnitude_limit)?,
        )
        .and_then(|scale| {
            trailing
                .checked_mul(exponent_per_digit)
                .and_then(|adjustment| scale.checked_add(adjustment))
        })
        .ok_or_else(magnitude_limit)?;
    i64::try_from(scale).map_err(|_| magnitude_limit())
}

fn ensure_power_exponent(radix: u32, exponent: u64) -> Result<(), ConstantError> {
    let minimum_bits = match radix {
        2 => exponent.checked_add(1).ok_or_else(magnitude_limit)?,
        // 3.321 is a strict lower bound for log2(10). The exact power is
        // checked after construction, so this guard bounds work without
        // creating an artificial rejection at MAX_CONSTANT_BITS.
        10 => exponent
            .checked_mul(3_321)
            .map(|bits| bits / 1_000)
            .and_then(|bits| bits.checked_add(1))
            .ok_or_else(magnitude_limit)?,
        _ => return Err(magnitude_limit()),
    };
    ensure_bits(minimum_bits)
}

/// Builds `significand * radix^exponent` exactly when its predicted factor and
/// result both fit the shared constant bit budget.
fn make_scaled_float(
    significand: BigInt,
    radix: u32,
    exponent: i64,
) -> Result<Value, ConstantError> {
    if significand.is_zero() {
        return Ok(Value::Rat(BigRational::zero()));
    }

    ensure_big_int(&significand)?;
    let exponent_magnitude = exponent.unsigned_abs();
    ensure_power_exponent(radix, exponent_magnitude)?;
    let power = u32::try_from(exponent_magnitude).map_err(|_| magnitude_limit())?;
    let factor = BigInt::from(radix).pow(power);
    ensure_big_int(&factor)?;
    let value = if exponent >= 0 {
        checked_work_bit_sum(significand.bits(), factor.bits())?;
        BigRational::from_integer(significand * factor)
    } else {
        BigRational::new(significand, factor)
    };
    make_rat_checked(value)
}

/// Parses a decimal floating-point literal as an exact rational. Converting
/// through the host `f64` here would collapse distinct constants above 53 bits
/// before constant folding has a chance to evaluate them.
fn parse_decimal_float(lit: &str) -> Result<Value, ConstantError> {
    let (negative, body) = if let Some(rest) = lit.strip_prefix('-') {
        (true, rest)
    } else if let Some(rest) = lit.strip_prefix('+') {
        (false, rest)
    } else {
        (false, lit)
    };

    let (mantissa, exponent) = if let Some(pos) = body.find(['e', 'E']) {
        let exponent = &body[pos + 1..];
        if exponent.contains(['e', 'E']) {
            return Ok(Value::Unknown);
        }
        (
            &body[..pos],
            match parse_exponent(exponent) {
                Some(exp) => exp,
                None => return Ok(Value::Unknown),
            },
        )
    } else {
        (body, ParsedExponent::Finite(0))
    };

    let Some(normalized) = normalize_significand(mantissa, 10, true)? else {
        return Ok(Value::Unknown);
    };
    let mut significand = match BigInt::from_str_radix(&normalized.digits, 10) {
        Ok(value) => value,
        Err(_) => return Ok(Value::Unknown),
    };
    if negative {
        significand = -significand;
    }

    match exponent {
        ParsedExponent::Finite(exp) => match adjusted_literal_exponent(
            exp,
            normalized.fraction_digits,
            normalized.discarded_trailing_digits,
            1,
        ) {
            Ok(scale) => make_scaled_float(significand, 10, scale),
            Err(_) => scaled_float_limit(&significand),
        },
        ParsedExponent::PositiveOverflow | ParsedExponent::NegativeOverflow => {
            scaled_float_limit(&significand)
        }
    }
}

/// Parses a hexadecimal floating-point literal.
/// Format: 0x[mantissa]p[exponent] where mantissa is hex and exponent is decimal.
/// Examples: 0x1p0 = 1.0, 0x1p-2 = 0.25, 0x1.8p0 = 1.5
fn parse_hex_float(lit: &str) -> Result<Value, ConstantError> {
    let (negative, body) = if let Some(rest) = lit.strip_prefix('-') {
        (true, rest)
    } else if let Some(rest) = lit.strip_prefix('+') {
        (false, rest)
    } else {
        (false, lit)
    };
    let Some(prefix_end) = radix_prefix_end(body, 'x') else {
        return Ok(Value::Unknown);
    };
    let body = &body[prefix_end..];

    // Find 'p' or 'P' (required for hex float)
    let p_pos = body.find(['p', 'P']);
    let p_pos = match p_pos {
        Some(pos) => pos,
        None => return Ok(Value::Unknown), // hex float must have exponent
    };
    if body[p_pos + 1..].contains(['p', 'P']) {
        return Ok(Value::Unknown);
    }

    let mantissa = &body[..p_pos];
    let exp_str = &body[p_pos + 1..];
    let Some(normalized) = normalize_significand(mantissa, 16, true)? else {
        return Ok(Value::Unknown);
    };
    let mut significand = match BigInt::from_str_radix(&normalized.digits, 16) {
        Ok(value) => value,
        Err(_) => return Ok(Value::Unknown),
    };
    if negative {
        significand = -significand;
    }
    let exponent = match parse_exponent(exp_str) {
        Some(exp) => exp,
        None => return Ok(Value::Unknown),
    };
    match exponent {
        ParsedExponent::Finite(exp) => match adjusted_literal_exponent(
            exp,
            normalized.fraction_digits,
            normalized.discarded_trailing_digits,
            4,
        ) {
            Ok(scale) => make_scaled_float(significand, 2, scale),
            Err(_) => scaled_float_limit(&significand),
        },
        ParsedExponent::PositiveOverflow | ParsedExponent::NegativeOverflow => {
            scaled_float_limit(&significand)
        }
    }
}

// ============================================================================
// Part 6: Accessors
// ============================================================================

/// Returns the Go boolean value of x.
/// x must be Bool or Unknown. If x is Unknown, returns false.
pub fn bool_val(x: &Value) -> bool {
    match x {
        Value::Bool(b) => *b,
        Value::Unknown => false,
        _ => panic!("{:?} not a Bool", x),
    }
}

/// Returns the Go string value of x.
/// x must be String or Unknown. If x is Unknown, returns "".
pub fn string_val(x: &Value) -> &str {
    match x {
        Value::Str(s) => s,
        Value::Unknown => "",
        _ => panic!("{:?} not a String", x),
    }
}

/// Returns the Go int64 value of x and whether the result is exact.
/// x must be Int or Unknown.
pub fn int64_val(x: &Value) -> (i64, bool) {
    match x {
        Value::Int64(i) => (*i, true),
        Value::IntBig(i) => (i.to_i64().unwrap_or(0), i.to_i64().is_some()),
        Value::Unknown => (0, false),
        _ => panic!("{:?} not an Int", x),
    }
}

/// Returns the Go uint64 value of x and whether the result is exact.
/// x must be Int or Unknown.
pub fn uint64_val(x: &Value) -> (u64, bool) {
    match x {
        Value::Int64(i) => {
            if *i >= 0 {
                (*i as u64, true)
            } else {
                (0, false)
            }
        }
        Value::IntBig(i) => {
            if let Some(u) = i.to_u64() {
                (u, true)
            } else {
                (0, false)
            }
        }
        Value::Unknown => (0, false),
        _ => panic!("{:?} not an Int", x),
    }
}

/// Returns the nearest Go float64 value of x and whether the result is exact.
/// x must be numeric or Unknown.
pub fn float64_val(x: &Value) -> (f64, bool) {
    match x {
        Value::Int64(i) => {
            let f = *i as f64;
            (f, f as i64 == *i)
        }
        Value::IntBig(i) => {
            let f = big_to_f64(i);
            (f, false) // BigInt to f64 is generally not exact
        }
        Value::Rat(r) => {
            let f = rat_to_f64(r);
            // Check if exact by converting back
            if let Some(r2) = BigRational::from_float(f) {
                (f, r2 == *r)
            } else {
                (f, false)
            }
        }
        Value::Float(f) => (*f, true),
        Value::Unknown => (0.0, false),
        _ => panic!("{:?} not numeric", x),
    }
}

/// Returns -1, 0, or 1 depending on whether x < 0, x == 0, or x > 0.
/// x must be numeric or Unknown. If x is Unknown, returns 1.
pub fn sign(x: &Value) -> i32 {
    match x {
        Value::Int64(i) => {
            if *i < 0 {
                -1
            } else if *i > 0 {
                1
            } else {
                0
            }
        }
        Value::IntBig(i) => match i.sign() {
            Sign::Minus => -1,
            Sign::NoSign => 0,
            Sign::Plus => 1,
        },
        Value::Rat(r) => {
            if r.is_negative() {
                -1
            } else if r.is_zero() {
                0
            } else {
                1
            }
        }
        Value::Float(f) => {
            if *f < 0.0 {
                -1
            } else if *f > 0.0 {
                1
            } else {
                0
            }
        }
        Value::Unknown => 1, // Avoid spurious division by zero
        _ => panic!("{:?} not numeric", x),
    }
}

/// Returns the number of bits required to represent the absolute value of x.
/// x must be Int or Unknown. If x is Unknown, returns 0.
pub fn bit_len(x: &Value) -> usize {
    match x {
        Value::Int64(i) => {
            let u = i.unsigned_abs();
            64 - u.leading_zeros() as usize
        }
        Value::IntBig(i) => i.bits() as usize,
        Value::Unknown => 0,
        _ => panic!("{:?} not an Int", x),
    }
}

// ============================================================================
// Part 7: Conversion Functions (To*)
// ============================================================================

/// Converts x to an Int value if x is representable as an Int.
/// Otherwise returns Unknown.
pub fn to_int(x: &Value) -> Value {
    match x {
        Value::Int64(_) | Value::IntBig(_) => x.clone(),
        Value::Rat(r) => {
            if r.is_integer() {
                make_int(r.numer().clone())
            } else {
                Value::Unknown
            }
        }
        Value::Float(f) => {
            // Check if f is an integer
            if f.fract() == 0.0 && f.is_finite() {
                if let Some(i) = f.to_i64() {
                    return Value::Int64(i);
                }
                // Try BigInt
                if let Some(r) = BigRational::from_float(*f) {
                    if r.is_integer() {
                        return make_int(r.numer().clone());
                    }
                }
            }
            Value::Unknown
        }
        _ => Value::Unknown,
    }
}

/// Converts x to a Float value if x is representable as a Float.
/// Otherwise returns Unknown.
pub fn to_float(x: &Value) -> Value {
    match x {
        Value::Int64(i) => Value::Rat(i64_to_rat(*i)),
        Value::IntBig(i) => Value::Rat(big_to_rat(i)),
        Value::Rat(_) | Value::Float(_) => x.clone(),
        _ => Value::Unknown,
    }
}

// ============================================================================
// Part 8: Fraction Operations
// ============================================================================

/// Returns the numerator of x; x must be Int, Float, or Unknown.
pub fn num(x: &Value) -> Value {
    match x {
        Value::Int64(_) | Value::IntBig(_) => x.clone(),
        Value::Rat(r) => make_int(r.numer().clone()),
        Value::Float(f) => {
            if let Some(r) = BigRational::from_float(*f) {
                make_int(r.numer().clone())
            } else {
                Value::Unknown
            }
        }
        Value::Unknown => Value::Unknown,
        _ => panic!("{:?} not Int or Float", x),
    }
}

/// Returns the denominator of x; x must be Int, Float, or Unknown.
pub fn denom(x: &Value) -> Value {
    match x {
        Value::Int64(_) | Value::IntBig(_) => Value::Int64(1),
        Value::Rat(r) => make_int(r.denom().clone()),
        Value::Float(f) => {
            if let Some(r) = BigRational::from_float(*f) {
                make_int(r.denom().clone())
            } else {
                Value::Unknown
            }
        }
        Value::Unknown => Value::Unknown,
        _ => panic!("{:?} not Int or Float", x),
    }
}

// ============================================================================
// Part 9: Type Matching
// ============================================================================

/// Returns the order of a value for type matching.
fn ord(x: &Value) -> i32 {
    match x {
        Value::Unknown => 0,
        Value::Bool(_) | Value::Str(_) => 1,
        Value::Int64(_) => 2,
        Value::IntBig(_) => 3,
        Value::Rat(_) => 4,
        Value::Float(_) => 5,
    }
}

/// Matches two values to the same type (the more complex one).
fn match_values(x: Value, y: Value) -> (Value, Value) {
    let ox = ord(&x);
    let oy = ord(&y);

    if ox < oy {
        (promote(x, &y), y)
    } else if ox > oy {
        (x.clone(), promote(y, &x))
    } else {
        (x, y)
    }
}

/// Promotes x to match the type of target.
fn promote(x: Value, target: &Value) -> Value {
    match target {
        Value::IntBig(_) => match x {
            Value::Int64(i) => Value::IntBig(i64_to_big(i)),
            _ => x,
        },
        Value::Rat(_) => match x {
            Value::Int64(i) => Value::Rat(i64_to_rat(i)),
            Value::IntBig(i) => Value::Rat(big_to_rat(&i)),
            _ => x,
        },
        Value::Float(_) => match x {
            Value::Int64(i) => Value::Float(i64_to_f64(i)),
            Value::IntBig(i) => Value::Float(big_to_f64(&i)),
            Value::Rat(r) => Value::Float(rat_to_f64(&r)),
            _ => x,
        },
        _ => x,
    }
}

// ============================================================================
// Part 10: Operations
// ============================================================================

/// Returns the result of the unary expression op y.
/// If prec > 0, it specifies the ^ (xor) result size in bits.
pub fn unary_op(op: UnaryOp, y: &Value, prec: u32) -> Value {
    try_unary_op(op, y, prec).unwrap_or(Value::Unknown)
}

/// Fallible constant unary evaluation with the shared exact-value budget.
pub fn try_unary_op(op: UnaryOp, y: &Value, prec: u32) -> Result<Value, ConstantError> {
    ensure_value(y)?;
    let value = match op {
        UnaryOp::Pos => y.clone(),
        UnaryOp::Neg => match y {
            Value::Unknown => Value::Unknown,
            Value::Int64(i) => {
                if let Some(neg) = i.checked_neg() {
                    Value::Int64(neg)
                } else {
                    // Overflow: -i64::MIN
                    Value::IntBig(-i64_to_big(*i))
                }
            }
            Value::IntBig(i) => make_int_checked(-i)?,
            Value::Rat(r) => make_rat_checked(-r)?,
            Value::Float(f) => make_float_checked(-f)?,
            _ => Value::Unknown,
        },
        UnaryOp::Not => match y {
            Value::Unknown => Value::Unknown,
            Value::Bool(b) => Value::Bool(!b),
            _ => Value::Unknown,
        },
        UnaryOp::BitNot => match y {
            Value::Unknown => Value::Unknown,
            Value::Int64(i) => {
                let mut z = !i64_to_big(*i);
                if prec > 0 {
                    ensure_bits(u64::from(prec))?;
                    // For unsigned types, limit precision
                    let mask = (BigInt::from(1) << prec as usize) - 1;
                    z &= mask;
                }
                make_int_checked(z)?
            }
            Value::IntBig(i) => {
                let mut z = !i.clone();
                if prec > 0 {
                    ensure_bits(u64::from(prec))?;
                    let mask = (BigInt::from(1) << prec as usize) - 1;
                    z &= mask;
                }
                make_int_checked(z)?
            }
            _ => Value::Unknown,
        },
        // Addr and Deref are not compile-time operations
        UnaryOp::Addr | UnaryOp::Deref => Value::Unknown,
    };
    Ok(value)
}

/// Returns the result of the binary expression x op y.
/// Does not handle comparisons or shifts.
pub fn binary_op(x: &Value, op: BinaryOp, y: &Value) -> Value {
    try_binary_op(x, op, y).unwrap_or(Value::Unknown)
}

fn preflight_rational_binary(
    left: &BigRational,
    op: BinaryOp,
    right: &BigRational,
) -> Result<(), ConstantError> {
    let left_num = left.numer().bits();
    let left_den = left.denom().bits();
    let right_num = right.numer().bits();
    let right_den = right.denom().bits();

    match op {
        BinaryOp::Add | BinaryOp::Sub => {
            let left_product = checked_work_bit_sum(left_num, right_den)?;
            let right_product = checked_work_bit_sum(right_num, left_den)?;
            checked_work_bit_sum(left_product.max(right_product), 1)?;
            checked_work_bit_sum(left_den, right_den)?;
        }
        BinaryOp::Mul => {
            checked_work_bit_sum(left_num, right_num)?;
            checked_work_bit_sum(left_den, right_den)?;
        }
        BinaryOp::Div => {
            checked_work_bit_sum(left_num, right_den)?;
            checked_work_bit_sum(left_den, right_num)?;
        }
        _ => {}
    }
    Ok(())
}

/// Fallible constant binary evaluation with pre-allocation complexity checks.
pub fn try_binary_op(x: &Value, op: BinaryOp, y: &Value) -> Result<Value, ConstantError> {
    ensure_value(x)?;
    ensure_value(y)?;
    if x.is_unknown() || y.is_unknown() {
        return Ok(Value::Unknown);
    }

    let (x, y) = match_values(x.clone(), y.clone());

    let value = match (&x, &y) {
        (Value::Bool(a), Value::Bool(b)) => match op {
            BinaryOp::LogAnd => Value::Bool(*a && *b),
            BinaryOp::LogOr => Value::Bool(*a || *b),
            _ => Value::Unknown,
        },

        (Value::Int64(a), Value::Int64(b)) => {
            match op {
                BinaryOp::Add => {
                    if let Some(c) = a.checked_add(*b) {
                        Value::Int64(c)
                    } else {
                        make_int_checked(i64_to_big(*a) + i64_to_big(*b))?
                    }
                }
                BinaryOp::Sub => {
                    if let Some(c) = a.checked_sub(*b) {
                        Value::Int64(c)
                    } else {
                        make_int_checked(i64_to_big(*a) - i64_to_big(*b))?
                    }
                }
                BinaryOp::Mul => {
                    if let Some(c) = a.checked_mul(*b) {
                        Value::Int64(c)
                    } else {
                        make_int_checked(i64_to_big(*a) * i64_to_big(*b))?
                    }
                }
                BinaryOp::Div => {
                    if *b == 0 {
                        Value::Unknown
                    } else if let Some(c) = a.checked_div(*b) {
                        Value::Int64(c)
                    } else {
                        // i64::MIN / -1 overflows, use BigInt
                        make_int_checked(i64_to_big(*a) / i64_to_big(*b))?
                    }
                }
                BinaryOp::Rem => {
                    if *b == 0 {
                        Value::Unknown
                    } else if let Some(c) = a.checked_rem(*b) {
                        Value::Int64(c)
                    } else {
                        // i64::MIN % -1 overflows, use BigInt (result is 0)
                        make_int_checked(i64_to_big(*a) % i64_to_big(*b))?
                    }
                }
                BinaryOp::And => Value::Int64(a & b),
                BinaryOp::Or => Value::Int64(a | b),
                BinaryOp::Xor => Value::Int64(a ^ b),
                BinaryOp::AndNot => Value::Int64(a & !b),
                _ => Value::Unknown,
            }
        }

        (Value::IntBig(a), Value::IntBig(b)) => match op {
            BinaryOp::Add | BinaryOp::Sub => {
                checked_work_bit_sum(a.bits().max(b.bits()), 1)?;
                if op == BinaryOp::Add {
                    make_int_checked(a + b)?
                } else {
                    make_int_checked(a - b)?
                }
            }
            BinaryOp::Mul => {
                checked_work_bit_sum(a.bits(), b.bits())?;
                make_int_checked(a * b)?
            }
            BinaryOp::Div => {
                if b.is_zero() {
                    Value::Unknown
                } else {
                    // Integer division (truncated, like Go)
                    make_int_checked(a / b)?
                }
            }
            BinaryOp::Rem => {
                if b.is_zero() {
                    Value::Unknown
                } else {
                    make_int_checked(a % b)?
                }
            }
            BinaryOp::And | BinaryOp::Or | BinaryOp::Xor | BinaryOp::AndNot => {
                checked_work_bit_sum(a.bits().max(b.bits()), 1)?;
                let result = match op {
                    BinaryOp::And => a & b,
                    BinaryOp::Or => a | b,
                    BinaryOp::Xor => a ^ b,
                    BinaryOp::AndNot => a & !b,
                    _ => unreachable!(),
                };
                make_int_checked(result)?
            }
            _ => Value::Unknown,
        },

        (Value::Rat(a), Value::Rat(b)) => {
            preflight_rational_binary(a, op, b)?;
            match op {
                BinaryOp::Add => make_rat_checked(a + b)?,
                BinaryOp::Sub => make_rat_checked(a - b)?,
                BinaryOp::Mul => make_rat_checked(a * b)?,
                BinaryOp::Div => {
                    if b.is_zero() {
                        Value::Unknown
                    } else {
                        make_rat_checked(a / b)?
                    }
                }
                _ => Value::Unknown,
            }
        }

        (Value::Float(a), Value::Float(b)) => match op {
            BinaryOp::Add => make_float_checked(a + b)?,
            BinaryOp::Sub => make_float_checked(a - b)?,
            BinaryOp::Mul => make_float_checked(a * b)?,
            BinaryOp::Div => make_float_checked(a / b)?,
            _ => Value::Unknown,
        },

        (Value::Str(a), Value::Str(b)) => match op {
            BinaryOp::Add => {
                let len = a
                    .len()
                    .checked_add(b.len())
                    .ok_or(ConstantError::StringTooLarge {
                        max_bytes: MAX_CONSTANT_STRING_BYTES,
                    })?;
                ensure_string_bytes(len)?;
                let mut result = String::new();
                result
                    .try_reserve_exact(len)
                    .map_err(|_| ConstantError::AllocationFailed)?;
                result.push_str(a);
                result.push_str(b);
                Value::Str(result)
            }
            _ => Value::Unknown,
        },

        _ => Value::Unknown,
    };
    ensure_value(&value)?;
    Ok(value)
}

/// Returns the result of the shift expression x op s.
/// op must be Shl or Shr.
pub fn shift(x: &Value, op: BinaryOp, s: u32) -> Value {
    try_shift(x, op, s).unwrap_or(Value::Unknown)
}

/// Fallible constant shift with a pre-allocation bit-width check.
pub fn try_shift(x: &Value, op: BinaryOp, s: u32) -> Result<Value, ConstantError> {
    ensure_value(x)?;
    if s > MAX_CONSTANT_SHIFT {
        return Err(magnitude_limit());
    }
    if s == 0 {
        return Ok(x.clone());
    }

    let value = match x {
        Value::Unknown => Value::Unknown,
        Value::Int64(i) => {
            match op {
                BinaryOp::Shl => {
                    // Left shift may overflow
                    let value = i64_to_big(*i);
                    if !value.is_zero() {
                        checked_bit_sum(value.bits(), u64::from(s))?;
                    }
                    make_int_checked(value << s as usize)?
                }
                // Use the same unbounded signed-integer semantics as IntBig.
                // A native i64 shift panics when the source-level constant
                // count reaches the machine width even though such shifts are
                // valid constant expressions and have a well-defined result.
                BinaryOp::Shr => make_int_checked(i64_to_big(*i) >> s as usize)?,
                _ => Value::Unknown,
            }
        }
        Value::IntBig(i) => match op {
            BinaryOp::Shl => {
                if !i.is_zero() {
                    checked_bit_sum(i.bits(), u64::from(s))?;
                }
                make_int_checked(i << s as usize)?
            }
            BinaryOp::Shr => make_int_checked(i >> s as usize)?,
            _ => Value::Unknown,
        },
        _ => Value::Unknown,
    };
    Ok(value)
}

/// Returns the result of the comparison x op y.
/// If one operand is Unknown, the result is false.
pub fn compare(x: &Value, op: BinaryOp, y: &Value) -> bool {
    if x.is_unknown() || y.is_unknown() {
        return false;
    }

    let (x, y) = match_values(x.clone(), y.clone());

    match (&x, &y) {
        (Value::Bool(a), Value::Bool(b)) => match op {
            BinaryOp::Eq => a == b,
            BinaryOp::NotEq => a != b,
            _ => false,
        },

        (Value::Int64(a), Value::Int64(b)) => cmp_ord(a.cmp(b), op),
        (Value::IntBig(a), Value::IntBig(b)) => cmp_ord(a.cmp(b), op),
        (Value::Rat(a), Value::Rat(b)) => cmp_ord(a.cmp(b), op),
        (Value::Float(a), Value::Float(b)) => match a.partial_cmp(b) {
            Some(ord) => cmp_ord(ord, op),
            None => false,
        },

        (Value::Str(a), Value::Str(b)) => cmp_ord(a.cmp(b), op),

        _ => false,
    }
}

/// Helper for comparison operations.
fn cmp_ord(ord: std::cmp::Ordering, op: BinaryOp) -> bool {
    use std::cmp::Ordering;
    match op {
        BinaryOp::Eq => ord == Ordering::Equal,
        BinaryOp::NotEq => ord != Ordering::Equal,
        BinaryOp::Lt => ord == Ordering::Less,
        BinaryOp::LtEq => ord != Ordering::Greater,
        BinaryOp::Gt => ord == Ordering::Greater,
        BinaryOp::GtEq => ord != Ordering::Less,
        _ => false,
    }
}

// ============================================================================
// Part 11: Helpers
// ============================================================================

/// Shortens a string for display.
fn short_quote_str(s: &str, max: usize) -> String {
    if s.len() <= max {
        s.to_string()
    } else {
        let mut end = max.saturating_sub(3).min(s.len());
        while !s.is_char_boundary(end) {
            end -= 1;
        }
        format!("{}...", &s[..end])
    }
}

// ============================================================================
// Part 12: Value Methods
// ============================================================================

impl Value {
    /// Convert to integer if possible (returns Cow for efficiency).
    pub fn to_int(&self) -> std::borrow::Cow<'_, Value> {
        let result = to_int(self);
        if result.is_unknown() && !self.is_unknown() {
            std::borrow::Cow::Borrowed(self)
        } else {
            std::borrow::Cow::Owned(result)
        }
    }

    /// Returns sign as i32 (-1, 0, or 1).
    pub fn sign(&self) -> i32 {
        sign(self)
    }

    /// Returns string value for Str variant.
    pub fn str_as_string(&self) -> &str {
        match self {
            Value::Str(s) => s,
            _ => "",
        }
    }

    /// Returns (i64, exact) for Int variants.
    pub fn int_as_i64(&self) -> (i64, bool) {
        int64_val(self)
    }

    /// Returns (u64, exact) for Int variants.
    pub fn int_as_u64(&self) -> (u64, bool) {
        uint64_val(self)
    }

    /// Returns true if this is an Int value.
    pub fn is_int(&self) -> bool {
        matches!(self, Value::Int64(_) | Value::IntBig(_))
    }

    /// Returns true if this is a Bool value.
    pub fn is_bool(&self) -> bool {
        matches!(self, Value::Bool(_))
    }

    /// Returns true if this is a String value.
    pub fn is_string(&self) -> bool {
        matches!(self, Value::Str(_))
    }

    /// Returns the integer value as i64 if possible.
    pub fn int_val(&self) -> Option<i64> {
        match self {
            Value::Int64(i) => Some(*i),
            Value::IntBig(i) => i.to_i64(),
            _ => None,
        }
    }

    /// Returns BigInt if this is an Int value.
    pub fn as_big_int(&self) -> Option<BigInt> {
        match self {
            Value::Int64(i) => Some(BigInt::from(*i)),
            Value::IntBig(i) => Some(i.clone()),
            _ => None,
        }
    }

    /// Check if value can be represented as the given basic type.
    pub fn representable(
        &self,
        base: &crate::typ::BasicDetail,
        rounded: Option<&mut Value>,
    ) -> bool {
        use crate::typ::{BasicInfo, BasicType};
        use num_traits::ToPrimitive;

        if self.is_unknown() {
            return true; // avoid follow-up errors
        }

        match base.info() {
            BasicInfo::IsInteger => {
                let int_val = to_int(self);
                match &int_val {
                    Value::Int64(i) => {
                        if let Some(r) = rounded {
                            *r = Value::Int64(*i);
                        }
                        match base.typ() {
                            BasicType::Int => true,
                            BasicType::Int8 => *i >= i8::MIN as i64 && *i <= i8::MAX as i64,
                            BasicType::Int16 => *i >= i16::MIN as i64 && *i <= i16::MAX as i64,
                            BasicType::Int32 | BasicType::Rune => {
                                *i >= i32::MIN as i64 && *i <= i32::MAX as i64
                            }
                            BasicType::Int64 => true,
                            BasicType::Uint => *i >= 0,
                            BasicType::Uint8 | BasicType::Byte => *i >= 0 && *i <= u8::MAX as i64,
                            BasicType::Uint16 => *i >= 0 && *i <= u16::MAX as i64,
                            BasicType::Uint32 => *i >= 0 && *i <= u32::MAX as i64,
                            BasicType::Uint64 => *i >= 0,
                            BasicType::UntypedInt | BasicType::UntypedRune => true,
                            _ => false,
                        }
                    }
                    Value::IntBig(i) => {
                        if let Some(r) = rounded {
                            *r = Value::IntBig(i.clone());
                        }
                        match base.typ() {
                            BasicType::Int => i.to_i64().is_some(),
                            BasicType::Int8 => i.to_i8().is_some(),
                            BasicType::Int16 => i.to_i16().is_some(),
                            BasicType::Int32 | BasicType::Rune => i.to_i32().is_some(),
                            BasicType::Int64 => i.to_i64().is_some(),
                            BasicType::Uint => i.to_u64().is_some(),
                            BasicType::Uint8 | BasicType::Byte => i.to_u8().is_some(),
                            BasicType::Uint16 => i.to_u16().is_some(),
                            BasicType::Uint32 => i.to_u32().is_some(),
                            BasicType::Uint64 => i.to_u64().is_some(),
                            BasicType::UntypedInt | BasicType::UntypedRune => true,
                            _ => false,
                        }
                    }
                    _ => false,
                }
            }
            BasicInfo::IsFloat => {
                let (f, _) = float64_val(self);
                match base.typ() {
                    BasicType::UntypedFloat => true,
                    BasicType::Float64 => {
                        let ok = f.is_finite();
                        if ok {
                            if let Some(r) = rounded {
                                *r = make_float64(f);
                            }
                        }
                        ok
                    }
                    BasicType::Float32 => {
                        let f32_ = f as f32;
                        let ok = f.is_finite() && f32_.is_finite();
                        if ok {
                            if let Some(r) = rounded {
                                *r = make_float64(f32_ as f64);
                            }
                        }
                        ok
                    }
                    _ => false,
                }
            }
            BasicInfo::IsBoolean => matches!(self, Value::Bool(_)),
            BasicInfo::IsString => matches!(self, Value::Str(_)),
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::typ::{BasicDetail, BasicInfo, BasicType};

    fn float_type(typ: BasicType, name: &'static str) -> BasicDetail {
        BasicDetail::new(typ, BasicInfo::IsFloat, name)
    }

    #[test]
    fn decimal_float_folding_retains_precision_beyond_f64() {
        let high = float_from_literal("9007199254740993.0");
        let low = float_from_literal("9007199254740992.0");
        let delta = binary_op(&high, BinaryOp::Sub, &low);

        assert!(matches!(to_int(&delta), Value::Int64(1)));
    }

    #[test]
    fn hexadecimal_float_folding_retains_precision_beyond_f64() {
        let high = float_from_literal("0x20000000000001p0");
        let low = float_from_literal("0x20000000000000p0");
        let delta = binary_op(&high, BinaryOp::Sub, &low);

        assert!(matches!(to_int(&delta), Value::Int64(1)));
    }

    #[test]
    fn large_exponent_untyped_floats_remain_exact() {
        let high = float_from_literal("1e1500");
        let low = float_from_literal("9e1499");

        assert!(matches!(high, Value::Rat(_)));
        assert!(matches!(low, Value::Rat(_)));
        assert!(compare(&high, BinaryOp::Gt, &low));
        assert!(!compare(&high, BinaryOp::Eq, &low));
    }

    #[test]
    fn extended_decimal_exponents_compare_exactly() {
        let high = try_float_from_literal("1e5000").unwrap();
        let low = try_float_from_literal("1e4999").unwrap();
        let tiny_high = try_float_from_literal("1e-4999").unwrap();
        let tiny_low = try_float_from_literal("1e-5000").unwrap();

        assert!(matches!(high, Value::Rat(_)));
        assert!(matches!(low, Value::Rat(_)));
        assert!(compare(&high, BinaryOp::Gt, &low));
        assert!(compare(&tiny_high, BinaryOp::Gt, &tiny_low));
        assert!(compare(
            &unary_op(UnaryOp::Neg, &low, 0),
            BinaryOp::Gt,
            &unary_op(UnaryOp::Neg, &high, 0)
        ));
        assert!(compare(
            &high,
            BinaryOp::Eq,
            &try_float_from_literal("1e5000").unwrap()
        ));
    }

    #[test]
    fn extended_hexadecimal_exponents_compare_exactly() {
        let high = try_float_from_literal("0x1p5000").unwrap();
        let low = try_float_from_literal("0x1p4999").unwrap();
        let tiny_high = try_float_from_literal("0x1p-4999").unwrap();
        let tiny_low = try_float_from_literal("0x1p-5000").unwrap();

        assert!(compare(&high, BinaryOp::Gt, &low));
        assert!(compare(&tiny_high, BinaryOp::Gt, &tiny_low));
        assert!(compare(
            &unary_op(UnaryOp::Neg, &low, 0),
            BinaryOp::Gt,
            &unary_op(UnaryOp::Neg, &high, 0)
        ));
        assert!(compare(
            &high,
            BinaryOp::Eq,
            &try_float_from_literal("0x1p5000").unwrap()
        ));
    }

    #[test]
    fn former_scale_boundary_remains_exact() {
        let at_boundary = try_float_from_literal("1e4096").unwrap();
        let above_boundary = try_float_from_literal("1e4097").unwrap();
        let below_negative_boundary = try_float_from_literal("1e-4097").unwrap();
        let ten_thousand = try_float_from_literal("1e10000").unwrap();

        assert!(matches!(at_boundary, Value::Rat(_)));
        assert!(matches!(above_boundary, Value::Rat(_)));
        assert!(matches!(below_negative_boundary, Value::Rat(_)));
        assert!(matches!(ten_thousand, Value::Rat(_)));
        assert!(compare(&above_boundary, BinaryOp::Gt, &at_boundary));
        assert!(compare(&ten_thousand, BinaryOp::Gt, &above_boundary));
    }

    #[test]
    fn out_of_budget_magnitude_is_reported_without_approximation() {
        let expected = ConstantError::MagnitudeTooLarge {
            max_bits: MAX_CONSTANT_BITS,
        };

        assert_eq!(try_float_from_literal("1e20000"), Err(expected));
        assert_eq!(try_float_from_literal("1e-20000"), Err(expected));
        assert_eq!(try_float_from_literal("0x1p70000"), Err(expected));
        assert_eq!(try_float_from_literal("0x1p-70000"), Err(expected));
        assert_eq!(
            try_float_from_literal("0e999999999999999999999999"),
            Ok(Value::Rat(BigRational::zero()))
        );
    }

    #[test]
    fn literal_preflight_preserves_the_exact_numeric_boundary() {
        let boundary = BigInt::from(1u8) << MAX_CONSTANT_SHIFT as usize;
        let decimal = boundary.to_str_radix(10);
        let parsed = try_int_from_literal(&decimal).unwrap();

        assert_eq!(parsed.as_big_int(), Some(boundary.clone()));
        assert_eq!(bit_len(&parsed), MAX_CONSTANT_BITS as usize);

        let outside = (boundary << 1usize).to_str_radix(10);
        assert_eq!(
            try_int_from_literal(&outside),
            Err(ConstantError::MagnitudeTooLarge {
                max_bits: MAX_CONSTANT_BITS,
            })
        );
    }

    #[test]
    fn literal_normalization_keeps_its_allocation_bounded_by_significant_digits() {
        let mut literal = String::new();
        literal.reserve_exact((16 << 20) + 3);
        literal.push_str("0x");
        literal.extend(std::iter::repeat_n('0', 16 << 20));
        literal.push('1');

        assert_eq!(try_int_from_literal(&literal), Ok(Value::Int64(1)));
    }

    #[test]
    fn float_normalization_discards_exact_radix_trailing_zeroes() {
        let decimal = format!("1.{}e5000", "0".repeat(30_000));
        let hexadecimal = format!("0x1.{}p5000", "0".repeat(20_000));

        assert_eq!(
            try_float_from_literal(&decimal),
            try_float_from_literal("1e5000")
        );
        assert_eq!(
            try_float_from_literal(&hexadecimal),
            try_float_from_literal("0x1p5000")
        );
        assert_eq!(
            try_float_from_literal("1000.00e0"),
            try_float_from_literal("1e3")
        );
        assert_eq!(
            try_float_from_literal("0x1000.00p0"),
            try_float_from_literal("0x1p12")
        );
    }

    #[test]
    fn bounded_temporaries_allow_exact_cancellation_and_reduction() {
        let huge = try_float_from_literal("1e19000").unwrap();
        let negative = try_unary_op(UnaryOp::Neg, &huge, 0).unwrap();

        assert_eq!(
            try_binary_op(&huge, BinaryOp::Add, &negative).unwrap(),
            Value::Rat(BigRational::zero())
        );
        assert_eq!(
            try_binary_op(&huge, BinaryOp::Div, &huge).unwrap(),
            Value::Rat(BigRational::from_integer(BigInt::from(1)))
        );
        assert_eq!(
            try_binary_op(&huge, BinaryOp::Mul, &huge),
            Err(ConstantError::MagnitudeTooLarge {
                max_bits: MAX_CONSTANT_BITS,
            })
        );
    }

    #[test]
    fn shift_and_typed_float_operations_share_the_result_budget() {
        let near_limit = Value::IntBig(BigInt::from(1u8) << 65_000usize);
        assert_eq!(
            try_shift(&near_limit, BinaryOp::Shl, 1_000),
            Err(ConstantError::MagnitudeTooLarge {
                max_bits: MAX_CONSTANT_BITS,
            })
        );
        assert!(try_shift(&near_limit, BinaryOp::Shr, 1_000).is_ok());
        assert_eq!(
            try_binary_op(&Value::Float(f64::MAX), BinaryOp::Mul, &Value::Float(2.0)),
            Err(ConstantError::FloatingPointOverflow)
        );
    }

    #[test]
    fn folded_strings_have_a_checked_single_value_limit() {
        let mut value = try_make_string("x").unwrap();
        for _ in 0..20 {
            value = try_binary_op(&value, BinaryOp::Add, &value).unwrap();
        }
        assert_eq!(value.str_as_string().len(), MAX_CONSTANT_STRING_BYTES);
        assert_eq!(
            try_binary_op(&value, BinaryOp::Add, &value),
            Err(ConstantError::StringTooLarge {
                max_bytes: MAX_CONSTANT_STRING_BYTES,
            })
        );
    }

    #[test]
    fn typed_floats_reject_overflow_and_non_finite_values() {
        let float32 = float_type(BasicType::Float32, "float32");
        let float64 = float_type(BasicType::Float64, "float64");
        let untyped_float = float_type(BasicType::UntypedFloat, "untyped float");

        assert!(!float_from_literal("1e39").representable(&float32, None));
        assert!(!float_from_literal("1e400").representable(&float64, None));
        assert!(!int_from_literal(&format!("1{}", "0".repeat(400))).representable(&float64, None));
        assert!(!Value::Float(f64::INFINITY).representable(&float64, None));
        assert!(!Value::Float(f64::NAN).representable(&float64, None));
        assert!(float_from_literal("1e400").representable(&untyped_float, None));
    }

    #[test]
    fn finite_float_boundaries_remain_representable() {
        let float32 = float_type(BasicType::Float32, "float32");
        let float64 = float_type(BasicType::Float64, "float64");

        assert!(float_from_literal("3.4e38").representable(&float32, None));
        assert!(float_from_literal("1e308").representable(&float64, None));
        assert!(float_from_literal("1e-4000").representable(&float64, None));
    }

    #[test]
    fn extended_exact_constants_obey_explicit_float_overflow_and_underflow_rules() {
        let float32 = float_type(BasicType::Float32, "float32");
        let float64 = float_type(BasicType::Float64, "float64");
        let untyped_float = float_type(BasicType::UntypedFloat, "untyped float");
        let huge = try_float_from_literal("1e5000").unwrap();
        let tiny = try_float_from_literal("1e-5000").unwrap();

        assert!(huge.representable(&untyped_float, None));
        assert!(!huge.representable(&float32, None));
        assert!(!huge.representable(&float64, None));

        let mut rounded64 = Value::Unknown;
        assert!(tiny.representable(&float64, Some(&mut rounded64)));
        assert_eq!(float64_val(&rounded64).0.to_bits(), 0.0f64.to_bits());

        let negative_tiny = unary_op(UnaryOp::Neg, &tiny, 0);
        let mut rounded_negative64 = Value::Unknown;
        assert!(negative_tiny.representable(&float64, Some(&mut rounded_negative64)));
        assert_eq!(
            float64_val(&rounded_negative64).0.to_bits(),
            (-0.0f64).to_bits()
        );

        let mut rounded32 = Value::Unknown;
        assert!(tiny.representable(&float32, Some(&mut rounded32)));
        assert_eq!(float64_val(&rounded32).0.to_bits(), 0.0f64.to_bits());
    }

    #[test]
    fn float64_conversion_records_the_rounded_value() {
        let float64 = float_type(BasicType::Float64, "float64");
        let mut rounded = Value::Unknown;

        assert!(int_from_literal("9007199254740993").representable(&float64, Some(&mut rounded)));
        assert_eq!(float64_val(&rounded).0, 9007199254740992.0);
    }

    #[test]
    fn shortening_multibyte_string_uses_utf8_boundary() {
        let shortened = short_quote_str(&"🙂".repeat(30), 72);

        assert!(shortened.ends_with("..."));
        assert!(shortened.len() <= 72);
    }

    #[test]
    fn int64_right_shift_uses_unbounded_signed_integer_semantics() {
        for count in [63, 64, 65, 1074, MAX_CONSTANT_SHIFT] {
            assert_eq!(
                shift(&Value::Int64(1), BinaryOp::Shr, count),
                Value::Int64(0),
                "positive value at shift count {count}"
            );
            assert_eq!(
                shift(&Value::Int64(0), BinaryOp::Shr, count),
                Value::Int64(0),
                "zero at shift count {count}"
            );
            assert_eq!(
                shift(&Value::Int64(-1), BinaryOp::Shr, count),
                Value::Int64(-1),
                "negative value at shift count {count}"
            );
        }

        assert_eq!(
            shift(&Value::Int64(i64::MIN), BinaryOp::Shr, 63),
            Value::Int64(-1)
        );
    }

    #[test]
    fn maximum_language_shift_count_is_safe_for_all_integer_representations() {
        let unsigned = make_uint64(u64::MAX);
        assert_eq!(shift(&unsigned, BinaryOp::Shr, 63), Value::Int64(1));
        assert_eq!(shift(&unsigned, BinaryOp::Shr, 64), Value::Int64(0));

        let wide = Value::IntBig(BigInt::from(1u8) << 128usize);
        assert_eq!(
            shift(&wide, BinaryOp::Shr, MAX_CONSTANT_SHIFT),
            Value::Int64(0)
        );
        assert_eq!(
            shift(&Value::Int64(-1), BinaryOp::Shr, MAX_CONSTANT_SHIFT),
            Value::Int64(-1)
        );

        let largest_power = try_shift(&Value::Int64(1), BinaryOp::Shl, MAX_CONSTANT_SHIFT)
            .expect("the largest in-budget power of two must fold exactly");
        assert_eq!(bit_len(&largest_power), MAX_CONSTANT_BITS as usize);
        assert_eq!(
            try_shift(&Value::Int64(1), BinaryOp::Shl, MAX_CONSTANT_SHIFT + 1),
            Err(ConstantError::MagnitudeTooLarge {
                max_bits: MAX_CONSTANT_BITS,
            })
        );
        assert_eq!(
            try_shift(&Value::Int64(-1), BinaryOp::Shr, MAX_CONSTANT_SHIFT + 1),
            Err(ConstantError::MagnitudeTooLarge {
                max_bits: MAX_CONSTANT_BITS,
            })
        );
    }

    #[test]
    fn bit_len_handles_the_minimum_i64_without_overflow() {
        assert_eq!(bit_len(&Value::Int64(i64::MIN)), 64);
    }
}
