//! Constant value representation and operations.
//!
//! This module implements compile-time constant values and operations
//! for constant folding during type checking.
//!
//! Aligned with Go's go/constant package:
//! - Int values use i64 when possible, BigInt otherwise
//! - Float values use BigRational when exact, f64 otherwise
//! - Precision limit of 512 bits for rationals
//!
//! Vo doesn't support complex numbers.

use vo_syntax::ast::{BinaryOp, UnaryOp};
use num_bigint::{BigInt, Sign};
use num_rational::BigRational;
use num_traits::cast::ToPrimitive;
use num_traits::sign::Signed;
use num_traits::{Num, Zero};
use std::fmt;

// ============================================================================
// Part 1: Constants and Types
// ============================================================================

/// Maximum supported mantissa precision (in bits).
/// The Go spec requires at least 256 bits; typical implementations use 512 bits.
/// TODO: Implement overflow check for untyped integers (see Go's types/const.go).
#[allow(dead_code)]
const MAX_PREC: usize = 512;

/// Maximum exponent for "small" rationals that we keep as rationals.
/// Beyond this, we convert to f64.
const MAX_EXP: usize = 4 << 10; // 4096

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
#[derive(Clone, Debug)]
pub enum Value {
    /// Unknown value (due to error).
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

impl Default for Value {
    fn default() -> Self {
        Value::Unknown
    }
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
// Part 2: Precision Control
// ============================================================================

/// Reports whether x would lead to "reasonably"-sized fraction.
fn small_int(x: &BigInt) -> bool {
    x.bits() < MAX_EXP as u64
}

/// Reports whether x would lead to "reasonably"-sized fraction.
fn small_float(x: f64) -> bool {
    if x.is_infinite() {
        return false;
    }
    let (_, exp, _) = decode_f64(x);
    let e = exp as i32 - 1023;
    -(MAX_EXP as i32) < e && e < MAX_EXP as i32
}

/// Decodes f64 into (sign, exponent, mantissa).
fn decode_f64(x: f64) -> (bool, u16, u64) {
    let bits = x.to_bits();
    let sign = (bits >> 63) != 0;
    let exp = ((bits >> 52) & 0x7FF) as u16;
    let mant = bits & 0x000F_FFFF_FFFF_FFFF;
    (sign, exp, mant)
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
    x.to_f64().unwrap_or(f64::INFINITY)
}

fn rat_to_f64(x: &BigRational) -> f64 {
    let num = x.numer().to_f64().unwrap_or(f64::INFINITY);
    let den = x.denom().to_f64().unwrap_or(1.0);
    num / den
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

/// Returns the String value for s.
pub fn make_string(s: String) -> Value {
    Value::Str(s)
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
/// If x is -0.0, the result is 0.0.
/// If x is not finite, the result is Unknown.
pub fn make_float64(x: f64) -> Value {
    if x.is_infinite() || x.is_nan() {
        return Value::Unknown;
    }
    // Convert -0 to 0
    let x = if x == 0.0 { 0.0 } else { x };
    if small_float(x) {
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

/// Internal: creates Float value from rational, checking precision.
fn make_rat(x: BigRational) -> Value {
    let a = x.numer();
    let b = x.denom();
    if small_int(a) && small_int(b) {
        Value::Rat(x)
    } else {
        // Components too large, switch to float
        Value::Float(rat_to_f64(&x))
    }
}

/// Internal: creates Float value from f64.
fn make_float(x: f64) -> Value {
    if x == 0.0 {
        // Normalize -0 to 0
        return Value::Float(0.0);
    }
    if x.is_infinite() || x.is_nan() {
        return Value::Unknown;
    }
    Value::Float(x)
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

/// Parses an integer literal string.
pub fn int_from_literal(lit: &str) -> Value {
    // Remove underscores
    let lit = lit.replace('_', "");
    
    // Try parsing as i64 first (fast path)
    if let Ok(x) = parse_int_str(&lit) {
        return Value::Int64(x);
    }
    
    // Try parsing as BigInt
    let result = if lit.starts_with("0x") || lit.starts_with("0X") {
        BigInt::from_str_radix(&lit[2..], 16)
    } else if lit.starts_with("0o") || lit.starts_with("0O") {
        BigInt::from_str_radix(&lit[2..], 8)
    } else if lit.starts_with("0b") || lit.starts_with("0B") {
        BigInt::from_str_radix(&lit[2..], 2)
    } else {
        BigInt::from_str_radix(&lit, 10)
    };
    
    match result {
        Ok(x) => make_int(x),
        Err(_) => Value::Unknown,
    }
}

/// Helper to parse int string with different bases.
fn parse_int_str(lit: &str) -> Result<i64, std::num::ParseIntError> {
    if lit.starts_with("0x") || lit.starts_with("0X") {
        i64::from_str_radix(&lit[2..], 16)
    } else if lit.starts_with("0o") || lit.starts_with("0O") {
        i64::from_str_radix(&lit[2..], 8)
    } else if lit.starts_with("0b") || lit.starts_with("0B") {
        i64::from_str_radix(&lit[2..], 2)
    } else {
        lit.parse()
    }
}

/// Parses a float literal string.
pub fn float_from_literal(lit: &str) -> Value {
    // Remove underscores
    let lit = lit.replace('_', "");
    
    match lit.parse::<f64>() {
        Ok(f) => make_float64(f),
        Err(_) => Value::Unknown,
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
            if *i < 0 { -1 }
            else if *i > 0 { 1 }
            else { 0 }
        }
        Value::IntBig(i) => match i.sign() {
            Sign::Minus => -1,
            Sign::NoSign => 0,
            Sign::Plus => 1,
        },
        Value::Rat(r) => {
            if r.is_negative() { -1 }
            else if r.is_zero() { 0 }
            else { 1 }
        }
        Value::Float(f) => {
            if *f < 0.0 { -1 }
            else if *f > 0.0 { 1 }
            else { 0 }
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
            let u = if *i < 0 { (-*i) as u64 } else { *i as u64 };
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
        Value::IntBig(i) => {
            if small_int(i) {
                Value::Rat(big_to_rat(i))
            } else {
                Value::Float(big_to_f64(i))
            }
        }
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
    match op {
        UnaryOp::Pos => y.clone(),
        UnaryOp::Neg => {
            match y {
                Value::Unknown => Value::Unknown,
                Value::Int64(i) => {
                    if let Some(neg) = i.checked_neg() {
                        Value::Int64(neg)
                    } else {
                        // Overflow: -i64::MIN
                        Value::IntBig(-i64_to_big(*i))
                    }
                }
                Value::IntBig(i) => make_int(-i),
                Value::Rat(r) => make_rat(-r),
                Value::Float(f) => make_float(-f),
                _ => Value::Unknown,
            }
        }
        UnaryOp::Not => {
            match y {
                Value::Unknown => Value::Unknown,
                Value::Bool(b) => Value::Bool(!b),
                _ => Value::Unknown,
            }
        }
        UnaryOp::BitNot => {
            match y {
                Value::Unknown => Value::Unknown,
                Value::Int64(i) => {
                    let mut z = !i64_to_big(*i);
                    if prec > 0 {
                        // For unsigned types, limit precision
                        let mask = (BigInt::from(1) << prec as usize) - 1;
                        z = z & mask;
                    }
                    make_int(z)
                }
                Value::IntBig(i) => {
                    let mut z = !i.clone();
                    if prec > 0 {
                        let mask = (BigInt::from(1) << prec as usize) - 1;
                        z = z & mask;
                    }
                    make_int(z)
                }
                _ => Value::Unknown,
            }
        }
        // Addr and Deref are not compile-time operations
        UnaryOp::Addr | UnaryOp::Deref => Value::Unknown,
    }
}

/// Returns the result of the binary expression x op y.
/// Does not handle comparisons or shifts.
pub fn binary_op(x: &Value, op: BinaryOp, y: &Value) -> Value {
    if x.is_unknown() || y.is_unknown() {
        return Value::Unknown;
    }
    
    let (x, y) = match_values(x.clone(), y.clone());
    
    match (&x, &y) {
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
                        make_int(i64_to_big(*a) + i64_to_big(*b))
                    }
                }
                BinaryOp::Sub => {
                    if let Some(c) = a.checked_sub(*b) {
                        Value::Int64(c)
                    } else {
                        make_int(i64_to_big(*a) - i64_to_big(*b))
                    }
                }
                BinaryOp::Mul => {
                    if let Some(c) = a.checked_mul(*b) {
                        Value::Int64(c)
                    } else {
                        make_int(i64_to_big(*a) * i64_to_big(*b))
                    }
                }
                BinaryOp::Div => {
                    if *b == 0 {
                        Value::Unknown
                    } else {
                        // Integer division produces rational
                        make_rat(BigRational::new(i64_to_big(*a), i64_to_big(*b)))
                    }
                }
                BinaryOp::Rem => {
                    if *b == 0 {
                        Value::Unknown
                    } else {
                        Value::Int64(a % b)
                    }
                }
                BinaryOp::And => Value::Int64(a & b),
                BinaryOp::Or => Value::Int64(a | b),
                BinaryOp::Xor => Value::Int64(a ^ b),
                BinaryOp::AndNot => Value::Int64(a & !b),
                _ => Value::Unknown,
            }
        }
        
        (Value::IntBig(a), Value::IntBig(b)) => {
            match op {
                BinaryOp::Add => make_int(a + b),
                BinaryOp::Sub => make_int(a - b),
                BinaryOp::Mul => make_int(a * b),
                BinaryOp::Div => {
                    if b.is_zero() {
                        Value::Unknown
                    } else {
                        make_rat(BigRational::new(a.clone(), b.clone()))
                    }
                }
                BinaryOp::Rem => {
                    if b.is_zero() {
                        Value::Unknown
                    } else {
                        make_int(a % b)
                    }
                }
                BinaryOp::And => make_int(a & b),
                BinaryOp::Or => make_int(a | b),
                BinaryOp::Xor => make_int(a ^ b),
                BinaryOp::AndNot => make_int(a & !b),
                _ => Value::Unknown,
            }
        }
        
        (Value::Rat(a), Value::Rat(b)) => {
            match op {
                BinaryOp::Add => make_rat(a + b),
                BinaryOp::Sub => make_rat(a - b),
                BinaryOp::Mul => make_rat(a * b),
                BinaryOp::Div => {
                    if b.is_zero() {
                        Value::Unknown
                    } else {
                        make_rat(a / b)
                    }
                }
                _ => Value::Unknown,
            }
        }
        
        (Value::Float(a), Value::Float(b)) => {
            match op {
                BinaryOp::Add => make_float(a + b),
                BinaryOp::Sub => make_float(a - b),
                BinaryOp::Mul => make_float(a * b),
                BinaryOp::Div => make_float(a / b),
                _ => Value::Unknown,
            }
        }
        
        (Value::Str(a), Value::Str(b)) => {
            match op {
                BinaryOp::Add => Value::Str(format!("{}{}", a, b)),
                _ => Value::Unknown,
            }
        }
        
        _ => Value::Unknown,
    }
}

/// Returns the result of the shift expression x op s.
/// op must be Shl or Shr.
pub fn shift(x: &Value, op: BinaryOp, s: u32) -> Value {
    if s == 0 {
        return x.clone();
    }
    
    match x {
        Value::Unknown => Value::Unknown,
        Value::Int64(i) => {
            match op {
                BinaryOp::Shl => {
                    // Left shift may overflow
                    make_int(i64_to_big(*i) << s as usize)
                }
                BinaryOp::Shr => {
                    Value::Int64(i >> s)
                }
                _ => Value::Unknown,
            }
        }
        Value::IntBig(i) => {
            match op {
                BinaryOp::Shl => make_int(i << s as usize),
                BinaryOp::Shr => make_int(i >> s as usize),
                _ => Value::Unknown,
            }
        }
        _ => Value::Unknown,
    }
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
        (Value::Float(a), Value::Float(b)) => {
            match a.partial_cmp(b) {
                Some(ord) => cmp_ord(ord, op),
                None => false,
            }
        }
        
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
        format!("{}...", &s[..max.saturating_sub(3)])
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
    
    /// Returns the integer value as i64 if possible (legacy).
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
    pub fn representable(&self, base: &crate::typ::BasicDetail, rounded: Option<&mut Value>) -> bool {
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
                            BasicType::Int32 | BasicType::Rune => *i >= i32::MIN as i64 && *i <= i32::MAX as i64,
                            BasicType::Int64 => true,
                            BasicType::Uint | BasicType::Uintptr => *i >= 0,
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
                            BasicType::Int => i.to_isize().is_some(),
                            BasicType::Int8 => i.to_i8().is_some(),
                            BasicType::Int16 => i.to_i16().is_some(),
                            BasicType::Int32 | BasicType::Rune => i.to_i32().is_some(),
                            BasicType::Int64 => i.to_i64().is_some(),
                            BasicType::Uint | BasicType::Uintptr => i.to_usize().is_some(),
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
                    BasicType::Float64 | BasicType::UntypedFloat => true,
                    BasicType::Float32 => {
                        let f32_ = f as f32;
                        let ok = !f32_.is_infinite() || f.is_infinite();
                        if let Some(r) = rounded {
                            *r = make_float64(f32_ as f64);
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
