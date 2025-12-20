//! Constant value representation and operations.
//!
//! This module implements compile-time constant values and operations
//! for constant folding during type checking.

#![allow(dead_code)]

use crate::typ::{BasicDetail, BasicInfo, BasicType};
use gox_syntax::ast::{BinaryOp, UnaryOp};
use num_bigint::{BigInt, Sign};
use num_rational::BigRational;
use num_traits::cast::FromPrimitive;
use num_traits::cast::ToPrimitive;
use num_traits::sign::Signed;
use num_traits::{Num, Zero};
use std::borrow::Cow;
use std::fmt;

type F64 = f64;

/// Constant values for compile-time evaluation.
/// GoX doesn't support complex numbers, so we skip Complex variant.
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    /// Unknown value (due to error).
    Unknown,
    /// Boolean constant.
    Bool(bool),
    /// String constant.
    Str(String),
    /// Integer constant (arbitrary precision).
    Int(BigInt),
    /// Rational constant (for exact division).
    Rat(BigRational),
    /// Float constant.
    Float(F64),
}

impl Default for Value {
    fn default() -> Self {
        Value::Unknown
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Unknown => write!(f, "unknown"),
            Value::Bool(b) => b.fmt(f),
            Value::Str(s) => write!(f, "\"{}\"", s),
            Value::Int(i) => i.fmt(f),
            Value::Rat(r) => r.fmt(f),
            Value::Float(fl) => fl.fmt(f),
        }
    }
}

impl Value {
    pub fn with_bool(b: bool) -> Value {
        Value::Bool(b)
    }

    pub fn with_str(s: String) -> Value {
        Value::Str(s)
    }

    pub fn with_i64(i: i64) -> Value {
        Value::Int(BigInt::from_i64(i).unwrap())
    }

    pub fn with_u64(u: u64) -> Value {
        Value::Int(BigInt::from_u64(u).unwrap())
    }

    pub fn with_f64(f: f64) -> Value {
        Value::Float(f)
    }

    pub fn is_unknown(&self) -> bool {
        matches!(self, Value::Unknown)
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Value::Bool(_))
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Value::Str(_))
    }

    pub fn is_int(&self) -> bool {
        match self {
            Value::Int(_) => true,
            Value::Rat(r) => r.is_integer(),
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Value::Float(_))
    }

    /// Returns the boolean value if this is a Bool.
    pub fn bool_val(&self) -> Option<bool> {
        match self {
            Value::Bool(b) => Some(*b),
            _ => None,
        }
    }

    /// Returns the string value if this is a String.
    pub fn string_val(&self) -> Option<&str> {
        match self {
            Value::Str(s) => Some(s),
            _ => None,
        }
    }

    /// Returns the integer value as i64 if possible.
    pub fn int_val(&self) -> Option<i64> {
        match self {
            Value::Int(i) => i.to_i64(),
            _ => None,
        }
    }

    /// Returns the float value if this is a Float.
    pub fn float_val(&self) -> Option<f64> {
        match self {
            Value::Float(f) => Some(*f),
            _ => None,
        }
    }

    /// Check if value can be represented as the given basic type.
    pub fn representable(&self, base: &BasicDetail, rounded: Option<&mut Value>) -> bool {
        if let Value::Unknown = self {
            return true; // avoid follow-up errors
        }

        let float_representable = |val: &Value, btype: BasicType, rounded: Option<&mut Value>| -> bool {
            match val.to_float() {
                Value::Float(f) => match btype {
                    BasicType::Float64 => true,
                    BasicType::Float32 => {
                        let f32_ = f as f32;
                        let ok = !f32_.is_infinite();
                        if let Some(r) = rounded {
                            *r = Value::Float((f as f32) as f64);
                        }
                        ok
                    }
                    BasicType::UntypedFloat => true,
                    _ => false,
                },
                _ => false,
            }
        };

        match base.info() {
            BasicInfo::IsInteger => match self.to_int() {
                Cow::Owned(Value::Int(ref ival)) | Cow::Borrowed(Value::Int(ref ival)) => {
                    if let Some(r) = rounded {
                        *r = Value::Int(ival.clone())
                    }
                    match base.typ() {
                        BasicType::Int => ival.to_isize().is_some(),
                        BasicType::Int8 => ival.to_i8().is_some(),
                        BasicType::Int16 => ival.to_i16().is_some(),
                        BasicType::Int32 | BasicType::Rune => ival.to_i32().is_some(),
                        BasicType::Int64 => ival.to_i64().is_some(),
                        BasicType::Uint | BasicType::Uintptr => ival.to_usize().is_some(),
                        BasicType::Uint8 | BasicType::Byte => ival.to_u8().is_some(),
                        BasicType::Uint16 => ival.to_u16().is_some(),
                        BasicType::Uint32 => ival.to_u32().is_some(),
                        BasicType::Uint64 => ival.to_u64().is_some(),
                        BasicType::UntypedInt | BasicType::UntypedRune => true,
                        _ => false,
                    }
                }
                _ => false,
            },
            BasicInfo::IsFloat => float_representable(self, base.typ(), rounded),
            BasicInfo::IsBoolean => matches!(self, Value::Bool(_)),
            BasicInfo::IsString => matches!(self, Value::Str(_)),
            _ => false,
        }
    }

    /// Convert to integer if possible.
    pub fn to_int(&self) -> Cow<Value> {
        let f64_to_int = |x: f64| -> Cow<Value> {
            match BigRational::from_f64(x) {
                Some(v) => {
                    if v.is_integer() {
                        Cow::Owned(Value::Int(v.to_integer()))
                    } else {
                        Cow::Owned(Value::Unknown)
                    }
                }
                None => Cow::Owned(Value::Unknown),
            }
        };
        match self {
            Value::Int(_) => Cow::Borrowed(self),
            Value::Rat(r) => {
                if r.is_integer() {
                    Cow::Owned(Value::Int(r.to_integer()))
                } else {
                    Cow::Owned(Value::Unknown)
                }
            }
            Value::Float(f) => f64_to_int(*f),
            _ => Cow::Owned(Value::Unknown),
        }
    }

    /// Convert to float.
    pub fn to_float(&self) -> Value {
        let v = match self {
            Value::Int(i) => i.to_f64(),
            Value::Rat(r) => rat_to_f64(r),
            Value::Float(f) => Some(*f),
            _ => None,
        };
        v.map_or(Value::Unknown, Value::Float)
    }

    /// Returns the sign: -1, 0, or 1.
    pub fn sign(&self) -> isize {
        match self {
            Value::Int(i) => match i.sign() {
                Sign::Plus => 1,
                Sign::Minus => -1,
                Sign::NoSign => 0,
            },
            Value::Rat(r) => {
                if r.is_positive() { 1 } 
                else if r.is_negative() { -1 } 
                else { 0 }
            }
            Value::Float(v) => {
                if *v > 0.0 { 1 } 
                else if *v < 0.0 { -1 } 
                else { 0 }
            }
            Value::Unknown => 1, // avoid spurious division by zero errors
            _ => 0,
        }
    }

    /// Match types for binary operations.
    pub fn match_type<'a>(x: Cow<'a, Value>, y: Cow<'a, Value>) -> (Cow<'a, Value>, Cow<'a, Value>) {
        fn ord(v: &Value) -> u8 {
            match v {
                Value::Unknown => 0,
                Value::Bool(_) => 1,
                Value::Int(_) => 2,
                Value::Rat(_) => 3,
                Value::Float(_) => 4,
                Value::Str(_) => 5,
            }
        }

        let ox = ord(&x);
        let oy = ord(&y);
        if ox == oy {
            return (x, y);
        }

        // Promote to higher type
        match (ox, oy) {
            (2, 3) => (Cow::Owned(int_to_rat(&x)), y), // Int -> Rat
            (3, 2) => (x, Cow::Owned(int_to_rat(&y))),
            (2, 4) => (Cow::Owned(x.to_float()), y), // Int -> Float
            (4, 2) => (x, Cow::Owned(y.to_float())),
            (3, 4) => (Cow::Owned(x.to_float()), y), // Rat -> Float
            (4, 3) => (x, Cow::Owned(y.to_float())),
            _ => (x, y),
        }
    }

    /// Binary operation.
    pub fn binary_op(x: &Value, op: BinaryOp, y: &Value) -> Value {
        let (x, y) = Value::match_type(Cow::Borrowed(x), Cow::Borrowed(y));
        match (&*x, &*y) {
            (Value::Unknown, _) | (_, Value::Unknown) => Value::Unknown,
            (Value::Bool(a), Value::Bool(b)) => match op {
                BinaryOp::LogAnd => Value::Bool(*a && *b),
                BinaryOp::LogOr => Value::Bool(*a || *b),
                _ => Value::Unknown,
            },
            (Value::Int(a), Value::Int(b)) => match op {
                BinaryOp::Add => Value::Int(a + b),
                BinaryOp::Sub => Value::Int(a - b),
                BinaryOp::Mul => Value::Int(a * b),
                BinaryOp::Div => {
                    if b.sign() == Sign::NoSign {
                        Value::Unknown
                    } else {
                        Value::Rat(BigRational::new(a.clone(), b.clone()))
                    }
                }
                BinaryOp::Rem => {
                    if b.sign() == Sign::NoSign {
                        Value::Unknown
                    } else {
                        Value::Int(a % b)
                    }
                }
                BinaryOp::And => Value::Int(a & b),
                BinaryOp::Or => Value::Int(a | b),
                BinaryOp::Xor => Value::Int(a ^ b),
                BinaryOp::AndNot => Value::Int(a & !b),
                _ => Value::Unknown,
            },
            (Value::Rat(a), Value::Rat(b)) => match op {
                BinaryOp::Add => Value::Rat(a + b),
                BinaryOp::Sub => Value::Rat(a - b),
                BinaryOp::Mul => Value::Rat(a * b),
                BinaryOp::Div => {
                    if b.is_zero() {
                        Value::Unknown
                    } else {
                        Value::Rat(a / b)
                    }
                }
                _ => Value::Unknown,
            },
            (Value::Float(a), Value::Float(b)) => match op {
                BinaryOp::Add => Value::Float(a + b),
                BinaryOp::Sub => Value::Float(a - b),
                BinaryOp::Mul => Value::Float(a * b),
                BinaryOp::Div => Value::Float(a / b),
                _ => Value::Unknown,
            },
            (Value::Str(a), Value::Str(b)) => match op {
                BinaryOp::Add => Value::Str(format!("{}{}", a, b)),
                _ => Value::Unknown,
            },
            _ => Value::Unknown,
        }
    }

    /// Unary operation.
    /// If prec > 0, it specifies the ^ (xor) result size in bits.
    pub fn unary_op(op: UnaryOp, y: &Value, prec: usize) -> Value {
        if y.is_unknown() {
            return Value::Unknown;
        }
        match op {
            UnaryOp::Pos => y.clone(),
            UnaryOp::Neg => match y {
                Value::Int(i) => Value::Int(-i),
                Value::Rat(r) => Value::Rat(-r),
                Value::Float(f) => Value::Float(-f),
                _ => Value::Unknown,
            },
            UnaryOp::Not => match y {
                Value::Bool(b) => Value::Bool(!b),
                _ => Value::Unknown,
            },
            UnaryOp::BitNot => match y {
                Value::Int(i) => {
                    let mut v = !i;
                    if prec > 0 {
                        // Mask to prec bytes
                        let mask = (BigInt::from(1) << (prec * 8)) - 1;
                        v = v & mask;
                    }
                    Value::Int(v)
                }
                _ => Value::Unknown,
            },
            _ => Value::Unknown,
        }
    }

    /// Shift operation.
    pub fn shift(x: &Value, op: BinaryOp, s: u64) -> Value {
        match x {
            Value::Unknown => Value::Unknown,
            Value::Int(i) => match op {
                BinaryOp::Shl => Value::Int(i << s),
                BinaryOp::Shr => Value::Int(i >> s),
                _ => Value::Unknown,
            },
            _ => Value::Unknown,
        }
    }

    /// Compare operation.
    pub fn compare(x: &Value, op: BinaryOp, y: &Value) -> bool {
        if x.is_unknown() || y.is_unknown() {
            return false;
        }
        let (x, y) = Value::match_type(Cow::Borrowed(x), Cow::Borrowed(y));
        let cmp = match (&*x, &*y) {
            (Value::Bool(a), Value::Bool(b)) => a.cmp(b),
            (Value::Int(a), Value::Int(b)) => a.cmp(b),
            (Value::Rat(a), Value::Rat(b)) => a.cmp(b),
            (Value::Float(a), Value::Float(b)) => a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal),
            (Value::Str(a), Value::Str(b)) => a.cmp(b),
            _ => return false,
        };
        match op {
            BinaryOp::Eq => cmp == std::cmp::Ordering::Equal,
            BinaryOp::NotEq => cmp != std::cmp::Ordering::Equal,
            BinaryOp::Lt => cmp == std::cmp::Ordering::Less,
            BinaryOp::LtEq => cmp != std::cmp::Ordering::Greater,
            BinaryOp::Gt => cmp == std::cmp::Ordering::Greater,
            BinaryOp::GtEq => cmp != std::cmp::Ordering::Less,
            _ => false,
        }
    }

    /// Returns the bool value.
    pub fn bool_as_bool(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Unknown => false,
            _ => panic!("not a bool"),
        }
    }

    /// Returns the string value.
    pub fn str_as_string(&self) -> String {
        match self {
            Value::Str(s) => s.clone(),
            Value::Unknown => String::new(),
            _ => panic!("not a string"),
        }
    }

    /// int_as_i64 returns the Go int64 value and whether the result is exact.
    pub fn int_as_i64(&self) -> (i64, bool) {
        match self {
            Value::Int(i) => match i.to_i64() {
                Some(v) => (v, true),
                None => (0, false),
            },
            _ => (0, false),
        }
    }

    /// int_as_u64 returns the Go uint64 value and whether the result is exact.
    pub fn int_as_u64(&self) -> (u64, bool) {
        match self {
            Value::Int(i) => match i.to_u64() {
                Some(v) => (v, true),
                None => (0, false),
            },
            _ => (0, false),
        }
    }

    /// num_as_f64 returns float64 value and whether the result is exact.
    pub fn num_as_f64(&self) -> (f64, bool) {
        match self {
            Value::Float(f) => (*f, true),
            Value::Int(i) => (i.to_f64().unwrap_or(0.0), true),
            Value::Rat(r) => (rat_to_f64(r).unwrap_or(0.0), true),
            _ => (0.0, false),
        }
    }

    /// num_as_f32 returns float32 value and whether the result is exact.
    pub fn num_as_f32(&self) -> (f32, bool) {
        match self {
            Value::Int(_) | Value::Rat(_) => {
                let vf = self.to_float();
                if let Value::Float(f) = vf {
                    let f32_ = f as f32;
                    if f32_.is_finite() {
                        (f32_, true)
                    } else {
                        (if self.sign() > 0 { f32::MAX } else { f32::MIN }, false)
                    }
                } else {
                    (if self.sign() > 0 { f32::MAX } else { f32::MIN }, false)
                }
            }
            Value::Float(v) => {
                let f = *v;
                let min = f32::MIN as f64;
                let max = f32::MAX as f64;
                if f > min && f < max {
                    (f as f32, true)
                } else if f <= min {
                    (f32::MIN, false)
                } else {
                    (f32::MAX, false)
                }
            }
            Value::Unknown => (0.0, false),
            _ => panic!("not a number"),
        }
    }
}

// Helper functions

fn rat_to_f64(r: &BigRational) -> Option<f64> {
    let num = r.numer().to_f64()?;
    let den = r.denom().to_f64()?;
    Some(num / den)
}

fn int_to_rat(v: &Value) -> Value {
    match v {
        Value::Int(i) => Value::Rat(BigRational::from_integer(i.clone())),
        _ => Value::Unknown,
    }
}
