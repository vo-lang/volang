//! Compile-time constant values for the GoX type checker.
//!
//! This module provides arbitrary-precision constant representation
//! for evaluating constant expressions at compile time.

use std::fmt;

use crate::types::{BasicType, UntypedKind};

/// A compile-time constant value.
#[derive(Debug, Clone, PartialEq)]
pub enum Constant {
    /// Boolean constant.
    Bool(bool),

    /// Integer constant (arbitrary precision).
    /// Stored as i128 for simplicity; can be extended to BigInt if needed.
    Int(i128),

    /// Floating-point constant (arbitrary precision).
    /// Stored as f64 for simplicity; can be extended to BigRational if needed.
    Float(f64),

    /// Rune (character) constant.
    Rune(char),

    /// String constant.
    String(String),

    /// Nil constant.
    Nil,
}

impl Constant {
    /// Returns the untyped kind of this constant.
    pub fn kind(&self) -> Option<UntypedKind> {
        match self {
            Constant::Bool(_) => Some(UntypedKind::Bool),
            Constant::Int(_) => Some(UntypedKind::Int),
            Constant::Float(_) => Some(UntypedKind::Float),
            Constant::Rune(_) => Some(UntypedKind::Rune),
            Constant::String(_) => Some(UntypedKind::String),
            Constant::Nil => None,
        }
    }

    /// Returns the default type for this constant.
    pub fn default_type(&self) -> Option<BasicType> {
        self.kind().map(|k| k.default_type())
    }

    /// Returns true if this constant is zero/false/empty.
    pub fn is_zero(&self) -> bool {
        match self {
            Constant::Bool(b) => !*b,
            Constant::Int(i) => *i == 0,
            Constant::Float(f) => *f == 0.0,
            Constant::Rune(c) => *c == '\0',
            Constant::String(s) => s.is_empty(),
            Constant::Nil => true,
        }
    }

    /// Attempts to convert this constant to a boolean.
    pub fn to_bool(&self) -> Option<bool> {
        match self {
            Constant::Bool(b) => Some(*b),
            _ => None,
        }
    }

    /// Attempts to convert this constant to an integer.
    pub fn to_int(&self) -> Option<i128> {
        match self {
            Constant::Int(i) => Some(*i),
            Constant::Rune(c) => Some(*c as i128),
            Constant::Float(f) if f.fract() == 0.0 => Some(*f as i128),
            _ => None,
        }
    }

    /// Attempts to convert this constant to a float.
    pub fn to_float(&self) -> Option<f64> {
        match self {
            Constant::Float(f) => Some(*f),
            Constant::Int(i) => Some(*i as f64),
            Constant::Rune(c) => Some(*c as i128 as f64),
            _ => None,
        }
    }

    /// Checks if this integer constant is representable in the given basic type.
    pub fn is_representable_as(&self, ty: BasicType) -> bool {
        match self {
            Constant::Bool(_) => ty == BasicType::Bool,
            Constant::String(_) => ty == BasicType::String,
            Constant::Nil => false,
            Constant::Rune(c) => {
                let v = *c as i128;
                Self::int_fits(v, ty)
            }
            Constant::Int(i) => {
                let v = *i;
                match ty {
                    BasicType::Float32 | BasicType::Float64 => true, // integers can become floats
                    _ => Self::int_fits(v, ty),
                }
            }
            Constant::Float(f) => {
                let v = *f;
                match ty {
                    BasicType::Float32 => {
                        v >= f32::MIN as f64 && v <= f32::MAX as f64
                    }
                    BasicType::Float64 => true,
                    // Float to int: must be whole number and in range
                    _ if ty.is_integer() && v.fract() == 0.0 => {
                        Self::int_fits(v as i128, ty)
                    }
                    _ => false,
                }
            }
        }
    }

    /// Checks if an integer value fits in the given basic type.
    fn int_fits(v: i128, ty: BasicType) -> bool {
        match ty {
            BasicType::Int8 => v >= i8::MIN as i128 && v <= i8::MAX as i128,
            BasicType::Int16 => v >= i16::MIN as i128 && v <= i16::MAX as i128,
            BasicType::Int32 => v >= i32::MIN as i128 && v <= i32::MAX as i128,
            BasicType::Int64 => v >= i64::MIN as i128 && v <= i64::MAX as i128,
            BasicType::Int => v >= i64::MIN as i128 && v <= i64::MAX as i128, // assume 64-bit
            BasicType::Uint8 => v >= 0 && v <= u8::MAX as i128,
            BasicType::Uint16 => v >= 0 && v <= u16::MAX as i128,
            BasicType::Uint32 => v >= 0 && v <= u32::MAX as i128,
            BasicType::Uint64 => v >= 0 && v <= u64::MAX as i128,
            BasicType::Uint => v >= 0 && v <= u64::MAX as i128, // assume 64-bit
            _ => false,
        }
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constant::Bool(b) => write!(f, "{}", b),
            Constant::Int(i) => write!(f, "{}", i),
            Constant::Float(v) => write!(f, "{}", v),
            Constant::Rune(c) => write!(f, "'{}'", c.escape_default()),
            Constant::String(s) => write!(f, "\"{}\"", s.escape_default()),
            Constant::Nil => write!(f, "nil"),
        }
    }
}

/// Binary operations on constants.
impl Constant {
    /// Adds two constants.
    pub fn add(&self, other: &Constant) -> Option<Constant> {
        match (self, other) {
            (Constant::Int(a), Constant::Int(b)) => Some(Constant::Int(a.checked_add(*b)?)),
            (Constant::Float(a), Constant::Float(b)) => Some(Constant::Float(a + b)),
            (Constant::Int(a), Constant::Float(b)) => Some(Constant::Float(*a as f64 + b)),
            (Constant::Float(a), Constant::Int(b)) => Some(Constant::Float(a + *b as f64)),
            (Constant::String(a), Constant::String(b)) => {
                Some(Constant::String(format!("{}{}", a, b)))
            }
            (Constant::Rune(a), Constant::Int(b)) => {
                let v = (*a as i128).checked_add(*b)?;
                char::from_u32(v as u32).map(Constant::Rune)
            }
            _ => None,
        }
    }

    /// Subtracts two constants.
    pub fn sub(&self, other: &Constant) -> Option<Constant> {
        match (self, other) {
            (Constant::Int(a), Constant::Int(b)) => Some(Constant::Int(a.checked_sub(*b)?)),
            (Constant::Float(a), Constant::Float(b)) => Some(Constant::Float(a - b)),
            (Constant::Int(a), Constant::Float(b)) => Some(Constant::Float(*a as f64 - b)),
            (Constant::Float(a), Constant::Int(b)) => Some(Constant::Float(a - *b as f64)),
            _ => None,
        }
    }

    /// Multiplies two constants.
    pub fn mul(&self, other: &Constant) -> Option<Constant> {
        match (self, other) {
            (Constant::Int(a), Constant::Int(b)) => Some(Constant::Int(a.checked_mul(*b)?)),
            (Constant::Float(a), Constant::Float(b)) => Some(Constant::Float(a * b)),
            (Constant::Int(a), Constant::Float(b)) => Some(Constant::Float(*a as f64 * b)),
            (Constant::Float(a), Constant::Int(b)) => Some(Constant::Float(a * *b as f64)),
            _ => None,
        }
    }

    /// Divides two constants.
    pub fn div(&self, other: &Constant) -> Option<Constant> {
        match (self, other) {
            (Constant::Int(a), Constant::Int(b)) if *b != 0 => {
                Some(Constant::Int(a.checked_div(*b)?))
            }
            (Constant::Float(a), Constant::Float(b)) if *b != 0.0 => Some(Constant::Float(a / b)),
            (Constant::Int(a), Constant::Float(b)) if *b != 0.0 => {
                Some(Constant::Float(*a as f64 / b))
            }
            (Constant::Float(a), Constant::Int(b)) if *b != 0 => {
                Some(Constant::Float(a / *b as f64))
            }
            _ => None,
        }
    }

    /// Computes the remainder of two constants.
    pub fn rem(&self, other: &Constant) -> Option<Constant> {
        match (self, other) {
            (Constant::Int(a), Constant::Int(b)) if *b != 0 => {
                Some(Constant::Int(a.checked_rem(*b)?))
            }
            _ => None,
        }
    }

    /// Negates a constant.
    pub fn neg(&self) -> Option<Constant> {
        match self {
            Constant::Int(i) => Some(Constant::Int(i.checked_neg()?)),
            Constant::Float(f) => Some(Constant::Float(-f)),
            _ => None,
        }
    }

    /// Logical NOT on a constant.
    pub fn not(&self) -> Option<Constant> {
        match self {
            Constant::Bool(b) => Some(Constant::Bool(!b)),
            _ => None,
        }
    }

    /// Bitwise NOT on a constant.
    pub fn bit_not(&self) -> Option<Constant> {
        match self {
            Constant::Int(i) => Some(Constant::Int(!i)),
            _ => None,
        }
    }

    /// Left shift.
    pub fn shl(&self, other: &Constant) -> Option<Constant> {
        match (self, other) {
            (Constant::Int(a), Constant::Int(b)) if *b >= 0 && *b < 128 => {
                Some(Constant::Int(a.checked_shl(*b as u32)?))
            }
            _ => None,
        }
    }

    /// Right shift.
    pub fn shr(&self, other: &Constant) -> Option<Constant> {
        match (self, other) {
            (Constant::Int(a), Constant::Int(b)) if *b >= 0 && *b < 128 => {
                Some(Constant::Int(a.checked_shr(*b as u32)?))
            }
            _ => None,
        }
    }

    /// Bitwise AND.
    pub fn bit_and(&self, other: &Constant) -> Option<Constant> {
        match (self, other) {
            (Constant::Int(a), Constant::Int(b)) => Some(Constant::Int(a & b)),
            _ => None,
        }
    }

    /// Bitwise OR.
    pub fn bit_or(&self, other: &Constant) -> Option<Constant> {
        match (self, other) {
            (Constant::Int(a), Constant::Int(b)) => Some(Constant::Int(a | b)),
            _ => None,
        }
    }

    /// Bitwise XOR.
    pub fn bit_xor(&self, other: &Constant) -> Option<Constant> {
        match (self, other) {
            (Constant::Int(a), Constant::Int(b)) => Some(Constant::Int(a ^ b)),
            _ => None,
        }
    }

    /// Bit clear (AND NOT).
    pub fn bit_clear(&self, other: &Constant) -> Option<Constant> {
        match (self, other) {
            (Constant::Int(a), Constant::Int(b)) => Some(Constant::Int(a & !b)),
            _ => None,
        }
    }

    /// Equality comparison.
    pub fn eq(&self, other: &Constant) -> Option<Constant> {
        let result = match (self, other) {
            (Constant::Bool(a), Constant::Bool(b)) => a == b,
            (Constant::Int(a), Constant::Int(b)) => a == b,
            (Constant::Float(a), Constant::Float(b)) => a == b,
            (Constant::Rune(a), Constant::Rune(b)) => a == b,
            (Constant::String(a), Constant::String(b)) => a == b,
            (Constant::Nil, Constant::Nil) => true,
            _ => return None,
        };
        Some(Constant::Bool(result))
    }

    /// Less than comparison.
    pub fn lt(&self, other: &Constant) -> Option<Constant> {
        let result = match (self, other) {
            (Constant::Int(a), Constant::Int(b)) => a < b,
            (Constant::Float(a), Constant::Float(b)) => a < b,
            (Constant::String(a), Constant::String(b)) => a < b,
            _ => return None,
        };
        Some(Constant::Bool(result))
    }

    /// Less than or equal comparison.
    pub fn le(&self, other: &Constant) -> Option<Constant> {
        let result = match (self, other) {
            (Constant::Int(a), Constant::Int(b)) => a <= b,
            (Constant::Float(a), Constant::Float(b)) => a <= b,
            (Constant::String(a), Constant::String(b)) => a <= b,
            _ => return None,
        };
        Some(Constant::Bool(result))
    }

    /// Logical AND.
    pub fn and(&self, other: &Constant) -> Option<Constant> {
        match (self, other) {
            (Constant::Bool(a), Constant::Bool(b)) => Some(Constant::Bool(*a && *b)),
            _ => None,
        }
    }

    /// Logical OR.
    pub fn or(&self, other: &Constant) -> Option<Constant> {
        match (self, other) {
            (Constant::Bool(a), Constant::Bool(b)) => Some(Constant::Bool(*a || *b)),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_constant_kind() {
        assert_eq!(Constant::Bool(true).kind(), Some(UntypedKind::Bool));
        assert_eq!(Constant::Int(42).kind(), Some(UntypedKind::Int));
        assert_eq!(Constant::Float(3.14).kind(), Some(UntypedKind::Float));
        assert_eq!(Constant::Rune('a').kind(), Some(UntypedKind::Rune));
        assert_eq!(
            Constant::String("hello".to_string()).kind(),
            Some(UntypedKind::String)
        );
        assert_eq!(Constant::Nil.kind(), None);
    }

    #[test]
    fn test_constant_arithmetic() {
        let a = Constant::Int(10);
        let b = Constant::Int(3);

        assert_eq!(a.add(&b), Some(Constant::Int(13)));
        assert_eq!(a.sub(&b), Some(Constant::Int(7)));
        assert_eq!(a.mul(&b), Some(Constant::Int(30)));
        assert_eq!(a.div(&b), Some(Constant::Int(3)));
        assert_eq!(a.rem(&b), Some(Constant::Int(1)));
    }

    #[test]
    fn test_constant_division_by_zero() {
        let a = Constant::Int(10);
        let zero = Constant::Int(0);

        assert_eq!(a.div(&zero), None);
        assert_eq!(a.rem(&zero), None);
    }

    #[test]
    fn test_constant_string_concat() {
        let a = Constant::String("hello".to_string());
        let b = Constant::String(" world".to_string());

        assert_eq!(
            a.add(&b),
            Some(Constant::String("hello world".to_string()))
        );
    }

    #[test]
    fn test_constant_comparison() {
        let a = Constant::Int(10);
        let b = Constant::Int(20);

        assert_eq!(a.lt(&b), Some(Constant::Bool(true)));
        assert_eq!(a.eq(&b), Some(Constant::Bool(false)));
        assert_eq!(a.le(&a), Some(Constant::Bool(true)));
    }

    #[test]
    fn test_constant_representability() {
        assert!(Constant::Int(127).is_representable_as(BasicType::Int8));
        assert!(!Constant::Int(128).is_representable_as(BasicType::Int8));
        assert!(Constant::Int(255).is_representable_as(BasicType::Uint8));
        assert!(!Constant::Int(256).is_representable_as(BasicType::Uint8));
        assert!(!Constant::Int(-1).is_representable_as(BasicType::Uint8));

        assert!(Constant::Float(3.14).is_representable_as(BasicType::Float64));
        assert!(Constant::Float(3.0).is_representable_as(BasicType::Int)); // whole number
        assert!(!Constant::Float(3.14).is_representable_as(BasicType::Int)); // not whole
    }

    #[test]
    fn test_constant_shift() {
        let a = Constant::Int(1);
        let shift = Constant::Int(10);

        assert_eq!(a.shl(&shift), Some(Constant::Int(1024)));
        assert_eq!(Constant::Int(1024).shr(&shift), Some(Constant::Int(1)));
    }
}
