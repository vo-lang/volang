//! Compile-time constant values for the GoX type checker.
//!
//! This module provides arbitrary-precision constant representation
//! for evaluating constant expressions at compile time, matching Go's
//! semantics for untyped constants.
//!
//! # Go Constant Semantics
//!
//! - Integer constants have arbitrary precision (no overflow at compile time)
//! - Floating-point constants use high-precision rational arithmetic
//! - Constants are "untyped" until assigned to a typed variable
//! - `1 << 100` is valid as a constant, overflow only checked at use

use std::fmt;

use num_bigint::BigInt;
use num_rational::BigRational;
use num_traits::{Zero, ToPrimitive, Signed};

use crate::types::{BasicType, UntypedKind};

/// A compile-time constant value with arbitrary precision.
#[derive(Debug, Clone)]
pub enum Constant {
    /// Boolean constant.
    Bool(bool),

    /// Integer constant (arbitrary precision).
    Int(BigInt),

    /// Floating-point constant (arbitrary precision rational).
    Float(BigRational),

    /// Rune (character) constant.
    Rune(char),

    /// String constant.
    String(String),

    /// Nil constant.
    Nil,
}

/// Helper to create BigInt from i64.
impl Constant {
    /// Create an integer constant from i64.
    pub fn int(v: i64) -> Self {
        Constant::Int(BigInt::from(v))
    }
    
    /// Create a float constant from f64.
    pub fn float(v: f64) -> Self {
        Constant::Float(BigRational::from_float(v).unwrap_or_else(BigRational::zero))
    }
    
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
            Constant::Int(i) => i.is_zero(),
            Constant::Float(f) => f.is_zero(),
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

    /// Attempts to convert this constant to a BigInt.
    pub fn to_bigint(&self) -> Option<BigInt> {
        match self {
            Constant::Int(i) => Some(i.clone()),
            Constant::Rune(c) => Some(BigInt::from(*c as u32)),
            Constant::Float(f) if f.is_integer() => Some(f.to_integer()),
            _ => None,
        }
    }
    
    /// Attempts to convert this constant to i64.
    pub fn to_i64(&self) -> Option<i64> {
        self.to_bigint().and_then(|b| b.to_i64())
    }

    /// Attempts to convert this constant to a float (f64).
    pub fn to_f64(&self) -> Option<f64> {
        match self {
            Constant::Float(f) => f.to_f64(),
            Constant::Int(i) => i.to_f64(),
            Constant::Rune(c) => Some(*c as u32 as f64),
            _ => None,
        }
    }

    /// Checks if this constant is representable in the given basic type.
    pub fn is_representable_as(&self, ty: BasicType) -> bool {
        match self {
            Constant::Bool(_) => ty == BasicType::Bool,
            Constant::String(_) => ty == BasicType::String,
            Constant::Nil => false,
            Constant::Rune(c) => {
                let v = BigInt::from(*c as u32);
                Self::bigint_fits(&v, ty)
            }
            Constant::Int(i) => {
                match ty {
                    BasicType::Float32 | BasicType::Float64 => true,
                    _ => Self::bigint_fits(i, ty),
                }
            }
            Constant::Float(f) => {
                match ty {
                    BasicType::Float32 => {
                        if let Some(v) = f.to_f64() {
                            v >= f32::MIN as f64 && v <= f32::MAX as f64
                        } else {
                            false
                        }
                    }
                    BasicType::Float64 => true,
                    _ if ty.is_integer() && f.is_integer() => {
                        let i = f.to_integer();
                        Self::bigint_fits(&i, ty)
                    }
                    _ => false,
                }
            }
        }
    }

    /// Checks if a BigInt fits in the given basic type.
    fn bigint_fits(v: &BigInt, ty: BasicType) -> bool {
        let min_i8 = BigInt::from(i8::MIN);
        let max_i8 = BigInt::from(i8::MAX);
        let min_i16 = BigInt::from(i16::MIN);
        let max_i16 = BigInt::from(i16::MAX);
        let min_i32 = BigInt::from(i32::MIN);
        let max_i32 = BigInt::from(i32::MAX);
        let min_i64 = BigInt::from(i64::MIN);
        let max_i64 = BigInt::from(i64::MAX);
        let zero = BigInt::zero();
        let max_u8 = BigInt::from(u8::MAX);
        let max_u16 = BigInt::from(u16::MAX);
        let max_u32 = BigInt::from(u32::MAX);
        let max_u64 = BigInt::from(u64::MAX);
        
        match ty {
            BasicType::Int8 => *v >= min_i8 && *v <= max_i8,
            BasicType::Int16 => *v >= min_i16 && *v <= max_i16,
            BasicType::Int32 => *v >= min_i32 && *v <= max_i32,
            BasicType::Int64 | BasicType::Int => *v >= min_i64 && *v <= max_i64,
            BasicType::Uint8 => *v >= zero && *v <= max_u8,
            BasicType::Uint16 => *v >= zero && *v <= max_u16,
            BasicType::Uint32 => *v >= zero && *v <= max_u32,
            BasicType::Uint64 | BasicType::Uint => *v >= zero && *v <= max_u64,
            _ => false,
        }
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constant::Bool(b) => write!(f, "{}", b),
            Constant::Int(i) => write!(f, "{}", i),
            Constant::Float(r) => {
                // Display as decimal if possible
                if let Some(v) = r.to_f64() {
                    write!(f, "{}", v)
                } else {
                    write!(f, "{}/{}", r.numer(), r.denom())
                }
            }
            Constant::Rune(c) => write!(f, "'{}'", c.escape_default()),
            Constant::String(s) => write!(f, "\"{}\"", s.escape_default()),
            Constant::Nil => write!(f, "nil"),
        }
    }
}

impl PartialEq for Constant {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Constant::Bool(a), Constant::Bool(b)) => a == b,
            (Constant::Int(a), Constant::Int(b)) => a == b,
            (Constant::Float(a), Constant::Float(b)) => a == b,
            (Constant::Rune(a), Constant::Rune(b)) => a == b,
            (Constant::String(a), Constant::String(b)) => a == b,
            (Constant::Nil, Constant::Nil) => true,
            // Cross-type comparisons for numeric types
            (Constant::Int(a), Constant::Float(b)) => {
                BigRational::from(a.clone()) == *b
            }
            (Constant::Float(a), Constant::Int(b)) => {
                *a == BigRational::from(b.clone())
            }
            _ => false,
        }
    }
}

/// Binary operations on constants (arbitrary precision).
impl Constant {
    /// Adds two constants.
    pub fn add(&self, other: &Constant) -> Option<Constant> {
        match (self, other) {
            (Constant::Int(a), Constant::Int(b)) => Some(Constant::Int(a + b)),
            (Constant::Float(a), Constant::Float(b)) => Some(Constant::Float(a + b)),
            (Constant::Int(a), Constant::Float(b)) => {
                Some(Constant::Float(BigRational::from(a.clone()) + b))
            }
            (Constant::Float(a), Constant::Int(b)) => {
                Some(Constant::Float(a + BigRational::from(b.clone())))
            }
            (Constant::String(a), Constant::String(b)) => {
                Some(Constant::String(format!("{}{}", a, b)))
            }
            (Constant::Rune(a), Constant::Int(b)) => {
                let v = BigInt::from(*a as u32) + b;
                v.to_u32().and_then(char::from_u32).map(Constant::Rune)
            }
            _ => None,
        }
    }

    /// Subtracts two constants.
    pub fn sub(&self, other: &Constant) -> Option<Constant> {
        match (self, other) {
            (Constant::Int(a), Constant::Int(b)) => Some(Constant::Int(a - b)),
            (Constant::Float(a), Constant::Float(b)) => Some(Constant::Float(a - b)),
            (Constant::Int(a), Constant::Float(b)) => {
                Some(Constant::Float(BigRational::from(a.clone()) - b))
            }
            (Constant::Float(a), Constant::Int(b)) => {
                Some(Constant::Float(a - BigRational::from(b.clone())))
            }
            _ => None,
        }
    }

    /// Multiplies two constants.
    pub fn mul(&self, other: &Constant) -> Option<Constant> {
        match (self, other) {
            (Constant::Int(a), Constant::Int(b)) => Some(Constant::Int(a * b)),
            (Constant::Float(a), Constant::Float(b)) => Some(Constant::Float(a * b)),
            (Constant::Int(a), Constant::Float(b)) => {
                Some(Constant::Float(BigRational::from(a.clone()) * b))
            }
            (Constant::Float(a), Constant::Int(b)) => {
                Some(Constant::Float(a * BigRational::from(b.clone())))
            }
            _ => None,
        }
    }

    /// Divides two constants.
    /// For integers, this performs integer division (like Go).
    pub fn div(&self, other: &Constant) -> Option<Constant> {
        match (self, other) {
            (Constant::Int(a), Constant::Int(b)) if !b.is_zero() => {
                Some(Constant::Int(a / b))
            }
            (Constant::Float(a), Constant::Float(b)) if !b.is_zero() => {
                Some(Constant::Float(a / b))
            }
            (Constant::Int(a), Constant::Float(b)) if !b.is_zero() => {
                Some(Constant::Float(BigRational::from(a.clone()) / b))
            }
            (Constant::Float(a), Constant::Int(b)) if !b.is_zero() => {
                Some(Constant::Float(a / BigRational::from(b.clone())))
            }
            _ => None,
        }
    }

    /// Computes the remainder of two constants.
    pub fn rem(&self, other: &Constant) -> Option<Constant> {
        match (self, other) {
            (Constant::Int(a), Constant::Int(b)) if !b.is_zero() => {
                Some(Constant::Int(a % b))
            }
            _ => None,
        }
    }

    /// Negates a constant.
    pub fn neg(&self) -> Option<Constant> {
        match self {
            Constant::Int(i) => Some(Constant::Int(-i)),
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
            // For arbitrary precision, we use two's complement behavior
            // In Go, ^x for integers means bitwise complement
            Constant::Int(i) => Some(Constant::Int(!i.clone())),
            _ => None,
        }
    }

    /// Left shift (arbitrary precision - no overflow!).
    pub fn shl(&self, other: &Constant) -> Option<Constant> {
        match (self, other) {
            (Constant::Int(a), Constant::Int(b)) => {
                // Shift amount must be non-negative and reasonable
                if b.is_negative() {
                    return None;
                }
                // Allow very large shifts for compile-time constants
                if let Some(shift) = b.to_u64() {
                    if shift <= 10000 {  // Reasonable limit
                        Some(Constant::Int(a << shift as usize))
                    } else {
                        None  // Too large
                    }
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Right shift.
    pub fn shr(&self, other: &Constant) -> Option<Constant> {
        match (self, other) {
            (Constant::Int(a), Constant::Int(b)) => {
                if b.is_negative() {
                    return None;
                }
                if let Some(shift) = b.to_u64() {
                    if shift <= 10000 {
                        Some(Constant::Int(a >> shift as usize))
                    } else {
                        // Shifting by very large amount: result is 0 or -1
                        if a.is_negative() {
                            Some(Constant::Int(BigInt::from(-1)))
                        } else {
                            Some(Constant::Int(BigInt::zero()))
                        }
                    }
                } else {
                    None
                }
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
            (Constant::Int(a), Constant::Int(b)) => Some(Constant::Int(a & !b.clone())),
            _ => None,
        }
    }

    /// Equality comparison.
    pub fn const_eq(&self, other: &Constant) -> Option<Constant> {
        let result = match (self, other) {
            (Constant::Bool(a), Constant::Bool(b)) => a == b,
            (Constant::Int(a), Constant::Int(b)) => a == b,
            (Constant::Float(a), Constant::Float(b)) => a == b,
            (Constant::Int(a), Constant::Float(b)) => BigRational::from(a.clone()) == *b,
            (Constant::Float(a), Constant::Int(b)) => *a == BigRational::from(b.clone()),
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
            (Constant::Int(a), Constant::Float(b)) => BigRational::from(a.clone()) < *b,
            (Constant::Float(a), Constant::Int(b)) => *a < BigRational::from(b.clone()),
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
            (Constant::Int(a), Constant::Float(b)) => BigRational::from(a.clone()) <= *b,
            (Constant::Float(a), Constant::Int(b)) => *a <= BigRational::from(b.clone()),
            (Constant::String(a), Constant::String(b)) => a <= b,
            _ => return None,
        };
        Some(Constant::Bool(result))
    }
    
    /// Greater than comparison.
    pub fn gt(&self, other: &Constant) -> Option<Constant> {
        other.lt(self)
    }
    
    /// Greater than or equal comparison.
    pub fn ge(&self, other: &Constant) -> Option<Constant> {
        other.le(self)
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
        assert_eq!(Constant::int(42).kind(), Some(UntypedKind::Int));
        assert_eq!(Constant::float(3.14).kind(), Some(UntypedKind::Float));
        assert_eq!(Constant::Bool(true).kind(), Some(UntypedKind::Bool));
        assert_eq!(Constant::Rune('a').kind(), Some(UntypedKind::Rune));
        assert_eq!(
            Constant::String("hello".to_string()).kind(),
            Some(UntypedKind::String)
        );
        assert_eq!(Constant::Nil.kind(), None);
    }

    #[test]
    fn test_constant_arithmetic() {
        let a = Constant::int(10);
        let b = Constant::int(3);

        assert_eq!(a.add(&b), Some(Constant::int(13)));
        assert_eq!(a.sub(&b), Some(Constant::int(7)));
        assert_eq!(a.mul(&b), Some(Constant::int(30)));
        assert_eq!(a.div(&b), Some(Constant::int(3)));
        assert_eq!(a.rem(&b), Some(Constant::int(1)));
    }

    #[test]
    fn test_constant_division_by_zero() {
        let a = Constant::int(10);
        let zero = Constant::int(0);

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
        let a = Constant::int(10);
        let b = Constant::int(20);

        assert_eq!(a.lt(&b), Some(Constant::Bool(true)));
        assert_eq!(a.const_eq(&b), Some(Constant::Bool(false)));
        assert_eq!(a.le(&a), Some(Constant::Bool(true)));
    }

    #[test]
    fn test_constant_representability() {
        assert!(Constant::int(127).is_representable_as(BasicType::Int8));
        assert!(!Constant::int(128).is_representable_as(BasicType::Int8));
        assert!(Constant::int(255).is_representable_as(BasicType::Uint8));
        assert!(!Constant::int(256).is_representable_as(BasicType::Uint8));
        assert!(!Constant::int(-1).is_representable_as(BasicType::Uint8));

        assert!(Constant::float(3.14).is_representable_as(BasicType::Float64));
        assert!(Constant::float(3.0).is_representable_as(BasicType::Int)); // whole number
        assert!(!Constant::float(3.14).is_representable_as(BasicType::Int)); // not whole
    }

    #[test]
    fn test_constant_shift() {
        let a = Constant::int(1);
        let shift = Constant::int(10);

        assert_eq!(a.shl(&shift), Some(Constant::int(1024)));
        assert_eq!(Constant::int(1024).shr(&shift), Some(Constant::int(1)));
    }
    
    #[test]
    fn test_arbitrary_precision_shift() {
        // Go allows `1 << 100` as a compile-time constant
        let one = Constant::int(1);
        let shift_100 = Constant::int(100);
        
        let result = one.shl(&shift_100);
        assert!(result.is_some());
        
        // The result should be a very large number
        let big = result.unwrap();
        if let Constant::Int(ref v) = big {
            // 2^100 is a 101-bit number
            assert!(v.bits() > 100);
        } else {
            panic!("expected Int");
        }
    }
    
    #[test]
    fn test_arbitrary_precision_multiply() {
        // Test multiplication that would overflow i64
        let large = Constant::Int(BigInt::from(i64::MAX));
        let two = Constant::int(2);
        
        let result = large.mul(&two);
        assert!(result.is_some());
        
        // Should not overflow, result should be 2 * i64::MAX
        if let Some(Constant::Int(v)) = result {
            assert!(v > BigInt::from(i64::MAX));
        } else {
            panic!("expected Int");
        }
    }
}
