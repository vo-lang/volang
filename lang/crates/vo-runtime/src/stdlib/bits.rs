//! math/bits package native function implementations.
//!
//! Hardware bit manipulation instructions (leading zeros, trailing zeros, popcount).
//! Rotation and byte reversal are implemented in Vo for JIT inlining.

use vo_ffi_macro::vo_extern_std;

// ==================== Leading zeros (CLZ instruction) ====================

#[vo_extern_std("math/bits", "LeadingZeros")]
fn leading_zeros(x: u64) -> i64 {
    x.leading_zeros() as i64
}

#[vo_extern_std("math/bits", "LeadingZeros8")]
fn leading_zeros8(x: u8) -> i64 {
    x.leading_zeros() as i64
}

#[vo_extern_std("math/bits", "LeadingZeros16")]
fn leading_zeros16(x: u16) -> i64 {
    x.leading_zeros() as i64
}

#[vo_extern_std("math/bits", "LeadingZeros32")]
fn leading_zeros32(x: u32) -> i64 {
    x.leading_zeros() as i64
}

#[vo_extern_std("math/bits", "LeadingZeros64")]
fn leading_zeros64(x: u64) -> i64 {
    x.leading_zeros() as i64
}

// ==================== Trailing zeros (CTZ instruction) ====================

#[vo_extern_std("math/bits", "TrailingZeros")]
fn trailing_zeros(x: u64) -> i64 {
    if x == 0 { 64 } else { x.trailing_zeros() as i64 }
}

#[vo_extern_std("math/bits", "TrailingZeros8")]
fn trailing_zeros8(x: u8) -> i64 {
    if x == 0 { 8 } else { x.trailing_zeros() as i64 }
}

#[vo_extern_std("math/bits", "TrailingZeros16")]
fn trailing_zeros16(x: u16) -> i64 {
    if x == 0 { 16 } else { x.trailing_zeros() as i64 }
}

#[vo_extern_std("math/bits", "TrailingZeros32")]
fn trailing_zeros32(x: u32) -> i64 {
    if x == 0 { 32 } else { x.trailing_zeros() as i64 }
}

#[vo_extern_std("math/bits", "TrailingZeros64")]
fn trailing_zeros64(x: u64) -> i64 {
    if x == 0 { 64 } else { x.trailing_zeros() as i64 }
}

// ==================== Population count (POPCNT instruction) ====================

#[vo_extern_std("math/bits", "OnesCount")]
fn ones_count(x: u64) -> i64 {
    x.count_ones() as i64
}

#[vo_extern_std("math/bits", "OnesCount8")]
fn ones_count8(x: u8) -> i64 {
    x.count_ones() as i64
}

#[vo_extern_std("math/bits", "OnesCount16")]
fn ones_count16(x: u16) -> i64 {
    x.count_ones() as i64
}

#[vo_extern_std("math/bits", "OnesCount32")]
fn ones_count32(x: u32) -> i64 {
    x.count_ones() as i64
}

#[vo_extern_std("math/bits", "OnesCount64")]
fn ones_count64(x: u64) -> i64 {
    x.count_ones() as i64
}

// ==================== Add with carry ====================

#[vo_extern_std("math/bits", "Add")]
fn add(x: u64, y: u64, carry: u64) -> (u64, u64) {
    let (sum1, c1) = x.overflowing_add(y);
    let (sum2, c2) = sum1.overflowing_add(carry & 1);
    (sum2, (c1 || c2) as u64)
}

#[vo_extern_std("math/bits", "Add32")]
fn add32(x: u32, y: u32, carry: u32) -> (u32, u32) {
    let carry = carry & 1;
    let (sum1, c1) = x.overflowing_add(y);
    let (sum2, c2) = sum1.overflowing_add(carry);
    (sum2, (c1 || c2) as u32)
}

#[vo_extern_std("math/bits", "Add64")]
fn add64(x: u64, y: u64, carry: u64) -> (u64, u64) {
    let (sum1, c1) = x.overflowing_add(y);
    let (sum2, c2) = sum1.overflowing_add(carry & 1);
    (sum2, (c1 || c2) as u64)
}

// ==================== Subtract with borrow ====================

#[vo_extern_std("math/bits", "Sub")]
fn sub(x: u64, y: u64, borrow: u64) -> (u64, u64) {
    let (diff1, b1) = x.overflowing_sub(y);
    let (diff2, b2) = diff1.overflowing_sub(borrow & 1);
    (diff2, (b1 || b2) as u64)
}

#[vo_extern_std("math/bits", "Sub32")]
fn sub32(x: u32, y: u32, borrow: u32) -> (u32, u32) {
    let borrow = borrow & 1;
    let (diff1, b1) = x.overflowing_sub(y);
    let (diff2, b2) = diff1.overflowing_sub(borrow);
    (diff2, (b1 || b2) as u32)
}

#[vo_extern_std("math/bits", "Sub64")]
fn sub64(x: u64, y: u64, borrow: u64) -> (u64, u64) {
    let (diff1, b1) = x.overflowing_sub(y);
    let (diff2, b2) = diff1.overflowing_sub(borrow & 1);
    (diff2, (b1 || b2) as u64)
}

// ==================== Multiply (full width result) ====================

#[vo_extern_std("math/bits", "Mul")]
fn mul(x: u64, y: u64) -> (u64, u64) {
    let result = (x as u128) * (y as u128);
    ((result >> 64) as u64, result as u64)
}

#[vo_extern_std("math/bits", "Mul32")]
fn mul32(x: u32, y: u32) -> (u32, u32) {
    let result = (x as u64) * (y as u64);
    ((result >> 32) as u32, result as u32)
}

#[vo_extern_std("math/bits", "Mul64")]
fn mul64(x: u64, y: u64) -> (u64, u64) {
    let result = (x as u128) * (y as u128);
    ((result >> 64) as u64, result as u64)
}

// ==================== Divide (double width dividend) ====================

#[vo_extern_std("math/bits", "Div")]
fn div(hi: u64, lo: u64, y: u64) -> (u64, u64) {
    if y == 0 {
        panic!("division by zero");
    }
    let dividend = ((hi as u128) << 64) | (lo as u128);
    let divisor = y as u128;
    ((dividend / divisor) as u64, (dividend % divisor) as u64)
}

#[vo_extern_std("math/bits", "Div32")]
fn div32(hi: u32, lo: u32, y: u32) -> (u32, u32) {
    if y == 0 {
        panic!("division by zero");
    }
    let dividend = ((hi as u64) << 32) | (lo as u64);
    let divisor = y as u64;
    ((dividend / divisor) as u32, (dividend % divisor) as u32)
}

#[vo_extern_std("math/bits", "Div64")]
fn div64(hi: u64, lo: u64, y: u64) -> (u64, u64) {
    if y == 0 {
        panic!("division by zero");
    }
    let dividend = ((hi as u128) << 64) | (lo as u128);
    let divisor = y as u128;
    ((dividend / divisor) as u64, (dividend % divisor) as u64)
}

crate::stdlib_register!(math_bits:
    LeadingZeros, LeadingZeros8, LeadingZeros16, LeadingZeros32, LeadingZeros64,
    TrailingZeros, TrailingZeros8, TrailingZeros16, TrailingZeros32, TrailingZeros64,
    OnesCount, OnesCount8, OnesCount16, OnesCount32, OnesCount64,
    Add, Add32, Add64,
    Sub, Sub32, Sub64,
    Mul, Mul32, Mul64,
    Div, Div32, Div64,
);
