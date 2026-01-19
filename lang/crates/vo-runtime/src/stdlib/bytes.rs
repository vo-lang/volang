//! bytes package native function implementations.
//!
//! Native functions for bytes that benefit from Rust's optimized implementations:
//! - Index/LastIndex: Fast byte pattern search
//! - ToLower/ToUpper: ASCII case conversion
//! - Split/Fields: Memory allocation optimization
//! - Replace: Complex byte manipulation

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use vo_ffi_macro::vo_extern_std;

// ==================== Search ====================

#[vo_extern_std("bytes", "Index")]
fn index(s: &[u8], sep: &[u8]) -> i64 {
    if sep.is_empty() {
        return 0;
    }
    s.windows(sep.len())
        .position(|w| w == sep)
        .map(|i| i as i64)
        .unwrap_or(-1)
}

#[vo_extern_std("bytes", "LastIndex")]
fn last_index(s: &[u8], sep: &[u8]) -> i64 {
    if sep.is_empty() {
        return s.len() as i64;
    }
    s.windows(sep.len())
        .rposition(|w| w == sep)
        .map(|i| i as i64)
        .unwrap_or(-1)
}

#[vo_extern_std("bytes", "Count")]
fn count(s: &[u8], sep: &[u8]) -> i64 {
    if sep.is_empty() {
        return (s.len() + 1) as i64;
    }
    let mut count = 0i64;
    let mut start = 0;
    while start + sep.len() <= s.len() {
        if &s[start..start + sep.len()] == sep {
            count += 1;
            start += sep.len();
        } else {
            start += 1;
        }
    }
    count
}

// ==================== Case conversion ====================

#[vo_extern_std("bytes", "ToLower")]
fn to_lower(s: &[u8]) -> Vec<u8> {
    s.to_ascii_lowercase()
}

#[vo_extern_std("bytes", "ToUpper")]
fn to_upper(s: &[u8]) -> Vec<u8> {
    s.to_ascii_uppercase()
}

// ==================== Replace ====================

#[vo_extern_std("bytes", "Replace")]
fn replace(s: &[u8], old: &[u8], new: &[u8], n: i64) -> Vec<u8> {
    if old.is_empty() || n == 0 {
        return s.to_vec();
    }
    
    let mut result = Vec::new();
    let mut start = 0;
    let mut count = 0i64;
    
    while start <= s.len() {
        if n >= 0 && count >= n {
            result.extend_from_slice(&s[start..]);
            break;
        }
        if let Some(pos) = s[start..].windows(old.len()).position(|w| w == old) {
            result.extend_from_slice(&s[start..start + pos]);
            result.extend_from_slice(new);
            start = start + pos + old.len();
            count += 1;
        } else {
            result.extend_from_slice(&s[start..]);
            break;
        }
    }
    result
}

crate::stdlib_register!(bytes: Index, LastIndex, Count, ToLower, ToUpper, Replace);
