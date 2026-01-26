//! strings package native function implementations.
//!
//! Native functions for strings that benefit from Rust's optimized implementations:
//! - Index/LastIndex: Boyer-Moore-like search
//! - ToLower/ToUpper/ToTitle: Unicode case conversion
//! - Split/Fields: Memory allocation optimization
//! - Replace: Complex string manipulation
//! - EqualFold: Unicode case folding

#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use vo_ffi_macro::vostd_extern;

// ==================== Search ====================

#[vostd_extern("strings", "Index")]
fn index(s: &str, substr: &str) -> i64 {
    s.find(substr).map(|i| i as i64).unwrap_or(-1)
}

#[vostd_extern("strings", "LastIndex")]
fn last_index(s: &str, substr: &str) -> i64 {
    s.rfind(substr).map(|i| i as i64).unwrap_or(-1)
}

#[vostd_extern("strings", "Count")]
fn count(s: &str, substr: &str) -> i64 {
    if substr.is_empty() {
        return (s.chars().count() + 1) as i64;
    }
    s.matches(substr).count() as i64
}

// ==================== Case conversion ====================

#[vostd_extern("strings", "ToLower")]
fn to_lower(s: &str) -> String {
    s.to_lowercase()
}

#[vostd_extern("strings", "ToUpper")]
fn to_upper(s: &str) -> String {
    s.to_uppercase()
}

#[vostd_extern("strings", "ToTitle")]
fn to_title(s: &str) -> String {
    // Go's ToTitle converts all letters to title case (not just first letter of each word)
    s.chars().flat_map(|c| c.to_uppercase()).collect()
}

// ==================== Splitting ====================

#[vostd_extern("strings", "Split")]
fn split(s: &str, sep: &str) -> Vec<String> {
    if sep.is_empty() {
        s.chars().map(|c| c.to_string()).collect()
    } else {
        s.split(sep).map(|s| s.to_string()).collect()
    }
}

#[vostd_extern("strings", "SplitN")]
fn split_n(s: &str, sep: &str, n: i64) -> Vec<String> {
    if n == 0 {
        return Vec::new();
    }
    if n < 0 {
        return split(s, sep);
    }
    if sep.is_empty() {
        s.chars().take(n as usize).map(|c| c.to_string()).collect()
    } else {
        s.splitn(n as usize, sep).map(|s| s.to_string()).collect()
    }
}

#[vostd_extern("strings", "SplitAfter")]
fn split_after(s: &str, sep: &str) -> Vec<String> {
    if sep.is_empty() {
        s.chars().map(|c| c.to_string()).collect()
    } else {
        let mut result = Vec::new();
        let mut start = 0;
        while let Some(pos) = s[start..].find(sep) {
            let end = start + pos + sep.len();
            result.push(s[start..end].to_string());
            start = end;
        }
        if start < s.len() {
            result.push(s[start..].to_string());
        }
        result
    }
}

#[vostd_extern("strings", "SplitAfterN")]
fn split_after_n(s: &str, sep: &str, n: i64) -> Vec<String> {
    if n == 0 {
        return Vec::new();
    }
    if n < 0 {
        return split_after(s, sep);
    }
    if sep.is_empty() {
        return s.chars().take(n as usize).map(|c| c.to_string()).collect();
    }
    
    let mut result = Vec::new();
    let mut start = 0;
    let mut count = 0;
    let max = n as usize;
    
    while count < max - 1 {
        if let Some(pos) = s[start..].find(sep) {
            let end = start + pos + sep.len();
            result.push(s[start..end].to_string());
            start = end;
            count += 1;
        } else {
            break;
        }
    }
    if start < s.len() {
        result.push(s[start..].to_string());
    }
    result
}

#[vostd_extern("strings", "Fields")]
fn fields(s: &str) -> Vec<String> {
    s.split_whitespace().map(|s| s.to_string()).collect()
}

// ==================== Replace ====================

#[vostd_extern("strings", "Replace")]
fn replace(s: &str, old: &str, new: &str, n: i64) -> String {
    if n < 0 {
        s.replace(old, new)
    } else if n == 0 {
        s.to_string()
    } else {
        s.replacen(old, new, n as usize)
    }
}

// ==================== Comparison ====================

#[vostd_extern("strings", "EqualFold")]
fn equal_fold(s: &str, t: &str) -> bool {
    // Unicode case-insensitive comparison
    s.to_lowercase() == t.to_lowercase()
}

vo_runtime::stdlib_register!(strings:
    Index, LastIndex, Count, EqualFold,
    ToLower, ToUpper, ToTitle,
    Split, SplitN, SplitAfter, SplitAfterN,
    Fields, Replace,
);
