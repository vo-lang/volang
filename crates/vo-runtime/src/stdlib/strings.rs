//! strings package native function implementations.
//!
//! Provides string manipulation functions for the strings standard library package.

use vo_ffi_macro::vo_extern_std;

// ==================== Search ====================

#[vo_extern_std("strings", "Index")]
fn index(s: &str, substr: &str) -> i64 {
    s.find(substr).map(|i| i as i64).unwrap_or(-1)
}

#[vo_extern_std("strings", "LastIndex")]
fn last_index(s: &str, substr: &str) -> i64 {
    s.rfind(substr).map(|i| i as i64).unwrap_or(-1)
}

#[vo_extern_std("strings", "Count")]
fn count(s: &str, substr: &str) -> i64 {
    if substr.is_empty() {
        return (s.chars().count() + 1) as i64;
    }
    s.matches(substr).count() as i64
}

// ==================== Case conversion ====================

#[vo_extern_std("strings", "ToLower")]
fn to_lower(s: &str) -> String {
    s.to_lowercase()
}

#[vo_extern_std("strings", "ToUpper")]
fn to_upper(s: &str) -> String {
    s.to_uppercase()
}

// ==================== Trimming ====================

#[vo_extern_std("strings", "TrimSpace")]
fn trim_space(s: &str) -> String {
    s.trim().to_string()
}

#[vo_extern_std("strings", "Trim")]
fn trim(s: &str, cutset: &str) -> String {
    s.trim_matches(|c| cutset.contains(c)).to_string()
}

#[vo_extern_std("strings", "TrimLeft")]
fn trim_left(s: &str, cutset: &str) -> String {
    s.trim_start_matches(|c| cutset.contains(c)).to_string()
}

#[vo_extern_std("strings", "TrimRight")]
fn trim_right(s: &str, cutset: &str) -> String {
    s.trim_end_matches(|c| cutset.contains(c)).to_string()
}

// ==================== Splitting ====================

#[vo_extern_std("strings", "Split")]
fn split(s: &str, sep: &str) -> Vec<String> {
    if sep.is_empty() {
        s.chars().map(|c| c.to_string()).collect()
    } else {
        s.split(sep).map(|s| s.to_string()).collect()
    }
}

#[vo_extern_std("strings", "SplitN")]
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

// ==================== Replace ====================

#[vo_extern_std("strings", "Replace")]
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

#[vo_extern_std("strings", "EqualFold")]
fn equal_fold(s: &str, t: &str) -> bool {
    s.eq_ignore_ascii_case(t)
}
