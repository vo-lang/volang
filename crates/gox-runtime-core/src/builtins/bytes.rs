//! Byte slice operations (pure logic).

/// Find the index of subslice in slice, or -1 if not found.
pub fn index(s: &[u8], sep: &[u8]) -> i64 {
    if sep.is_empty() {
        return 0;
    }
    s.windows(sep.len())
        .position(|w| w == sep)
        .map(|i| i as i64)
        .unwrap_or(-1)
}

/// Find the last index of subslice in slice, or -1 if not found.
pub fn last_index(s: &[u8], sep: &[u8]) -> i64 {
    if sep.is_empty() {
        return s.len() as i64;
    }
    s.windows(sep.len())
        .rposition(|w| w == sep)
        .map(|i| i as i64)
        .unwrap_or(-1)
}

/// Count non-overlapping occurrences of sep in s.
pub fn count(s: &[u8], sep: &[u8]) -> i64 {
    if sep.is_empty() {
        return (s.len() + 1) as i64;
    }
    let mut count = 0i64;
    let mut i = 0;
    while i + sep.len() <= s.len() {
        if &s[i..i + sep.len()] == sep {
            count += 1;
            i += sep.len();
        } else {
            i += 1;
        }
    }
    count
}

/// Check if slice contains subslice.
pub fn contains(s: &[u8], subslice: &[u8]) -> bool {
    index(s, subslice) >= 0
}

/// Check if slice has prefix.
pub fn has_prefix(s: &[u8], prefix: &[u8]) -> bool {
    s.starts_with(prefix)
}

/// Check if slice has suffix.
pub fn has_suffix(s: &[u8], suffix: &[u8]) -> bool {
    s.ends_with(suffix)
}

/// Compare two byte slices.
pub fn compare(a: &[u8], b: &[u8]) -> i64 {
    match a.cmp(b) {
        std::cmp::Ordering::Less => -1,
        std::cmp::Ordering::Equal => 0,
        std::cmp::Ordering::Greater => 1,
    }
}

/// Check equality.
pub fn equal(a: &[u8], b: &[u8]) -> bool {
    a == b
}

/// Convert to lowercase.
pub fn to_lower(s: &[u8]) -> Vec<u8> {
    s.to_ascii_lowercase()
}

/// Convert to uppercase.
pub fn to_upper(s: &[u8]) -> Vec<u8> {
    s.to_ascii_uppercase()
}

/// Trim leading and trailing whitespace.
pub fn trim_space(s: &[u8]) -> &[u8] {
    let start = s.iter().position(|&b| !b.is_ascii_whitespace()).unwrap_or(s.len());
    let end = s.iter().rposition(|&b| !b.is_ascii_whitespace()).map(|i| i + 1).unwrap_or(0);
    if start >= end {
        &[]
    } else {
        &s[start..end]
    }
}

/// Repeat bytes n times.
pub fn repeat(s: &[u8], n: i64) -> Vec<u8> {
    if n <= 0 {
        Vec::new()
    } else {
        s.repeat(n as usize)
    }
}
