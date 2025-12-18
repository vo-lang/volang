//! String operations (pure logic).

/// Find the index of substr in s, or -1 if not found.
pub fn index(s: &str, substr: &str) -> i64 {
    s.find(substr).map(|i| i as i64).unwrap_or(-1)
}

/// Find the last index of substr in s, or -1 if not found.
pub fn last_index(s: &str, substr: &str) -> i64 {
    s.rfind(substr).map(|i| i as i64).unwrap_or(-1)
}

/// Count non-overlapping occurrences of substr in s.
pub fn count(s: &str, substr: &str) -> i64 {
    if substr.is_empty() {
        return (s.chars().count() + 1) as i64;
    }
    s.matches(substr).count() as i64
}

/// Convert string to lowercase.
pub fn to_lower(s: &str) -> String {
    s.to_lowercase()
}

/// Convert string to uppercase.
pub fn to_upper(s: &str) -> String {
    s.to_uppercase()
}

/// Trim leading and trailing whitespace.
pub fn trim_space(s: &str) -> &str {
    s.trim()
}

/// Trim leading and trailing occurrences of cutset characters.
pub fn trim<'a>(s: &'a str, cutset: &str) -> &'a str {
    s.trim_matches(|c| cutset.contains(c))
}

/// Trim leading occurrences of cutset characters.
pub fn trim_left<'a>(s: &'a str, cutset: &str) -> &'a str {
    s.trim_start_matches(|c| cutset.contains(c))
}

/// Trim trailing occurrences of cutset characters.
pub fn trim_right<'a>(s: &'a str, cutset: &str) -> &'a str {
    s.trim_end_matches(|c| cutset.contains(c))
}

/// Split string by separator.
pub fn split(s: &str, sep: &str) -> Vec<String> {
    if sep.is_empty() {
        s.chars().map(|c| c.to_string()).collect()
    } else {
        s.split(sep).map(|p| p.to_string()).collect()
    }
}

/// Split string by separator with limit.
pub fn split_n(s: &str, sep: &str, n: i64) -> Vec<String> {
    if n == 0 {
        return Vec::new();
    }
    if n < 0 {
        return split(s, sep);
    }
    if sep.is_empty() {
        s.chars().take(n as usize).map(|c| c.to_string()).collect()
    } else {
        s.splitn(n as usize, sep).map(|p| p.to_string()).collect()
    }
}

/// Join strings with separator.
pub fn join(parts: &[&str], sep: &str) -> String {
    parts.join(sep)
}

/// Replace first n occurrences of old with new. If n < 0, replace all.
pub fn replace(s: &str, old: &str, new: &str, n: i64) -> String {
    if n < 0 {
        s.replace(old, new)
    } else if n == 0 {
        s.to_string()
    } else {
        s.replacen(old, new, n as usize)
    }
}

/// Check if string contains substr.
pub fn contains(s: &str, substr: &str) -> bool {
    s.contains(substr)
}

/// Check if string has prefix.
pub fn has_prefix(s: &str, prefix: &str) -> bool {
    s.starts_with(prefix)
}

/// Check if string has suffix.
pub fn has_suffix(s: &str, suffix: &str) -> bool {
    s.ends_with(suffix)
}

/// Repeat string n times.
pub fn repeat(s: &str, n: i64) -> String {
    if n <= 0 {
        String::new()
    } else {
        s.repeat(n as usize)
    }
}

/// Compare two strings lexicographically.
/// Returns -1 if a < b, 0 if a == b, 1 if a > b.
pub fn compare(a: &str, b: &str) -> i64 {
    match a.cmp(b) {
        std::cmp::Ordering::Less => -1,
        std::cmp::Ordering::Equal => 0,
        std::cmp::Ordering::Greater => 1,
    }
}

/// Check if two strings are equal ignoring case.
pub fn equal_fold(a: &str, b: &str) -> bool {
    a.eq_ignore_ascii_case(b)
}
