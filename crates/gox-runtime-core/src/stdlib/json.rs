//! JSON core implementations.
//!
//! Pure logic for JSON validation and string escaping.

/// Simple JSON validation (basic structure check)
pub fn is_valid(s: &str) -> bool {
    let s = s.trim();
    if s.is_empty() {
        return false;
    }
    
    match s.chars().next() {
        Some('{') => s.ends_with('}'),
        Some('[') => s.ends_with(']'),
        Some('"') => s.ends_with('"') && s.len() >= 2,
        Some(c) if c.is_ascii_digit() || c == '-' => {
            s.chars().all(|c| c.is_ascii_digit() || c == '.' || c == '-' || c == 'e' || c == 'E' || c == '+')
        }
        Some('t') => s == "true",
        Some('f') => s == "false",
        Some('n') => s == "null",
        _ => false,
    }
}

/// Escape a string for JSON
pub fn escape_string(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '"' => result.push_str("\\\""),
            '\\' => result.push_str("\\\\"),
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            c if c.is_control() => {
                result.push_str(&format!("\\u{:04x}", c as u32));
            }
            c => result.push(c),
        }
    }
    result
}

/// Unescape a JSON string
pub fn unescape_string(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();
    
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('"') => result.push('"'),
                Some('\\') => result.push('\\'),
                Some('/') => result.push('/'),
                Some('n') => result.push('\n'),
                Some('r') => result.push('\r'),
                Some('t') => result.push('\t'),
                Some('b') => result.push('\x08'),
                Some('f') => result.push('\x0c'),
                Some('u') => {
                    let hex: String = chars.by_ref().take(4).collect();
                    if let Ok(code) = u32::from_str_radix(&hex, 16) {
                        if let Some(ch) = char::from_u32(code) {
                            result.push(ch);
                        }
                    }
                }
                Some(c) => {
                    result.push('\\');
                    result.push(c);
                }
                None => result.push('\\'),
            }
        } else {
            result.push(c);
        }
    }
    result
}

/// Marshal a string to JSON format
pub fn marshal_string(s: &str) -> String {
    format!("\"{}\"", escape_string(s))
}

/// Unmarshal a JSON string, returns (value, error_message)
pub fn unmarshal_string(data: &str) -> (String, Option<&'static str>) {
    let trimmed = data.trim();
    if trimmed.starts_with('"') && trimmed.ends_with('"') && trimmed.len() >= 2 {
        let inner = &trimmed[1..trimmed.len()-1];
        (unescape_string(inner), None)
    } else {
        (String::new(), Some("invalid JSON string"))
    }
}
