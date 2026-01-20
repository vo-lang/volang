//! Struct tag parsing utilities.
//!
//! Parses Go-style struct tags like `json:"name,omitempty" toml:"other"`.

#[cfg(not(feature = "std"))]
use alloc::string::String;

/// Get the value for a specific key from a struct tag.
///
/// Tag format: `key:"value" key2:"value2"`
///
/// Returns None if the key is not found.
pub fn get_tag_value<'a>(tag: &'a str, key: &str) -> Option<&'a str> {
    let mut remaining = tag;
    
    while !remaining.is_empty() {
        // Skip whitespace
        remaining = remaining.trim_start();
        if remaining.is_empty() {
            break;
        }
        
        // Find the colon
        let colon_pos = match remaining.find(':') {
            Some(pos) => pos,
            None => break,  // No more valid tags
        };
        let current_key = &remaining[..colon_pos];
        remaining = &remaining[colon_pos + 1..];
        
        // Expect opening quote
        if !remaining.starts_with('"') {
            // Malformed tag, try to skip to next space
            if let Some(space_pos) = remaining.find(' ') {
                remaining = &remaining[space_pos..];
            } else {
                break;
            }
            continue;
        }
        remaining = &remaining[1..]; // skip opening quote
        
        // Find closing quote (handle escaped quotes)
        let mut value_end = 0;
        let bytes = remaining.as_bytes();
        while value_end < bytes.len() {
            if bytes[value_end] == b'"' {
                break;
            }
            if bytes[value_end] == b'\\' && value_end + 1 < bytes.len() {
                value_end += 2;
            } else {
                value_end += 1;
            }
        }
        
        let value = &remaining[..value_end];
        
        if current_key == key {
            return Some(value);
        }
        
        // Move past closing quote
        if value_end < remaining.len() {
            remaining = &remaining[value_end + 1..];
        } else {
            break;
        }
    }
    
    None
}

/// Parse field options from a tag value like "name,omitempty".
///
/// Returns (field_name, omitempty).
/// If field_name is empty, the original field name should be used.
/// If field_name is "-", the field should be skipped.
pub fn parse_field_options(value: &str) -> (&str, bool) {
    let mut parts = value.split(',');
    let name = parts.next().unwrap_or("");
    let omitempty = parts.any(|p| p == "omitempty");
    (name, omitempty)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_tag_value() {
        assert_eq!(get_tag_value(r#"json:"name""#, "json"), Some("name"));
        assert_eq!(get_tag_value(r#"json:"name" toml:"other""#, "json"), Some("name"));
        assert_eq!(get_tag_value(r#"json:"name" toml:"other""#, "toml"), Some("other"));
        assert_eq!(get_tag_value(r#"json:"name""#, "toml"), None);
        assert_eq!(get_tag_value(r#"json:"-""#, "json"), Some("-"));
        assert_eq!(get_tag_value(r#"json:"name,omitempty""#, "json"), Some("name,omitempty"));
    }

    #[test]
    fn test_parse_field_options() {
        assert_eq!(parse_field_options("name"), ("name", false));
        assert_eq!(parse_field_options("name,omitempty"), ("name", true));
        assert_eq!(parse_field_options(",omitempty"), ("", true));
        assert_eq!(parse_field_options("-"), ("-", false));
    }
}
