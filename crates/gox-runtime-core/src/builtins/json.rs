//! JSON operations (pure logic).

use serde_json::{Value, from_str, to_string, to_string_pretty};

/// Check if string is valid JSON.
pub fn valid(s: &str) -> bool {
    from_str::<Value>(s).is_ok()
}

/// Parse JSON string to serde_json::Value.
pub fn parse(s: &str) -> Result<Value, String> {
    from_str(s).map_err(|e| e.to_string())
}

/// Serialize value to JSON string.
pub fn stringify(v: &Value) -> Result<String, String> {
    to_string(v).map_err(|e| e.to_string())
}

/// Serialize value to pretty JSON string.
pub fn stringify_pretty(v: &Value) -> Result<String, String> {
    to_string_pretty(v).map_err(|e| e.to_string())
}

/// Compact JSON string (remove whitespace).
pub fn compact(s: &str) -> Result<String, String> {
    let v: Value = from_str(s).map_err(|e| e.to_string())?;
    to_string(&v).map_err(|e| e.to_string())
}

/// Indent JSON string.
pub fn indent(s: &str, prefix: &str, indent: &str) -> Result<String, String> {
    let v: Value = from_str(s).map_err(|e| e.to_string())?;
    let pretty = to_string_pretty(&v).map_err(|e| e.to_string())?;
    
    // Apply custom prefix and indent
    let mut result = String::new();
    for (i, line) in pretty.lines().enumerate() {
        if i > 0 {
            result.push('\n');
        }
        result.push_str(prefix);
        // Count leading spaces and replace with indent
        let trimmed = line.trim_start();
        let leading_spaces = line.len() - trimmed.len();
        let indent_count = leading_spaces / 2; // default is 2 spaces
        for _ in 0..indent_count {
            result.push_str(indent);
        }
        result.push_str(trimmed);
    }
    Ok(result)
}
