//! Regular expression operations (pure logic).

use regex::Regex;

/// Compile a regular expression pattern.
pub fn compile(pattern: &str) -> Result<Regex, String> {
    Regex::new(pattern).map_err(|e| e.to_string())
}

/// Check if pattern matches string.
pub fn match_string(pattern: &str, s: &str) -> Result<bool, String> {
    let re = compile(pattern)?;
    Ok(re.is_match(s))
}

/// Find first match in string.
pub fn find_string(re: &Regex, s: &str) -> Option<String> {
    re.find(s).map(|m| m.as_str().to_string())
}

/// Find all matches in string.
pub fn find_all_string(re: &Regex, s: &str, n: i64) -> Vec<String> {
    if n == 0 {
        return Vec::new();
    }
    let iter = re.find_iter(s);
    if n < 0 {
        iter.map(|m| m.as_str().to_string()).collect()
    } else {
        iter.take(n as usize).map(|m| m.as_str().to_string()).collect()
    }
}

/// Find first match indices.
pub fn find_string_index(re: &Regex, s: &str) -> Option<(usize, usize)> {
    re.find(s).map(|m| (m.start(), m.end()))
}

/// Replace first match.
pub fn replace_string(re: &Regex, s: &str, repl: &str) -> String {
    re.replace(s, repl).to_string()
}

/// Replace all matches.
pub fn replace_all_string(re: &Regex, s: &str, repl: &str) -> String {
    re.replace_all(s, repl).to_string()
}

/// Split string by pattern.
pub fn split_string(re: &Regex, s: &str, n: i64) -> Vec<String> {
    if n == 0 {
        return Vec::new();
    }
    if n < 0 {
        re.split(s).map(|p| p.to_string()).collect()
    } else {
        re.splitn(s, n as usize).map(|p| p.to_string()).collect()
    }
}

/// Find submatches (capture groups).
pub fn find_string_submatch(re: &Regex, s: &str) -> Vec<String> {
    re.captures(s)
        .map(|caps| {
            caps.iter()
                .map(|m| m.map(|m| m.as_str().to_string()).unwrap_or_default())
                .collect()
        })
        .unwrap_or_default()
}
