//! String conversion operations (pure logic).

/// Parse string to i64.
pub fn parse_int(s: &str, base: i64) -> Result<i64, String> {
    let base = if base == 0 { 10 } else { base as u32 };
    i64::from_str_radix(s.trim(), base).map_err(|e| e.to_string())
}

/// Parse string to f64.
pub fn parse_float(s: &str) -> Result<f64, String> {
    s.trim().parse::<f64>().map_err(|e| e.to_string())
}

/// Format i64 to string with given base.
pub fn format_int(n: i64, base: i64) -> String {
    match base {
        2 => format!("{:b}", n),
        8 => format!("{:o}", n),
        16 => format!("{:x}", n),
        _ => n.to_string(),
    }
}

/// Format f64 to string.
pub fn format_float(f: f64, fmt: char, prec: i64) -> String {
    if prec < 0 {
        match fmt {
            'e' | 'E' => format!("{:e}", f),
            'f' | 'F' => format!("{}", f),
            'g' | 'G' => format!("{:?}", f),
            _ => format!("{}", f),
        }
    } else {
        let prec = prec as usize;
        match fmt {
            'e' | 'E' => format!("{:.prec$e}", f, prec = prec),
            'f' | 'F' => format!("{:.prec$}", f, prec = prec),
            'g' | 'G' => format!("{:.prec$?}", f, prec = prec),
            _ => format!("{:.prec$}", f, prec = prec),
        }
    }
}

/// Itoa - convert int to string (base 10).
pub fn itoa(n: i64) -> String {
    n.to_string()
}

/// Atoi - convert string to int.
pub fn atoi(s: &str) -> Result<i64, String> {
    parse_int(s, 10)
}

/// Format bool to string.
pub fn format_bool(b: bool) -> String {
    if b { "true".to_string() } else { "false".to_string() }
}

/// Parse string to bool.
pub fn parse_bool(s: &str) -> Result<bool, String> {
    match s.trim() {
        "true" | "True" | "TRUE" | "1" => Ok(true),
        "false" | "False" | "FALSE" | "0" => Ok(false),
        _ => Err(format!("invalid bool: {}", s)),
    }
}

/// Quote a string (add quotes and escape special chars).
pub fn quote(s: &str) -> String {
    format!("{:?}", s)
}

/// Unquote a string.
pub fn unquote(s: &str) -> Result<String, String> {
    if s.len() < 2 {
        return Err("invalid quoted string".to_string());
    }
    let s = s.trim();
    if (s.starts_with('"') && s.ends_with('"')) || (s.starts_with('\'') && s.ends_with('\'')) {
        Ok(s[1..s.len()-1].to_string())
    } else {
        Err("invalid quoted string".to_string())
    }
}
