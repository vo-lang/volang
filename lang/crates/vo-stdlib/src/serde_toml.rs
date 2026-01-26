//! TOML format implementation for serde.

#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;
#[cfg(not(feature = "std"))]
use alloc::borrow::Cow;
#[cfg(not(feature = "std"))]
use alloc::format;

#[cfg(feature = "std")]
use std::borrow::Cow;

use super::serde::{FormatWriter, FormatReader, ParsedValue, ParsedObject};

// ==================== TOML Writer ====================

pub struct TomlWriter {
    buf: Vec<u8>,
}

impl TomlWriter {
    pub fn new() -> Self {
        Self {
            buf: Vec::with_capacity(256),
        }
    }
}

impl FormatWriter for TomlWriter {
    fn write_object_start(&mut self) {
        // For struct marshal, we use flat key=value format (no table headers)
    }
    
    fn write_object_end(&mut self) {
        // Nothing needed
    }
    
    fn write_field_start(&mut self, name: &str, first: bool) -> bool {
        if !first { /* fields are on separate lines */ }
        write_toml_key(&mut self.buf, name);
        self.buf.extend_from_slice(b" = ");
        true
    }
    
    fn write_field_end(&mut self) {
        self.buf.push(b'\n');
    }
    
    fn write_array_start(&mut self) {
        self.buf.push(b'[');
    }
    
    fn write_array_end(&mut self) {
        self.buf.push(b']');
    }
    
    fn write_array_elem_start(&mut self, first: bool) {
        if !first { self.buf.extend_from_slice(b", "); }
    }
    
    fn write_array_elem_end(&mut self) {
        // Nothing needed for TOML
    }
    
    fn write_int(&mut self, val: i64) {
        self.buf.extend_from_slice(format!("{}", val).as_bytes());
    }
    
    fn write_int32(&mut self, val: i32) {
        self.buf.extend_from_slice(format!("{}", val).as_bytes());
    }
    
    fn write_float(&mut self, val: f64) -> Result<(), &'static str> {
        if val.is_nan() {
            self.buf.extend_from_slice(b"nan");
        } else if val.is_infinite() {
            if val > 0.0 {
                self.buf.extend_from_slice(b"inf");
            } else {
                self.buf.extend_from_slice(b"-inf");
            }
        } else {
            self.buf.extend_from_slice(format!("{}", val).as_bytes());
        }
        Ok(())
    }
    
    fn write_bool(&mut self, val: bool) {
        self.buf.extend_from_slice(if val { b"true" } else { b"false" });
    }
    
    fn write_string(&mut self, val: &str) {
        write_toml_string(&mut self.buf, val);
    }
    
    fn write_null(&mut self) {
        // TOML doesn't have null, write empty string
        self.buf.extend_from_slice(b"\"\"");
    }
    
    fn tag_key(&self) -> &'static str {
        "toml"
    }
    
    fn into_bytes(self) -> Vec<u8> {
        self.buf
    }
}

fn write_toml_key(buf: &mut Vec<u8>, key: &str) {
    let needs_quote = key.is_empty() || key.chars().any(|c| !is_bare_key_char(c));
    if needs_quote {
        buf.push(b'"');
        write_toml_string_content(buf, key);
        buf.push(b'"');
    } else {
        buf.extend_from_slice(key.as_bytes());
    }
}

fn write_toml_string(buf: &mut Vec<u8>, s: &str) {
    buf.push(b'"');
    write_toml_string_content(buf, s);
    buf.push(b'"');
}

fn write_toml_string_content(buf: &mut Vec<u8>, s: &str) {
    for c in s.chars() {
        match c {
            '"' => buf.extend_from_slice(b"\\\""),
            '\\' => buf.extend_from_slice(b"\\\\"),
            '\n' => buf.extend_from_slice(b"\\n"),
            '\r' => buf.extend_from_slice(b"\\r"),
            '\t' => buf.extend_from_slice(b"\\t"),
            '\x08' => buf.extend_from_slice(b"\\b"),
            '\x0c' => buf.extend_from_slice(b"\\f"),
            c if c < ' ' => buf.extend_from_slice(format!("\\u{:04x}", c as u32).as_bytes()),
            c => { let mut t = [0u8; 4]; buf.extend_from_slice(c.encode_utf8(&mut t).as_bytes()); }
        }
    }
}

fn is_bare_key_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_' || c == '-'
}

// ==================== TOML Reader ====================

pub struct TomlReader;

impl<'a> FormatReader<'a> for TomlReader {
    fn parse(input: &'a str) -> Result<ParsedValue<'a>, &'static str> {
        // For struct unmarshal, we expect an inline table or implicit root table
        let input = input.trim();
        if input.is_empty() {
            return Ok(ParsedValue::Object(ParsedObject { inner: "", pos: 0 }));
        }
        
        // Check if it starts with { (inline table)
        if input.starts_with('{') {
            let bytes = input.as_bytes();
            let mut pos = 1;
            skip_ws_toml(bytes, &mut pos);
            if pos < bytes.len() && bytes[pos] == b'}' {
                return Ok(ParsedValue::Object(ParsedObject { inner: "", pos: 0 }));
            }
            let inner_end = find_inline_table_end(bytes, 1)?;
            return Ok(ParsedValue::Object(ParsedObject {
                inner: &input[1..inner_end],
                pos: 0,
            }));
        }
        
        // Treat as root table (key = value pairs)
        Ok(ParsedValue::Object(ParsedObject { inner: input, pos: 0 }))
    }
    
    fn next_field(obj: &mut ParsedObject<'a>) -> Result<Option<(Cow<'a, str>, ParsedValue<'a>)>, &'static str> {
        let bytes = obj.inner.as_bytes();
        
        skip_ws_and_comments(bytes, &mut obj.pos);
        if obj.pos >= bytes.len() { return Ok(None); }
        
        // Skip table headers for now (we're doing flat struct)
        if bytes[obj.pos] == b'[' {
            skip_line(bytes, &mut obj.pos);
            skip_ws_and_comments(bytes, &mut obj.pos);
            if obj.pos >= bytes.len() { return Ok(None); }
        }
        
        // Parse key
        let key = parse_toml_key(obj.inner, bytes, &mut obj.pos)?;
        
        skip_ws_toml(bytes, &mut obj.pos);
        if obj.pos >= bytes.len() || bytes[obj.pos] != b'=' { return Err("expected '='"); }
        obj.pos += 1;
        skip_ws_toml(bytes, &mut obj.pos);
        
        // Parse value
        let value = parse_toml_value(obj.inner, bytes, &mut obj.pos)?;
        
        // Skip to next line
        skip_ws_toml(bytes, &mut obj.pos);
        if obj.pos < bytes.len() && bytes[obj.pos] == b',' { obj.pos += 1; }
        skip_to_newline(bytes, &mut obj.pos);
        
        Ok(Some((Cow::Owned(key), value)))
    }
    
    fn tag_key() -> &'static str {
        "toml"
    }
}

fn parse_toml_key(input: &str, bytes: &[u8], pos: &mut usize) -> Result<String, &'static str> {
    if *pos >= bytes.len() { return Err("unexpected end"); }
    
    match bytes[*pos] {
        b'"' => parse_toml_basic_string(input, bytes, pos),
        b'\'' => parse_toml_literal_string(input, bytes, pos),
        _ => {
            let start = *pos;
            while *pos < bytes.len() && is_bare_key_char(bytes[*pos] as char) {
                *pos += 1;
            }
            if *pos == start { return Err("empty key"); }
            Ok(input[start..*pos].to_string())
        }
    }
}

fn parse_toml_value<'a>(input: &'a str, bytes: &[u8], pos: &mut usize) -> Result<ParsedValue<'a>, &'static str> {
    skip_ws_toml(bytes, pos);
    if *pos >= bytes.len() { return Err("unexpected end"); }
    
    match bytes[*pos] {
        b'"' => {
            let s = parse_toml_basic_string(input, bytes, pos)?;
            Ok(ParsedValue::String(Cow::Owned(s)))
        }
        b'\'' => {
            let s = parse_toml_literal_string(input, bytes, pos)?;
            Ok(ParsedValue::String(Cow::Owned(s)))
        }
        b'{' => {
            *pos += 1;
            skip_ws_toml(bytes, pos);
            if *pos < bytes.len() && bytes[*pos] == b'}' {
                *pos += 1;
                return Ok(ParsedValue::Object(ParsedObject { inner: "", pos: 0 }));
            }
            let inner_start = *pos;
            let inner_end = find_inline_table_end(bytes, *pos)?;
            *pos = inner_end + 1;
            Ok(ParsedValue::Object(ParsedObject {
                inner: &input[inner_start..inner_end],
                pos: 0,
            }))
        }
        b'[' => {
            // Skip arrays for struct unmarshal
            skip_toml_array(bytes, pos)?;
            Ok(ParsedValue::Null)
        }
        b't' => {
            if *pos + 4 > bytes.len() || &bytes[*pos..*pos+4] != b"true" {
                return Err("invalid true");
            }
            *pos += 4;
            Ok(ParsedValue::Bool(true))
        }
        b'f' => {
            if *pos + 5 > bytes.len() || &bytes[*pos..*pos+5] != b"false" {
                return Err("invalid false");
            }
            *pos += 5;
            Ok(ParsedValue::Bool(false))
        }
        b'+' | b'-' | b'0'..=b'9' => {
            parse_toml_number(input, bytes, pos)
        }
        b'i' => {
            // inf
            if *pos + 3 <= bytes.len() && &bytes[*pos..*pos+3] == b"inf" {
                *pos += 3;
                return Ok(ParsedValue::Float(f64::INFINITY));
            }
            Err("invalid value")
        }
        b'n' => {
            // nan
            if *pos + 3 <= bytes.len() && &bytes[*pos..*pos+3] == b"nan" {
                *pos += 3;
                return Ok(ParsedValue::Float(f64::NAN));
            }
            Err("invalid value")
        }
        _ => Err("unexpected character"),
    }
}

fn parse_toml_basic_string(input: &str, bytes: &[u8], pos: &mut usize) -> Result<String, &'static str> {
    if bytes[*pos] != b'"' { return Err("expected '\"'"); }
    *pos += 1;
    
    // Check for multiline
    if *pos + 1 < bytes.len() && bytes[*pos] == b'"' && bytes[*pos+1] == b'"' {
        *pos += 2;
        return parse_toml_multiline_basic_string(input, bytes, pos);
    }
    
    let mut result = String::new();
    while *pos < bytes.len() {
        let c = bytes[*pos];
        if c == b'"' {
            *pos += 1;
            return Ok(result);
        }
        if c == b'\\' {
            *pos += 1;
            if *pos >= bytes.len() { return Err("unterminated escape"); }
            match bytes[*pos] {
                b'b' => { result.push('\x08'); *pos += 1; }
                b't' => { result.push('\t'); *pos += 1; }
                b'n' => { result.push('\n'); *pos += 1; }
                b'f' => { result.push('\x0c'); *pos += 1; }
                b'r' => { result.push('\r'); *pos += 1; }
                b'"' => { result.push('"'); *pos += 1; }
                b'\\' => { result.push('\\'); *pos += 1; }
                b'u' => {
                    if *pos + 4 >= bytes.len() { return Err("invalid unicode escape"); }
                    let hex = &input[*pos+1..*pos+5];
                    let code = u32::from_str_radix(hex, 16).map_err(|_| "invalid unicode escape")?;
                    result.push(char::from_u32(code).ok_or("invalid code")?);
                    *pos += 5;
                }
                b'U' => {
                    if *pos + 8 >= bytes.len() { return Err("invalid unicode escape"); }
                    let hex = &input[*pos+1..*pos+9];
                    let code = u32::from_str_radix(hex, 16).map_err(|_| "invalid unicode escape")?;
                    result.push(char::from_u32(code).ok_or("invalid code")?);
                    *pos += 9;
                }
                _ => return Err("invalid escape"),
            }
        } else if c == b'\n' || c == b'\r' {
            return Err("newline in basic string");
        } else {
            result.push(c as char);
            *pos += 1;
        }
    }
    Err("unterminated string")
}

fn parse_toml_multiline_basic_string(_input: &str, bytes: &[u8], pos: &mut usize) -> Result<String, &'static str> {
    // Skip first newline if present
    if *pos < bytes.len() && bytes[*pos] == b'\n' { *pos += 1; }
    else if *pos + 1 < bytes.len() && bytes[*pos] == b'\r' && bytes[*pos+1] == b'\n' { *pos += 2; }
    
    let mut result = String::new();
    while *pos < bytes.len() {
        if *pos + 2 < bytes.len() && bytes[*pos] == b'"' && bytes[*pos+1] == b'"' && bytes[*pos+2] == b'"' {
            *pos += 3;
            return Ok(result);
        }
        let c = bytes[*pos];
        if c == b'\\' {
            *pos += 1;
            if *pos >= bytes.len() { return Err("unterminated"); }
            let esc = bytes[*pos];
            if esc == b'\n' || esc == b'\r' || esc == b' ' || esc == b'\t' {
                // Line ending backslash
                while *pos < bytes.len() && (bytes[*pos] == b' ' || bytes[*pos] == b'\t' || bytes[*pos] == b'\n' || bytes[*pos] == b'\r') {
                    *pos += 1;
                }
            } else {
                match esc {
                    b'b' => { result.push('\x08'); *pos += 1; }
                    b't' => { result.push('\t'); *pos += 1; }
                    b'n' => { result.push('\n'); *pos += 1; }
                    b'f' => { result.push('\x0c'); *pos += 1; }
                    b'r' => { result.push('\r'); *pos += 1; }
                    b'"' => { result.push('"'); *pos += 1; }
                    b'\\' => { result.push('\\'); *pos += 1; }
                    _ => return Err("invalid escape"),
                }
            }
        } else {
            result.push(c as char);
            *pos += 1;
        }
    }
    Err("unterminated multiline string")
}

fn parse_toml_literal_string(input: &str, bytes: &[u8], pos: &mut usize) -> Result<String, &'static str> {
    if bytes[*pos] != b'\'' { return Err("expected \"'\""); }
    *pos += 1;
    
    // Check for multiline
    if *pos + 1 < bytes.len() && bytes[*pos] == b'\'' && bytes[*pos+1] == b'\'' {
        *pos += 2;
        // Skip first newline
        if *pos < bytes.len() && bytes[*pos] == b'\n' { *pos += 1; }
        else if *pos + 1 < bytes.len() && bytes[*pos] == b'\r' && bytes[*pos+1] == b'\n' { *pos += 2; }
        
        let start = *pos;
        while *pos < bytes.len() {
            if *pos + 2 < bytes.len() && bytes[*pos] == b'\'' && bytes[*pos+1] == b'\'' && bytes[*pos+2] == b'\'' {
                let s = input[start..*pos].to_string();
                *pos += 3;
                return Ok(s);
            }
            *pos += 1;
        }
        return Err("unterminated multiline literal string");
    }
    
    let start = *pos;
    while *pos < bytes.len() {
        let c = bytes[*pos];
        if c == b'\'' {
            let s = input[start..*pos].to_string();
            *pos += 1;
            return Ok(s);
        }
        if c == b'\n' || c == b'\r' {
            return Err("newline in literal string");
        }
        *pos += 1;
    }
    Err("unterminated literal string")
}

fn parse_toml_number<'a>(input: &'a str, bytes: &[u8], pos: &mut usize) -> Result<ParsedValue<'a>, &'static str> {
    let start = *pos;
    
    // Handle sign
    if bytes[*pos] == b'+' || bytes[*pos] == b'-' { *pos += 1; }
    
    // Check for special values
    if *pos < bytes.len() {
        if bytes[*pos] == b'i' && *pos + 2 < bytes.len() && &bytes[*pos..*pos+3] == b"inf" {
            *pos += 3;
            let val = if input.as_bytes()[start] == b'-' { f64::NEG_INFINITY } else { f64::INFINITY };
            return Ok(ParsedValue::Float(val));
        }
        if bytes[*pos] == b'n' && *pos + 2 < bytes.len() && &bytes[*pos..*pos+3] == b"nan" {
            *pos += 3;
            return Ok(ParsedValue::Float(f64::NAN));
        }
    }
    
    // Collect number chars
    while *pos < bytes.len() {
        let c = bytes[*pos];
        if c.is_ascii_digit() || c == b'.' || c == b'_' || c == b'e' || c == b'E' || c == b'+' || c == b'-' {
            *pos += 1;
        } else {
            break;
        }
    }
    
    let s = &input[start..*pos];
    let cleaned: String = s.chars().filter(|c| *c != '_').collect();
    
    if cleaned.contains('.') || cleaned.contains('e') || cleaned.contains('E') {
        let f: f64 = cleaned.parse().map_err(|_| "invalid float")?;
        Ok(ParsedValue::Float(f))
    } else {
        // Check for hex/octal/binary
        if cleaned.len() > 2 {
            let prefix = &cleaned[..2];
            if prefix == "0x" || prefix == "0X" {
                let i = i64::from_str_radix(&cleaned[2..], 16).map_err(|_| "invalid hex")?;
                return Ok(ParsedValue::Int(i));
            } else if prefix == "0o" || prefix == "0O" {
                let i = i64::from_str_radix(&cleaned[2..], 8).map_err(|_| "invalid octal")?;
                return Ok(ParsedValue::Int(i));
            } else if prefix == "0b" || prefix == "0B" {
                let i = i64::from_str_radix(&cleaned[2..], 2).map_err(|_| "invalid binary")?;
                return Ok(ParsedValue::Int(i));
            }
        }
        let i: i64 = cleaned.parse().map_err(|_| "invalid int")?;
        Ok(ParsedValue::Int(i))
    }
}

fn find_inline_table_end(bytes: &[u8], start: usize) -> Result<usize, &'static str> {
    let mut pos = start;
    let mut depth = 1;
    while pos < bytes.len() && depth > 0 {
        match bytes[pos] {
            b'{' => depth += 1,
            b'}' => {
                depth -= 1;
                if depth == 0 { return Ok(pos); }
            }
            b'"' => {
                pos += 1;
                while pos < bytes.len() && bytes[pos] != b'"' {
                    if bytes[pos] == b'\\' { pos += 1; }
                    pos += 1;
                }
            }
            b'\'' => {
                pos += 1;
                while pos < bytes.len() && bytes[pos] != b'\'' { pos += 1; }
            }
            _ => {}
        }
        pos += 1;
    }
    if depth != 0 { return Err("unterminated inline table"); }
    Ok(start)
}

fn skip_toml_array(bytes: &[u8], pos: &mut usize) -> Result<(), &'static str> {
    if *pos >= bytes.len() || bytes[*pos] != b'[' { return Err("expected array"); }
    *pos += 1;
    let mut depth = 1;
    while *pos < bytes.len() && depth > 0 {
        match bytes[*pos] {
            b'[' => depth += 1,
            b']' => depth -= 1,
            b'"' => {
                *pos += 1;
                while *pos < bytes.len() && bytes[*pos] != b'"' {
                    if bytes[*pos] == b'\\' { *pos += 1; }
                    *pos += 1;
                }
            }
            b'\'' => {
                *pos += 1;
                while *pos < bytes.len() && bytes[*pos] != b'\'' { *pos += 1; }
            }
            _ => {}
        }
        *pos += 1;
    }
    if depth != 0 { return Err("unterminated array"); }
    Ok(())
}

#[inline]
fn skip_ws_toml(bytes: &[u8], pos: &mut usize) {
    while *pos < bytes.len() && (bytes[*pos] == b' ' || bytes[*pos] == b'\t') {
        *pos += 1;
    }
}

fn skip_ws_and_comments(bytes: &[u8], pos: &mut usize) {
    while *pos < bytes.len() {
        let c = bytes[*pos];
        if c == b' ' || c == b'\t' || c == b'\n' || c == b'\r' {
            *pos += 1;
        } else if c == b'#' {
            while *pos < bytes.len() && bytes[*pos] != b'\n' { *pos += 1; }
        } else {
            break;
        }
    }
}

fn skip_line(bytes: &[u8], pos: &mut usize) {
    while *pos < bytes.len() && bytes[*pos] != b'\n' { *pos += 1; }
    if *pos < bytes.len() { *pos += 1; }
}

fn skip_to_newline(bytes: &[u8], pos: &mut usize) {
    while *pos < bytes.len() && bytes[*pos] != b'\n' && bytes[*pos] != b'\r' { *pos += 1; }
    if *pos < bytes.len() && bytes[*pos] == b'\r' { *pos += 1; }
    if *pos < bytes.len() && bytes[*pos] == b'\n' { *pos += 1; }
}
