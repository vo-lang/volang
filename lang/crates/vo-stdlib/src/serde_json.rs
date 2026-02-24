//! JSON format implementation for serde.

#[cfg(not(feature = "std"))]
use alloc::string::String;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;
#[cfg(not(feature = "std"))]
use alloc::borrow::Cow;
#[cfg(not(feature = "std"))]
use alloc::format;

#[cfg(feature = "std")]
use std::borrow::Cow;

use super::serde::{FormatWriter, FormatReader, ParsedValue, ParsedObject};

// ==================== JSON Writer ====================

pub struct JsonWriter {
    buf: Vec<u8>,
    escape_html: bool,
}

impl JsonWriter {
    pub fn new() -> Self {
        Self {
            buf: Vec::with_capacity(256),
            escape_html: false,
        }
    }
    
    pub fn with_escape_html(mut self, escape: bool) -> Self {
        self.escape_html = escape;
        self
    }
}

impl FormatWriter for JsonWriter {
    fn write_object_start(&mut self) {
        self.buf.push(b'{');
    }
    
    fn write_object_end(&mut self) {
        self.buf.push(b'}');
    }
    
    fn write_field_start(&mut self, name: &str, first: bool) -> bool {
        if !first { self.buf.push(b','); }
        self.buf.push(b'"');
        self.buf.extend_from_slice(name.as_bytes());
        self.buf.push(b'"');
        self.buf.push(b':');
        true
    }
    
    fn write_field_end(&mut self) {
        // Nothing needed for JSON
    }
    
    fn write_array_start(&mut self) {
        self.buf.push(b'[');
    }
    
    fn write_array_end(&mut self) {
        self.buf.push(b']');
    }
    
    fn write_array_elem_start(&mut self, first: bool) {
        if !first { self.buf.push(b','); }
    }
    
    fn write_array_elem_end(&mut self) {
        // Nothing needed for JSON
    }
    
    fn write_int(&mut self, val: i64) {
        self.buf.extend_from_slice(format!("{}", val).as_bytes());
    }
    
    fn write_int32(&mut self, val: i32) {
        self.buf.extend_from_slice(format!("{}", val).as_bytes());
    }

    fn write_uint(&mut self, val: u64) {
        self.buf.extend_from_slice(format!("{}", val).as_bytes());
    }
    
    fn write_float(&mut self, val: f64) -> Result<(), &'static str> {
        if val.is_nan() || val.is_infinite() {
            return Err("NaN/Infinity not supported in JSON");
        }
        self.buf.extend_from_slice(format!("{}", val).as_bytes());
        Ok(())
    }
    
    fn write_bool(&mut self, val: bool) {
        self.buf.extend_from_slice(if val { b"true" } else { b"false" });
    }
    
    fn write_string(&mut self, val: &str) {
        write_json_string_to_buf(val, &mut self.buf, self.escape_html);
    }
    
    fn write_null(&mut self) {
        self.buf.extend_from_slice(b"null");
    }
    
    fn tag_key(&self) -> &'static str {
        "json"
    }
    
    fn into_bytes(self) -> Vec<u8> {
        self.buf
    }
}

/// Write a JSON-escaped string to buffer (with surrounding quotes).
pub fn write_json_string_to_buf(s: &str, buf: &mut Vec<u8>, escape_html: bool) {
    buf.push(b'"');
    for c in s.chars() {
        match c {
            '"' => buf.extend_from_slice(b"\\\""),
            '\\' => buf.extend_from_slice(b"\\\\"),
            '\n' => buf.extend_from_slice(b"\\n"),
            '\r' => buf.extend_from_slice(b"\\r"),
            '\t' => buf.extend_from_slice(b"\\t"),
            '\x08' => buf.extend_from_slice(b"\\b"),
            '\x0c' => buf.extend_from_slice(b"\\f"),
            '<' if escape_html => buf.extend_from_slice(b"\\u003c"),
            '>' if escape_html => buf.extend_from_slice(b"\\u003e"),
            '&' if escape_html => buf.extend_from_slice(b"\\u0026"),
            c if c < ' ' => buf.extend_from_slice(format!("\\u{:04x}", c as u32).as_bytes()),
            c => { let mut t = [0u8; 4]; buf.extend_from_slice(c.encode_utf8(&mut t).as_bytes()); }
        }
    }
    buf.push(b'"');
}

// ==================== JSON Reader ====================

pub struct JsonReader;

impl<'a> FormatReader<'a> for JsonReader {
    fn parse(input: &'a str) -> Result<ParsedValue<'a>, &'static str> {
        let input = input.trim();
        let bytes = input.as_bytes();
        let mut pos = 0;
        skip_ws(bytes, &mut pos);
        if pos >= bytes.len() {
            return Err("empty input");
        }
        parse_value(input, bytes, &mut pos)
    }
    
    fn next_field(obj: &mut ParsedObject<'a>) -> Result<Option<(Cow<'a, str>, ParsedValue<'a>)>, &'static str> {
        let bytes = obj.inner.as_bytes();
        
        skip_ws(bytes, &mut obj.pos);
        if obj.pos >= bytes.len() { return Ok(None); }
        
        if bytes[obj.pos] != b'"' { return Ok(None); }
        obj.pos += 1;
        let key_start = obj.pos;
        while obj.pos < bytes.len() && bytes[obj.pos] != b'"' {
            if bytes[obj.pos] == b'\\' { obj.pos += 2; } else { obj.pos += 1; }
        }
        if obj.pos >= bytes.len() { return Err("unterminated key"); }
        let key = parse_string_content(&obj.inner[key_start..obj.pos])?;
        obj.pos += 1;
        
        skip_ws(bytes, &mut obj.pos);
        if obj.pos >= bytes.len() || bytes[obj.pos] != b':' { return Err("expected colon"); }
        obj.pos += 1;
        skip_ws(bytes, &mut obj.pos);
        
        let value = parse_value(obj.inner, bytes, &mut obj.pos)?;
        
        skip_ws(bytes, &mut obj.pos);
        if obj.pos < bytes.len() && bytes[obj.pos] == b',' { obj.pos += 1; }
        
        Ok(Some((key, value)))
    }
    
    fn tag_key() -> &'static str {
        "json"
    }
}

fn parse_value<'a>(input: &'a str, bytes: &[u8], pos: &mut usize) -> Result<ParsedValue<'a>, &'static str> {
    skip_ws(bytes, pos);
    if *pos >= bytes.len() { return Err("unexpected end"); }
    
    match bytes[*pos] {
        b'{' => {
            *pos += 1;
            skip_ws(bytes, pos);
            if *pos < bytes.len() && bytes[*pos] == b'}' {
                *pos += 1;
                return Ok(ParsedValue::Object(ParsedObject { inner: "", pos: 0 }));
            }
            let inner_start = *pos;
            let inner_end = find_object_end(bytes, pos)?;
            Ok(ParsedValue::Object(ParsedObject {
                inner: &input[inner_start..inner_end],
                pos: 0,
            }))
        }
        b'"' => {
            *pos += 1;
            let start = *pos;
            while *pos < bytes.len() && bytes[*pos] != b'"' {
                if bytes[*pos] == b'\\' { *pos += 2; } else { *pos += 1; }
            }
            if *pos >= bytes.len() { return Err("unterminated string"); }
            let s = parse_string_content(&input[start..*pos])?;
            *pos += 1;
            Ok(ParsedValue::String(s))
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
        b'n' => {
            if *pos + 4 > bytes.len() || &bytes[*pos..*pos+4] != b"null" {
                return Err("invalid null");
            }
            *pos += 4;
            Ok(ParsedValue::Null)
        }
        b'-' | b'0'..=b'9' => {
            let start = *pos;
            if bytes[*pos] == b'-' { *pos += 1; }
            while *pos < bytes.len() && bytes[*pos] >= b'0' && bytes[*pos] <= b'9' { *pos += 1; }
            let mut is_float = false;
            if *pos < bytes.len() && bytes[*pos] == b'.' {
                is_float = true;
                *pos += 1;
                while *pos < bytes.len() && bytes[*pos] >= b'0' && bytes[*pos] <= b'9' { *pos += 1; }
            }
            if *pos < bytes.len() && (bytes[*pos] == b'e' || bytes[*pos] == b'E') {
                is_float = true;
                *pos += 1;
                if *pos < bytes.len() && (bytes[*pos] == b'+' || bytes[*pos] == b'-') { *pos += 1; }
                while *pos < bytes.len() && bytes[*pos] >= b'0' && bytes[*pos] <= b'9' { *pos += 1; }
            }
            let s = &input[start..*pos];
            if is_float {
                let f: f64 = s.parse().map_err(|_| "invalid float")?;
                Ok(ParsedValue::Float(f))
            } else {
                let i: i64 = s.parse().map_err(|_| "invalid int")?;
                Ok(ParsedValue::Int(i))
            }
        }
        b'[' => {
            *pos += 1;
            skip_ws(bytes, pos);
            let mut elems = Vec::new();
            if *pos < bytes.len() && bytes[*pos] == b']' {
                *pos += 1;
                return Ok(ParsedValue::Array(elems));
            }
            loop {
                skip_ws(bytes, pos);
                if *pos >= bytes.len() { return Err("unterminated array"); }
                let elem = parse_value(input, bytes, pos)?;
                elems.push(elem);
                skip_ws(bytes, pos);
                if *pos >= bytes.len() { return Err("unterminated array"); }
                match bytes[*pos] {
                    b',' => { *pos += 1; }
                    b']' => { *pos += 1; break; }
                    _ => return Err("expected ',' or ']' in array"),
                }
            }
            Ok(ParsedValue::Array(elems))
        }
        _ => Err("unexpected character"),
    }
}

fn find_object_end(bytes: &[u8], pos: &mut usize) -> Result<usize, &'static str> {
    let mut depth = 1;
    let start = *pos;
    while *pos < bytes.len() && depth > 0 {
        match bytes[*pos] {
            b'{' => depth += 1,
            b'}' => {
                depth -= 1;
                if depth == 0 {
                    let end = *pos;
                    *pos += 1;
                    return Ok(end);
                }
            }
            b'"' => {
                *pos += 1;
                while *pos < bytes.len() && bytes[*pos] != b'"' {
                    if bytes[*pos] == b'\\' { *pos += 1; }
                    *pos += 1;
                }
            }
            _ => {}
        }
        *pos += 1;
    }
    if depth != 0 { return Err("unterminated object"); }
    Ok(start)
}


fn parse_string_content(s: &str) -> Result<Cow<'_, str>, &'static str> {
    if !s.contains('\\') {
        return Ok(Cow::Borrowed(s));
    }
    
    let mut result = String::new();
    let mut chars = s.chars().peekable();
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('"') => result.push('"'),
                Some('\\') => result.push('\\'),
                Some('n') => result.push('\n'),
                Some('r') => result.push('\r'),
                Some('t') => result.push('\t'),
                Some('b') => result.push('\x08'),
                Some('f') => result.push('\x0c'),
                Some('/') => result.push('/'),
                Some('u') => {
                    let mut hex = String::new();
                    for _ in 0..4 { hex.push(chars.next().ok_or("invalid escape")?); }
                    let code = u32::from_str_radix(&hex, 16).map_err(|_| "invalid escape")?;
                    result.push(char::from_u32(code).ok_or("invalid code")?);
                }
                _ => return Err("invalid escape"),
            }
        } else {
            result.push(c);
        }
    }
    Ok(Cow::Owned(result))
}

#[inline]
fn skip_ws(bytes: &[u8], pos: &mut usize) {
    while *pos < bytes.len() {
        let c = bytes[*pos];
        if c == b' ' || c == b'\t' || c == b'\n' || c == b'\r' { *pos += 1; }
        else { break; }
    }
}
