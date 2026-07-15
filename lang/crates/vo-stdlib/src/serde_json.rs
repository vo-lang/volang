//! JSON format implementation for serde.

#[cfg(not(feature = "std"))]
use alloc::borrow::Cow;
#[cfg(not(feature = "std"))]
use alloc::format;
#[cfg(not(feature = "std"))]
use alloc::string::String;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

#[cfg(feature = "std")]
use std::borrow::Cow;

use super::serde::{FormatReader, FormatWriter, ParsedObject, ParsedValue, MAX_DEPTH};
use crate::raw_utf8;

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
}

impl FormatWriter for JsonWriter {
    fn write_object_start(&mut self) {
        self.buf.push(b'{');
    }

    fn write_object_end(&mut self) {
        self.buf.push(b'}');
    }

    fn write_field_start(&mut self, name: &str, first: bool) -> bool {
        if !first {
            self.buf.push(b',');
        }
        write_json_string_to_buf(name, &mut self.buf, self.escape_html);
        self.buf.push(b':');
        true
    }

    fn write_field_start_bytes(&mut self, name: &[u8], first: bool) -> Result<bool, &'static str> {
        if !first {
            self.buf.push(b',');
        }
        write_json_string_bytes_to_buf(name, &mut self.buf, self.escape_html);
        self.buf.push(b':');
        Ok(true)
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
        if !first {
            self.buf.push(b',');
        }
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

    fn write_uint(&mut self, val: u64) -> Result<(), &'static str> {
        self.buf.extend_from_slice(format!("{}", val).as_bytes());
        Ok(())
    }

    fn write_float(&mut self, val: f64) -> Result<(), &'static str> {
        if val.is_nan() || val.is_infinite() {
            return Err("NaN/Infinity not supported in JSON");
        }
        let mut buffer = ryu::Buffer::new();
        self.buf
            .extend_from_slice(buffer.format_finite(val).as_bytes());
        Ok(())
    }

    fn write_float32(&mut self, val: f32) -> Result<(), &'static str> {
        if val.is_nan() || val.is_infinite() {
            return Err("NaN/Infinity not supported in JSON");
        }
        let mut buffer = ryu::Buffer::new();
        self.buf
            .extend_from_slice(buffer.format_finite(val).as_bytes());
        Ok(())
    }

    fn write_bool(&mut self, val: bool) {
        self.buf
            .extend_from_slice(if val { b"true" } else { b"false" });
    }

    fn write_string(&mut self, val: &str) {
        write_json_string_to_buf(val, &mut self.buf, self.escape_html);
    }

    fn write_string_bytes(&mut self, val: &[u8]) -> Result<(), &'static str> {
        write_json_string_bytes_to_buf(val, &mut self.buf, self.escape_html);
        Ok(())
    }

    fn write_null(&mut self) -> Result<(), &'static str> {
        self.buf.extend_from_slice(b"null");
        Ok(())
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
        write_json_char_to_buf(c, buf, escape_html);
    }
    buf.push(b'"');
}

/// Write an arbitrary-byte VM string using encoding/json's malformed UTF-8
/// rule: each malformed input byte becomes one escaped U+FFFD. A valid U+FFFD
/// sequence remains distinguishable and is emitted as ordinary UTF-8.
pub fn write_json_string_bytes_to_buf(s: &[u8], buf: &mut Vec<u8>, escape_html: bool) {
    buf.push(b'"');
    let mut offset = 0;
    while offset < s.len() {
        let (c, width) = raw_utf8::decode_first(&s[offset..]);
        if c == raw_utf8::REPLACEMENT_CHARACTER && width == 1 {
            buf.extend_from_slice(b"\\ufffd");
        } else {
            write_json_char_to_buf(c, buf, escape_html);
        }
        offset += width;
    }
    buf.push(b'"');
}

fn write_json_char_to_buf(c: char, buf: &mut Vec<u8>, escape_html: bool) {
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
        c => {
            let mut encoded = [0u8; 4];
            buf.extend_from_slice(c.encode_utf8(&mut encoded).as_bytes());
        }
    }
}

// ==================== JSON Reader ====================

pub struct JsonReader;

impl<'a> FormatReader<'a> for JsonReader {
    fn parse(input: &'a str) -> Result<ParsedValue<'a>, &'static str> {
        let bytes = input.as_bytes();
        let mut pos = 0;
        if bytes.is_empty() {
            return Err("empty input");
        }
        let value = parse_value(input, bytes, &mut pos, 0)?;
        skip_ws(bytes, &mut pos);
        if pos != bytes.len() {
            return Err("trailing characters after JSON value");
        }
        Ok(value)
    }

    fn next_field(
        obj: &mut ParsedObject<'a>,
    ) -> Result<Option<(Cow<'a, str>, ParsedValue<'a>)>, &'static str> {
        let bytes = obj.inner.as_bytes();

        skip_ws(bytes, &mut obj.pos);
        if obj.pos >= bytes.len() {
            return Ok(None);
        }

        if bytes[obj.pos] != b'"' {
            return Err("expected string key in JSON object");
        }
        let key = parse_string(obj.inner, bytes, &mut obj.pos)?;

        skip_ws(bytes, &mut obj.pos);
        if obj.pos >= bytes.len() || bytes[obj.pos] != b':' {
            return Err("expected ':' after JSON object key");
        }
        obj.pos += 1;
        let value = parse_value(obj.inner, bytes, &mut obj.pos, obj.depth)?;

        skip_ws(bytes, &mut obj.pos);
        if obj.pos < bytes.len() {
            if bytes[obj.pos] != b',' {
                return Err("expected ',' between JSON object fields");
            }
            obj.pos += 1;
            skip_ws(bytes, &mut obj.pos);
            if obj.pos >= bytes.len() {
                return Err("trailing comma in JSON object");
            }
        }

        Ok(Some((key, value)))
    }

    fn tag_key() -> &'static str {
        "json"
    }
}

fn parse_value<'a>(
    input: &'a str,
    bytes: &[u8],
    pos: &mut usize,
    depth: usize,
) -> Result<ParsedValue<'a>, &'static str> {
    skip_ws(bytes, pos);
    if *pos >= bytes.len() {
        return Err("unexpected end");
    }

    match bytes[*pos] {
        b'{' => parse_object(input, bytes, pos, depth),
        b'"' => parse_string(input, bytes, pos).map(ParsedValue::String),
        b't' => {
            consume_keyword(bytes, pos, b"true", "invalid JSON literal")?;
            Ok(ParsedValue::Bool(true))
        }
        b'f' => {
            consume_keyword(bytes, pos, b"false", "invalid JSON literal")?;
            Ok(ParsedValue::Bool(false))
        }
        b'n' => {
            consume_keyword(bytes, pos, b"null", "invalid JSON literal")?;
            Ok(ParsedValue::Null)
        }
        b'-' | b'0'..=b'9' => parse_number(input, bytes, pos),
        b'[' => parse_array(input, bytes, pos, depth),
        _ => Err("unexpected character"),
    }
}

fn parse_object<'a>(
    input: &'a str,
    bytes: &[u8],
    pos: &mut usize,
    depth: usize,
) -> Result<ParsedValue<'a>, &'static str> {
    let child_depth = enter_container(depth)?;
    *pos += 1;
    skip_ws(bytes, pos);
    let inner_start = *pos;
    if *pos < bytes.len() && bytes[*pos] == b'}' {
        *pos += 1;
        return Ok(ParsedValue::Object(ParsedObject {
            inner: "",
            pos: 0,
            depth: child_depth,
            line_separated: false,
        }));
    }

    loop {
        if *pos >= bytes.len() || bytes[*pos] != b'"' {
            return Err("expected string key in JSON object");
        }
        let _ = parse_string(input, bytes, pos)?;
        skip_ws(bytes, pos);
        if *pos >= bytes.len() || bytes[*pos] != b':' {
            return Err("expected ':' after JSON object key");
        }
        *pos += 1;
        let _ = parse_value(input, bytes, pos, child_depth)?;
        skip_ws(bytes, pos);
        match bytes.get(*pos).copied() {
            Some(b',') => {
                *pos += 1;
                skip_ws(bytes, pos);
                if bytes.get(*pos) == Some(&b'}') {
                    return Err("trailing comma in JSON object");
                }
            }
            Some(b'}') => {
                let inner_end = *pos;
                *pos += 1;
                return Ok(ParsedValue::Object(ParsedObject {
                    inner: &input[inner_start..inner_end],
                    pos: 0,
                    depth: child_depth,
                    line_separated: false,
                }));
            }
            Some(_) => return Err("expected ',' or '}' in JSON object"),
            None => return Err("unterminated JSON object"),
        }
    }
}

fn parse_array<'a>(
    input: &'a str,
    bytes: &[u8],
    pos: &mut usize,
    depth: usize,
) -> Result<ParsedValue<'a>, &'static str> {
    let child_depth = enter_container(depth)?;
    *pos += 1;
    skip_ws(bytes, pos);
    let mut elems = Vec::new();
    if bytes.get(*pos) == Some(&b']') {
        *pos += 1;
        return Ok(ParsedValue::Array(elems));
    }

    loop {
        elems.push(parse_value(input, bytes, pos, child_depth)?);
        skip_ws(bytes, pos);
        match bytes.get(*pos).copied() {
            Some(b',') => {
                *pos += 1;
                skip_ws(bytes, pos);
                if bytes.get(*pos) == Some(&b']') {
                    return Err("trailing comma in JSON array");
                }
            }
            Some(b']') => {
                *pos += 1;
                return Ok(ParsedValue::Array(elems));
            }
            Some(_) => return Err("expected ',' or ']' in JSON array"),
            None => return Err("unterminated JSON array"),
        }
    }
}

fn parse_number<'a>(
    input: &'a str,
    bytes: &[u8],
    pos: &mut usize,
) -> Result<ParsedValue<'a>, &'static str> {
    let start = *pos;
    if bytes[*pos] == b'-' {
        *pos += 1;
        if *pos >= bytes.len() {
            return Err("invalid JSON number");
        }
    }

    match bytes.get(*pos).copied() {
        Some(b'0') => {
            *pos += 1;
            if bytes.get(*pos).is_some_and(u8::is_ascii_digit) {
                return Err("leading zero in JSON number");
            }
        }
        Some(b'1'..=b'9') => consume_digits(bytes, pos),
        _ => return Err("invalid JSON number"),
    }

    let mut is_float = false;
    if bytes.get(*pos) == Some(&b'.') {
        is_float = true;
        *pos += 1;
        if !bytes.get(*pos).is_some_and(u8::is_ascii_digit) {
            return Err("missing fraction digits in JSON number");
        }
        consume_digits(bytes, pos);
    }
    if matches!(bytes.get(*pos), Some(b'e' | b'E')) {
        is_float = true;
        *pos += 1;
        if matches!(bytes.get(*pos), Some(b'+' | b'-')) {
            *pos += 1;
        }
        if !bytes.get(*pos).is_some_and(u8::is_ascii_digit) {
            return Err("missing exponent digits in JSON number");
        }
        consume_digits(bytes, pos);
    }

    let text = &input[start..*pos];
    if is_float {
        let value: f64 = text.parse().map_err(|_| "invalid JSON number")?;
        if !value.is_finite() {
            return Err("JSON number out of range");
        }
        Ok(ParsedValue::Float(value))
    } else {
        match text.parse::<i64>() {
            Ok(value) => Ok(ParsedValue::Int(value)),
            Err(_) if !text.starts_with('-') => text
                .parse::<u64>()
                .map(ParsedValue::Uint)
                .map_err(|_| "JSON integer out of range"),
            Err(_) => Err("JSON integer out of range"),
        }
    }
}

fn parse_string<'a>(
    input: &'a str,
    bytes: &[u8],
    pos: &mut usize,
) -> Result<Cow<'a, str>, &'static str> {
    if bytes.get(*pos) != Some(&b'"') {
        return Err("expected JSON string");
    }
    *pos += 1;
    let start = *pos;
    while let Some(byte) = bytes.get(*pos).copied() {
        match byte {
            b'"' => {
                let content = &input[start..*pos];
                let parsed = parse_string_content(content)?;
                *pos += 1;
                return Ok(parsed);
            }
            b'\\' => {
                *pos += 1;
                if *pos >= bytes.len() {
                    return Err("unterminated escape in JSON string");
                }
                *pos += 1;
            }
            0x00..=0x1f => return Err("unescaped control character in JSON string"),
            _ => *pos += 1,
        }
    }
    Err("unterminated JSON string")
}

pub fn parse_json_string_at(input: &str, start: usize) -> Result<(String, usize), &'static str> {
    let mut pos = start;
    let value = parse_string(input, input.as_bytes(), &mut pos)?;
    Ok((value.into_owned(), pos))
}

fn parse_string_content(s: &str) -> Result<Cow<'_, str>, &'static str> {
    let bytes = s.as_bytes();
    if !bytes.contains(&b'\\') {
        if bytes.iter().any(|byte| *byte < 0x20) {
            return Err("unescaped control character in JSON string");
        }
        return Ok(Cow::Borrowed(s));
    }

    let mut result = String::with_capacity(s.len());
    let mut pos = 0;
    let mut segment_start = 0;
    while pos < bytes.len() {
        if bytes[pos] < 0x20 {
            return Err("unescaped control character in JSON string");
        }
        if bytes[pos] != b'\\' {
            pos += 1;
            continue;
        }

        result.push_str(&s[segment_start..pos]);
        pos += 1;
        let escape = *bytes.get(pos).ok_or("unterminated JSON escape")?;
        pos += 1;
        match escape {
            b'"' => result.push('"'),
            b'\\' => result.push('\\'),
            b'/' => result.push('/'),
            b'b' => result.push('\x08'),
            b'f' => result.push('\x0c'),
            b'n' => result.push('\n'),
            b'r' => result.push('\r'),
            b't' => result.push('\t'),
            b'u' => {
                let first = parse_hex_quad(bytes, &mut pos)?;
                let code = if (0xd800..=0xdbff).contains(&first) {
                    if bytes.get(pos..pos + 2) != Some(b"\\u") {
                        return Err("high surrogate without low surrogate in JSON string");
                    }
                    pos += 2;
                    let second = parse_hex_quad(bytes, &mut pos)?;
                    if !(0xdc00..=0xdfff).contains(&second) {
                        return Err("invalid low surrogate in JSON string");
                    }
                    0x1_0000 + ((first - 0xd800) << 10) + (second - 0xdc00)
                } else if (0xdc00..=0xdfff).contains(&first) {
                    return Err("unpaired low surrogate in JSON string");
                } else {
                    first
                };
                result.push(char::from_u32(code).ok_or("invalid Unicode escape in JSON string")?);
            }
            _ => return Err("invalid escape in JSON string"),
        }
        segment_start = pos;
    }
    result.push_str(&s[segment_start..]);
    Ok(Cow::Owned(result))
}

fn parse_hex_quad(bytes: &[u8], pos: &mut usize) -> Result<u32, &'static str> {
    let digits = bytes
        .get(*pos..(*pos).checked_add(4).ok_or("invalid Unicode escape")?)
        .ok_or("incomplete Unicode escape in JSON string")?;
    let mut value = 0u32;
    for digit in digits {
        value = (value << 4)
            | match digit {
                b'0'..=b'9' => u32::from(*digit - b'0'),
                b'a'..=b'f' => u32::from(*digit - b'a' + 10),
                b'A'..=b'F' => u32::from(*digit - b'A' + 10),
                _ => return Err("invalid Unicode escape in JSON string"),
            };
    }
    *pos += 4;
    Ok(value)
}

fn consume_keyword(
    bytes: &[u8],
    pos: &mut usize,
    keyword: &[u8],
    error: &'static str,
) -> Result<(), &'static str> {
    let end = (*pos).checked_add(keyword.len()).ok_or(error)?;
    if bytes.get(*pos..end) != Some(keyword) {
        return Err(error);
    }
    *pos = end;
    Ok(())
}

fn enter_container(depth: usize) -> Result<usize, &'static str> {
    if depth >= MAX_DEPTH {
        return Err("maximum JSON nesting depth exceeded");
    }
    Ok(depth + 1)
}

fn consume_digits(bytes: &[u8], pos: &mut usize) {
    while bytes.get(*pos).is_some_and(u8::is_ascii_digit) {
        *pos += 1;
    }
}

#[inline]
fn skip_ws(bytes: &[u8], pos: &mut usize) {
    while *pos < bytes.len() {
        let c = bytes[*pos];
        if c == b' ' || c == b'\t' || c == b'\n' || c == b'\r' {
            *pos += 1;
        } else {
            break;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parses(input: &str) -> bool {
        JsonReader::parse(input).is_ok()
    }

    #[test]
    fn root_rejects_trailing_tokens() {
        assert!(!parses("{} true"));
        assert!(!parses("nullx"));
        assert!(!parses("\u{00a0}{\"ok\": true}"));
        assert!(parses(" \n {\"ok\": true} \t"));
    }

    #[test]
    fn object_and_array_separators_are_strict() {
        for invalid in [
            "{\"a\":1 \"b\":2}",
            "{\"a\":1,}",
            "{\"a\":1;\"b\":2}",
            "[1 2]",
            "[1,]",
        ] {
            assert!(!parses(invalid), "accepted {invalid:?}");
        }
    }

    #[test]
    fn strings_validate_escapes_controls_and_surrogates() {
        let ParsedValue::String(value) = JsonReader::parse(r#""\uD83D\uDE03""#).unwrap() else {
            panic!("expected string");
        };
        assert_eq!(value, "😃");

        for invalid in [
            "\"\\\"",
            "\"line\nfeed\"",
            r#""\uD83D""#,
            r#""\uDE03""#,
            r#""\uD83D\u0041""#,
            r#""\x20""#,
        ] {
            assert!(!parses(invalid), "accepted {invalid:?}");
        }
    }

    #[test]
    fn numbers_follow_json_grammar() {
        for valid in ["0", "-0", "19", "-2.5", "6.02e23", "1E-9"] {
            assert!(parses(valid), "rejected {valid:?}");
        }
        for invalid in ["-", "01", "1.", "1e", "1e+", "+1", ".5", "1e400"] {
            assert!(!parses(invalid), "accepted {invalid:?}");
        }
    }

    #[test]
    fn nesting_limit_is_checked_before_recursing() {
        let at_limit = format!("{}0{}", "[".repeat(MAX_DEPTH), "]".repeat(MAX_DEPTH));
        assert!(parses(&at_limit));

        let too_deep = format!(
            "{}0{}",
            "[".repeat(MAX_DEPTH + 1),
            "]".repeat(MAX_DEPTH + 1)
        );
        assert_eq!(
            JsonReader::parse(&too_deep).err(),
            Some("maximum JSON nesting depth exceeded")
        );
    }

    #[test]
    fn object_iterator_never_treats_invalid_content_as_end() {
        let mut object = ParsedObject {
            inner: "?",
            pos: 0,
            depth: 1,
            line_separated: false,
        };
        assert!(JsonReader::next_field(&mut object).is_err());
    }

    #[test]
    fn writer_escapes_object_keys() {
        let mut writer = JsonWriter::new();
        writer.write_object_start();
        writer.write_field_start("a\"b\n", true);
        writer.write_int(1);
        writer.write_object_end();
        assert_eq!(writer.into_bytes(), br#"{"a\"b\n":1}"#);
    }

    #[test]
    fn writer_preserves_the_origin_of_replacement_characters() {
        let mut writer = JsonWriter::new();
        writer.write_string_bytes(&[0xe2, 0x82]).unwrap();
        assert_eq!(writer.into_bytes(), b"\"\\ufffd\\ufffd\"");

        let mut writer = JsonWriter::new();
        writer
            .write_string_bytes(raw_utf8::REPLACEMENT_CHARACTER.to_string().as_bytes())
            .unwrap();
        assert_eq!(writer.into_bytes(), "\"�\"".as_bytes());
    }

    #[test]
    fn writer_applies_raw_byte_semantics_to_object_keys() {
        let mut writer = JsonWriter::new();
        writer.write_object_start();
        writer.write_field_start_bytes(&[0xff], true).unwrap();
        writer.write_int(1);
        writer.write_object_end();
        assert_eq!(writer.into_bytes(), br#"{"\ufffd":1}"#);
    }

    #[test]
    fn writer_formats_float32_at_float32_precision() {
        let mut writer = JsonWriter::new();
        writer.write_float32(0.1).unwrap();
        assert_eq!(writer.into_bytes(), b"0.1");

        let mut writer = JsonWriter::new();
        assert!(writer.write_float32(f32::INFINITY).is_err());
    }
}
