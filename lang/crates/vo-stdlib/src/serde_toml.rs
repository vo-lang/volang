//! Compact TOML value-tree transport for native struct serialization.
//!
//! Public `encoding/toml.Unmarshal` first uses the package's document parser,
//! then emits the parsed tree as root key/value pairs with nested inline
//! values. This reader deliberately accepts that canonical transport grammar;
//! document-level table headers, dotted keys, multiline strings, and future
//! scalar forms stay centralized in the public parser.

#[cfg(not(feature = "std"))]
use alloc::borrow::Cow;
#[cfg(not(feature = "std"))]
use alloc::collections::BTreeSet;
#[cfg(not(feature = "std"))]
use alloc::format;
#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

#[cfg(feature = "std")]
use std::borrow::Cow;
#[cfg(feature = "std")]
use std::collections::BTreeSet;

use super::serde::{FormatReader, FormatWriter, ParsedObject, ParsedValue, MAX_DEPTH};

const OFFSET_DATE_TIME_TYPE: &str = "encoding/toml.OffsetDateTime";
const LOCAL_DATE_TIME_TYPE: &str = "encoding/toml.LocalDateTime";
const LOCAL_DATE_TYPE: &str = "encoding/toml.LocalDate";
const LOCAL_TIME_TYPE: &str = "encoding/toml.LocalTime";

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum TomlDateTimeKind {
    OffsetDateTime,
    LocalDateTime,
    LocalDate,
    LocalTime,
}

impl TomlDateTimeKind {
    fn type_name(self) -> &'static str {
        match self {
            Self::OffsetDateTime => OFFSET_DATE_TIME_TYPE,
            Self::LocalDateTime => LOCAL_DATE_TIME_TYPE,
            Self::LocalDate => LOCAL_DATE_TYPE,
            Self::LocalTime => LOCAL_TIME_TYPE,
        }
    }
}

fn datetime_kind_for_type_name(type_name: &str) -> Option<TomlDateTimeKind> {
    match type_name {
        OFFSET_DATE_TIME_TYPE => Some(TomlDateTimeKind::OffsetDateTime),
        LOCAL_DATE_TIME_TYPE => Some(TomlDateTimeKind::LocalDateTime),
        LOCAL_DATE_TYPE => Some(TomlDateTimeKind::LocalDate),
        LOCAL_TIME_TYPE => Some(TomlDateTimeKind::LocalTime),
        _ => None,
    }
}

// ==================== TOML Writer ====================

pub struct TomlWriter {
    buf: Vec<u8>,
    object_depth: usize,
}

impl TomlWriter {
    pub fn new() -> Self {
        Self {
            buf: Vec::with_capacity(256),
            object_depth: 0,
        }
    }
}

impl FormatWriter for TomlWriter {
    fn write_object_start(&mut self) {
        if self.object_depth > 0 {
            self.buf.push(b'{');
        }
        self.object_depth += 1;
    }

    fn write_object_end(&mut self) {
        self.object_depth = self
            .object_depth
            .checked_sub(1)
            .expect("unbalanced TOML object writer");
        if self.object_depth > 0 {
            self.buf.push(b'}');
        }
    }

    fn write_field_start(&mut self, name: &str, first: bool) -> bool {
        if self.object_depth > 1 && !first {
            self.buf.extend_from_slice(b", ");
        }
        write_toml_key(&mut self.buf, name);
        self.buf.extend_from_slice(b" = ");
        true
    }

    fn write_field_start_bytes(&mut self, name: &[u8], first: bool) -> Result<bool, &'static str> {
        let name = core::str::from_utf8(name)
            .map_err(|_| "TOML strings and keys must contain valid UTF-8")?;
        Ok(self.write_field_start(name, first))
    }

    fn write_field_end(&mut self) {
        if self.object_depth == 1 {
            self.buf.push(b'\n');
        }
    }

    fn write_array_start(&mut self) {
        self.buf.push(b'[');
    }

    fn write_array_end(&mut self) {
        self.buf.push(b']');
    }

    fn write_array_elem_start(&mut self, first: bool) {
        if !first {
            self.buf.extend_from_slice(b", ");
        }
    }

    fn write_array_elem_end(&mut self) {}

    fn write_int(&mut self, val: i64) {
        self.buf.extend_from_slice(format!("{}", val).as_bytes());
    }

    fn write_int32(&mut self, val: i32) {
        self.buf.extend_from_slice(format!("{}", val).as_bytes());
    }

    fn write_uint(&mut self, val: u64) -> Result<(), &'static str> {
        if val > i64::MAX as u64 {
            return Err("unsigned integer exceeds TOML's signed 64-bit range");
        }
        self.buf.extend_from_slice(format!("{}", val).as_bytes());
        Ok(())
    }

    fn write_float(&mut self, val: f64) -> Result<(), &'static str> {
        if val.is_nan() {
            self.buf.extend_from_slice(b"nan");
        } else if val.is_infinite() {
            self.buf.extend_from_slice(if val.is_sign_positive() {
                b"inf"
            } else {
                b"-inf"
            });
        } else {
            let mut buffer = ryu::Buffer::new();
            self.buf
                .extend_from_slice(buffer.format_finite(val).as_bytes());
        }
        Ok(())
    }

    fn write_float32(&mut self, val: f32) -> Result<(), &'static str> {
        if val.is_nan() {
            self.buf.extend_from_slice(b"nan");
        } else if val.is_infinite() {
            self.buf.extend_from_slice(if val.is_sign_positive() {
                b"inf"
            } else {
                b"-inf"
            });
        } else {
            let mut buffer = ryu::Buffer::new();
            self.buf
                .extend_from_slice(buffer.format_finite(val).as_bytes());
        }
        Ok(())
    }

    fn write_bool(&mut self, val: bool) {
        self.buf
            .extend_from_slice(if val { b"true" } else { b"false" });
    }

    fn write_string(&mut self, val: &str) {
        write_toml_string(&mut self.buf, val);
    }

    fn write_string_bytes(&mut self, val: &[u8]) -> Result<(), &'static str> {
        let val = core::str::from_utf8(val)
            .map_err(|_| "TOML strings and keys must contain valid UTF-8")?;
        self.write_string(val);
        Ok(())
    }

    fn write_typed_string_bytes(
        &mut self,
        val: &[u8],
        type_name: Option<&str>,
    ) -> Result<(), &'static str> {
        let Some(expected_kind) = type_name.and_then(datetime_kind_for_type_name) else {
            return self.write_string_bytes(val);
        };
        let raw = core::str::from_utf8(val)
            .map_err(|_| "TOML date/time values must contain valid UTF-8")?;
        let (actual_kind, end) = parse_toml_datetime_token(val, 0)?;
        if end != val.len() || actual_kind != expected_kind {
            return Err("invalid TOML date/time value or category");
        }
        self.buf.extend_from_slice(raw.as_bytes());
        Ok(())
    }

    fn write_null(&mut self) -> Result<(), &'static str> {
        Err("TOML does not support null values")
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
            c if c < ' ' || c == '\x7f' => {
                buf.extend_from_slice(format!("\\u{:04x}", c as u32).as_bytes());
            }
            c => {
                let mut encoded = [0u8; 4];
                buf.extend_from_slice(c.encode_utf8(&mut encoded).as_bytes());
            }
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
        let bytes = input.as_bytes();
        let mut first = 0;
        skip_document_layout(bytes, &mut first)?;

        if bytes.get(first) == Some(&b'{') {
            return Err("standalone TOML inline table is not a document");
        }

        let depth = enter_container(0)?;
        validate_root_table(input, bytes, depth)?;
        Ok(ParsedValue::Object(ParsedObject {
            inner: input,
            pos: 0,
            depth,
            line_separated: true,
        }))
    }

    fn next_field(
        obj: &mut ParsedObject<'a>,
    ) -> Result<Option<(Cow<'a, str>, ParsedValue<'a>)>, &'static str> {
        let bytes = obj.inner.as_bytes();
        if obj.line_separated {
            skip_document_layout(bytes, &mut obj.pos)?;
        } else {
            skip_inline_layout(bytes, &mut obj.pos)?;
        }
        if obj.pos >= bytes.len() {
            return Ok(None);
        }
        if bytes[obj.pos] == b'[' {
            return Err("TOML table headers are not supported by struct Unmarshal");
        }

        let key = parse_toml_key(obj.inner, bytes, &mut obj.pos)?;
        skip_horizontal_ws(bytes, &mut obj.pos);
        if bytes.get(obj.pos) != Some(&b'=') {
            return Err("expected '=' after TOML key");
        }
        obj.pos += 1;
        let value = parse_toml_value(obj.inner, bytes, &mut obj.pos, obj.depth)?;

        if obj.line_separated {
            finish_root_line(bytes, &mut obj.pos)?;
        } else {
            skip_inline_layout(bytes, &mut obj.pos)?;
            if obj.pos < bytes.len() {
                if bytes[obj.pos] != b',' {
                    return Err("expected ',' between TOML inline-table fields");
                }
                obj.pos += 1;
                skip_inline_layout(bytes, &mut obj.pos)?;
            }
        }

        Ok(Some((Cow::Owned(key), value)))
    }

    fn tag_key() -> &'static str {
        "toml"
    }

    fn validate_string_target(
        parsed_type_name: Option<&str>,
        target_type_name: Option<&str>,
    ) -> Result<(), &'static str> {
        let parsed_kind = parsed_type_name.and_then(datetime_kind_for_type_name);
        let target_kind = target_type_name.and_then(datetime_kind_for_type_name);
        match (parsed_kind, target_kind) {
            (None, None) => Ok(()),
            (Some(parsed), Some(target)) if parsed == target => Ok(()),
            (None, Some(_)) => Err("quoted TOML string cannot decode into a date/time type"),
            (Some(_), None) => {
                Err("TOML date/time value requires its matching named destination type")
            }
            (Some(_), Some(_)) => Err("TOML date/time category does not match destination type"),
        }
    }
}

fn validate_root_table(input: &str, bytes: &[u8], depth: usize) -> Result<(), &'static str> {
    let mut pos = 0;
    let mut keys = BTreeSet::new();
    loop {
        skip_document_layout(bytes, &mut pos)?;
        if pos >= bytes.len() {
            return Ok(());
        }
        if bytes[pos] == b'[' {
            return Err("TOML table headers are not supported by struct Unmarshal");
        }

        let key = parse_toml_key(input, bytes, &mut pos)?;
        if !keys.insert(key) {
            return Err("duplicate TOML key");
        }
        skip_horizontal_ws(bytes, &mut pos);
        if bytes.get(pos) != Some(&b'=') {
            return Err("expected '=' after TOML key");
        }
        pos += 1;
        let _ = parse_toml_value(input, bytes, &mut pos, depth)?;
        finish_root_line(bytes, &mut pos)?;
    }
}

fn parse_toml_key(input: &str, bytes: &[u8], pos: &mut usize) -> Result<String, &'static str> {
    match bytes.get(*pos).copied() {
        Some(b'"') => parse_toml_basic_string(input, bytes, pos),
        Some(b'\'') => parse_toml_literal_string(input, bytes, pos),
        Some(_) => {
            let start = *pos;
            while bytes
                .get(*pos)
                .is_some_and(|byte| is_bare_key_char(char::from(*byte)))
            {
                *pos += 1;
            }
            if *pos == start {
                return Err("invalid or empty TOML key");
            }
            Ok(input[start..*pos].to_string())
        }
        None => Err("unexpected end while reading TOML key"),
    }
}

fn parse_toml_value<'a>(
    input: &'a str,
    bytes: &[u8],
    pos: &mut usize,
    depth: usize,
) -> Result<ParsedValue<'a>, &'static str> {
    skip_horizontal_ws(bytes, pos);
    match bytes.get(*pos).copied() {
        Some(b'"') => parse_toml_basic_string(input, bytes, pos)
            .map(Cow::Owned)
            .map(ParsedValue::String),
        Some(b'\'') => parse_toml_literal_string(input, bytes, pos)
            .map(Cow::Owned)
            .map(ParsedValue::String),
        Some(b'{') => parse_inline_table(input, bytes, pos, depth),
        Some(b'[') => parse_toml_array(input, bytes, pos, depth),
        Some(b't') => {
            consume_keyword(bytes, pos, b"true", "invalid TOML boolean")?;
            Ok(ParsedValue::Bool(true))
        }
        Some(b'f') => {
            consume_keyword(bytes, pos, b"false", "invalid TOML boolean")?;
            Ok(ParsedValue::Bool(false))
        }
        Some(b'+' | b'-' | b'0'..=b'9' | b'i' | b'n') => parse_toml_number(input, bytes, pos),
        Some(_) => Err("unsupported or invalid TOML value"),
        None => Err("unexpected end while reading TOML value"),
    }
}

fn parse_inline_table<'a>(
    input: &'a str,
    bytes: &[u8],
    pos: &mut usize,
    depth: usize,
) -> Result<ParsedValue<'a>, &'static str> {
    let child_depth = enter_container(depth)?;
    *pos += 1;
    skip_inline_layout(bytes, pos)?;
    let inner_start = *pos;
    if bytes.get(*pos) == Some(&b'}') {
        *pos += 1;
        return Ok(ParsedValue::Object(ParsedObject {
            inner: "",
            pos: 0,
            depth: child_depth,
            line_separated: false,
        }));
    }

    let mut keys = BTreeSet::new();
    loop {
        let key = parse_toml_key(input, bytes, pos)?;
        if !keys.insert(key) {
            return Err("duplicate TOML key in inline table");
        }
        skip_horizontal_ws(bytes, pos);
        if bytes.get(*pos) != Some(&b'=') {
            return Err("expected '=' after TOML inline-table key");
        }
        *pos += 1;
        let _ = parse_toml_value(input, bytes, pos, child_depth)?;
        skip_inline_layout(bytes, pos)?;
        match bytes.get(*pos).copied() {
            Some(b',') => {
                *pos += 1;
                skip_inline_layout(bytes, pos)?;
                if bytes.get(*pos) == Some(&b'}') {
                    let inner_end = *pos;
                    *pos += 1;
                    return Ok(ParsedValue::Object(ParsedObject {
                        inner: &input[inner_start..inner_end],
                        pos: 0,
                        depth: child_depth,
                        line_separated: false,
                    }));
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
            Some(_) => return Err("expected ',' or '}' in TOML inline table"),
            None => return Err("unterminated TOML inline table"),
        }
    }
}

fn parse_toml_array<'a>(
    input: &'a str,
    bytes: &[u8],
    pos: &mut usize,
    depth: usize,
) -> Result<ParsedValue<'a>, &'static str> {
    let child_depth = enter_container(depth)?;
    *pos += 1;
    skip_array_layout(bytes, pos)?;
    let mut elems = Vec::new();
    if bytes.get(*pos) == Some(&b']') {
        *pos += 1;
        return Ok(ParsedValue::Array(elems));
    }

    loop {
        let elem = parse_toml_value(input, bytes, pos, child_depth)?;
        elems.push(elem);
        skip_array_layout(bytes, pos)?;
        match bytes.get(*pos).copied() {
            Some(b',') => {
                *pos += 1;
                skip_array_layout(bytes, pos)?;
                if bytes.get(*pos) == Some(&b']') {
                    *pos += 1;
                    return Ok(ParsedValue::Array(elems));
                }
            }
            Some(b']') => {
                *pos += 1;
                return Ok(ParsedValue::Array(elems));
            }
            Some(_) => return Err("expected ',' or ']' in TOML array"),
            None => return Err("unterminated TOML array"),
        }
    }
}

fn parse_toml_basic_string(
    input: &str,
    bytes: &[u8],
    pos: &mut usize,
) -> Result<String, &'static str> {
    if bytes.get(*pos) != Some(&b'"') {
        return Err("expected TOML basic string");
    }
    *pos += 1;
    if bytes.get(*pos..(*pos).saturating_add(2)) == Some(b"\"\"") {
        return Err("multiline TOML strings are not supported by struct Unmarshal");
    }

    let mut result = String::new();
    loop {
        let byte = bytes
            .get(*pos)
            .copied()
            .ok_or("unterminated TOML basic string")?;
        if byte == b'"' {
            *pos += 1;
            return Ok(result);
        }
        if byte == b'\\' {
            *pos += 1;
            parse_toml_escape(bytes, pos, &mut result)?;
            continue;
        }
        if byte == b'\r' || byte == b'\n' {
            return Err("newline in TOML basic string");
        }
        push_toml_string_char(input, bytes, pos, &mut result)?;
    }
}

fn parse_toml_escape(
    bytes: &[u8],
    pos: &mut usize,
    result: &mut String,
) -> Result<(), &'static str> {
    let escape = bytes.get(*pos).copied().ok_or("unterminated TOML escape")?;
    *pos += 1;
    match escape {
        b'b' => result.push('\x08'),
        b't' => result.push('\t'),
        b'n' => result.push('\n'),
        b'f' => result.push('\x0c'),
        b'r' => result.push('\r'),
        b'e' => result.push('\x1b'),
        b'"' => result.push('"'),
        b'\\' => result.push('\\'),
        b'x' => result.push(parse_toml_unicode_escape(bytes, pos, 2)?),
        b'u' => result.push(parse_toml_unicode_escape(bytes, pos, 4)?),
        b'U' => result.push(parse_toml_unicode_escape(bytes, pos, 8)?),
        _ => return Err("invalid escape in TOML basic string"),
    }
    Ok(())
}

fn parse_toml_unicode_escape(
    bytes: &[u8],
    pos: &mut usize,
    digits: usize,
) -> Result<char, &'static str> {
    let end = (*pos)
        .checked_add(digits)
        .ok_or("TOML Unicode escape overflow")?;
    let hex = bytes
        .get(*pos..end)
        .ok_or("incomplete TOML Unicode escape")?;
    let mut value = 0u32;
    for digit in hex {
        value = (value << 4)
            | match digit {
                b'0'..=b'9' => u32::from(*digit - b'0'),
                b'a'..=b'f' => u32::from(*digit - b'a' + 10),
                b'A'..=b'F' => u32::from(*digit - b'A' + 10),
                _ => return Err("invalid TOML Unicode escape"),
            };
    }
    *pos = end;
    char::from_u32(value).ok_or("invalid Unicode scalar in TOML string")
}

fn push_toml_string_char(
    input: &str,
    bytes: &[u8],
    pos: &mut usize,
    result: &mut String,
) -> Result<(), &'static str> {
    let byte = bytes[*pos];
    if (byte < 0x20 && byte != b'\t') || byte == 0x7f {
        return Err("unescaped control character in TOML string");
    }
    let ch = input[*pos..]
        .chars()
        .next()
        .ok_or("invalid UTF-8 cursor in TOML string")?;
    result.push(ch);
    *pos += ch.len_utf8();
    Ok(())
}

fn parse_toml_literal_string(
    input: &str,
    bytes: &[u8],
    pos: &mut usize,
) -> Result<String, &'static str> {
    if bytes.get(*pos) != Some(&b'\'') {
        return Err("expected TOML literal string");
    }
    *pos += 1;
    if bytes.get(*pos..(*pos).saturating_add(2)) == Some(b"''") {
        return Err("multiline TOML strings are not supported by struct Unmarshal");
    }

    let mut result = String::new();
    loop {
        let byte = bytes
            .get(*pos)
            .copied()
            .ok_or("unterminated TOML literal string")?;
        if byte == b'\'' {
            *pos += 1;
            return Ok(result);
        }
        if byte == b'\r' || byte == b'\n' {
            return Err("newline in TOML literal string");
        }
        push_toml_string_char(input, bytes, pos, &mut result)?;
    }
}

fn parse_toml_number<'a>(
    input: &'a str,
    bytes: &[u8],
    pos: &mut usize,
) -> Result<ParsedValue<'a>, &'static str> {
    if looks_like_toml_datetime(bytes, *pos) {
        let start = *pos;
        let (kind, end) = parse_toml_datetime_token(bytes, start)?;
        if bytes
            .get(end)
            .is_some_and(|byte| !is_toml_value_boundary(*byte))
        {
            return Err("invalid character after TOML date/time value");
        }
        *pos = end;
        return Ok(ParsedValue::NamedString {
            value: Cow::Borrowed(&input[start..end]),
            type_name: kind.type_name(),
        });
    }
    let start = *pos;
    let has_sign = matches!(bytes.get(*pos), Some(b'+' | b'-'));
    if has_sign {
        *pos += 1;
    }

    if bytes.get(*pos..(*pos).saturating_add(3)) == Some(b"inf") {
        *pos += 3;
        return Ok(ParsedValue::Float(if bytes[start] == b'-' {
            f64::NEG_INFINITY
        } else {
            f64::INFINITY
        }));
    }
    if bytes.get(*pos..(*pos).saturating_add(3)) == Some(b"nan") {
        *pos += 3;
        return Ok(ParsedValue::Float(f64::NAN));
    }

    if !has_sign && bytes.get(*pos) == Some(&b'0') {
        if let Some((radix, valid_digit)) = match bytes.get(*pos + 1).copied() {
            Some(b'x') => Some((16, is_hex_digit as fn(u8) -> bool)),
            Some(b'o') => Some((8, is_octal_digit as fn(u8) -> bool)),
            Some(b'b') => Some((2, is_binary_digit as fn(u8) -> bool)),
            _ => None,
        } {
            *pos += 2;
            let digits_start = *pos;
            consume_digit_sequence(bytes, pos, valid_digit)?;
            let cleaned = without_underscores(&input[digits_start..*pos]);
            let value =
                u64::from_str_radix(&cleaned, radix).map_err(|_| "TOML integer out of range")?;
            return i64::try_from(value)
                .map(ParsedValue::Int)
                .map_err(|_| "TOML integer out of range");
        }
    }

    let integer_start = *pos;
    consume_digit_sequence(bytes, pos, is_decimal_digit)?;
    if bytes[integer_start] == b'0'
        && bytes[integer_start + 1..*pos]
            .iter()
            .any(u8::is_ascii_digit)
    {
        return Err("leading zero in TOML integer");
    }

    let mut is_float = false;
    if bytes.get(*pos) == Some(&b'.') {
        is_float = true;
        *pos += 1;
        consume_digit_sequence(bytes, pos, is_decimal_digit)
            .map_err(|_| "missing TOML fraction digits")?;
    }
    if matches!(bytes.get(*pos), Some(b'e' | b'E')) {
        is_float = true;
        *pos += 1;
        if matches!(bytes.get(*pos), Some(b'+' | b'-')) {
            *pos += 1;
        }
        consume_digit_sequence(bytes, pos, is_decimal_digit)
            .map_err(|_| "missing TOML exponent digits")?;
    }

    let cleaned = without_underscores(&input[start..*pos]);
    if is_float {
        let value: f64 = cleaned.parse().map_err(|_| "invalid TOML float")?;
        if !value.is_finite() {
            return Err("TOML float out of range");
        }
        Ok(ParsedValue::Float(value))
    } else {
        cleaned
            .parse()
            .map(ParsedValue::Int)
            .map_err(|_| "TOML integer out of range")
    }
}

fn looks_like_toml_datetime(bytes: &[u8], start: usize) -> bool {
    let mut hyphens = 0usize;
    for byte in bytes.iter().copied().skip(start) {
        if matches!(
            byte,
            b' ' | b'\t' | b'\n' | b'\r' | b'#' | b',' | b']' | b'}'
        ) {
            break;
        }
        if matches!(byte, b':' | b'T' | b't' | b'Z' | b'z') {
            return true;
        }
        if byte == b'-' {
            hyphens += 1;
        }
    }
    if bytes.get(start) == Some(&b'-') {
        hyphens = hyphens.saturating_sub(1);
    }
    hyphens >= 2
}

fn parse_toml_datetime_token(
    bytes: &[u8],
    start: usize,
) -> Result<(TomlDateTimeKind, usize), &'static str> {
    if has_toml_date_shape(bytes, start) {
        let year = parse_fixed_decimal(bytes, start, 4).ok_or("invalid TOML date year")?;
        let month = parse_fixed_decimal(bytes, start + 5, 2).ok_or("invalid TOML date month")?;
        let day = parse_fixed_decimal(bytes, start + 8, 2).ok_or("invalid TOML date day")?;
        if !(1..=12).contains(&month) {
            return Err("TOML date month is out of range");
        }
        if day == 0 || day > days_in_toml_month(year, month) {
            return Err("TOML date day is out of range");
        }

        let mut pos = start + 10;
        let has_time = match bytes.get(pos).copied() {
            Some(b'T' | b't') => {
                pos += 1;
                true
            }
            Some(b' ') if has_toml_time_shape(bytes, pos + 1) => {
                pos += 1;
                true
            }
            _ => false,
        };
        if !has_time {
            return Ok((TomlDateTimeKind::LocalDate, pos));
        }

        pos = parse_toml_partial_time(bytes, pos)?;
        match bytes.get(pos).copied() {
            Some(b'Z' | b'z') => Ok((TomlDateTimeKind::OffsetDateTime, pos + 1)),
            Some(b'+' | b'-') => Ok((
                TomlDateTimeKind::OffsetDateTime,
                parse_toml_offset(bytes, pos)?,
            )),
            _ => Ok((TomlDateTimeKind::LocalDateTime, pos)),
        }
    } else if has_toml_time_shape(bytes, start) {
        Ok((
            TomlDateTimeKind::LocalTime,
            parse_toml_partial_time(bytes, start)?,
        ))
    } else {
        Err("invalid TOML date/time syntax")
    }
}

fn has_toml_date_shape(bytes: &[u8], start: usize) -> bool {
    let Some(end) = start.checked_add(10) else {
        return false;
    };
    end <= bytes.len()
        && bytes.get(start + 4) == Some(&b'-')
        && bytes.get(start + 7) == Some(&b'-')
        && parse_fixed_decimal(bytes, start, 4).is_some()
        && parse_fixed_decimal(bytes, start + 5, 2).is_some()
        && parse_fixed_decimal(bytes, start + 8, 2).is_some()
}

fn has_toml_time_shape(bytes: &[u8], start: usize) -> bool {
    let Some(end) = start.checked_add(5) else {
        return false;
    };
    end <= bytes.len()
        && bytes.get(start + 2) == Some(&b':')
        && parse_fixed_decimal(bytes, start, 2).is_some()
        && parse_fixed_decimal(bytes, start + 3, 2).is_some()
}

fn parse_toml_partial_time(bytes: &[u8], start: usize) -> Result<usize, &'static str> {
    if !has_toml_time_shape(bytes, start) {
        return Err("invalid TOML time syntax");
    }
    let hour = parse_fixed_decimal(bytes, start, 2).ok_or("invalid TOML time hour")?;
    let minute = parse_fixed_decimal(bytes, start + 3, 2).ok_or("invalid TOML time minute")?;
    if hour > 23 {
        return Err("TOML time hour is out of range");
    }
    if minute > 59 {
        return Err("TOML time minute is out of range");
    }

    let mut pos = start + 5;
    if bytes.get(pos) == Some(&b':') {
        let second =
            parse_fixed_decimal(bytes, pos + 1, 2).ok_or("TOML time seconds require two digits")?;
        if second > 60 {
            return Err("TOML time second is out of range");
        }
        pos += 3;
        if bytes.get(pos) == Some(&b'.') {
            pos += 1;
            let fraction_start = pos;
            while bytes.get(pos).is_some_and(u8::is_ascii_digit) {
                pos += 1;
            }
            if pos == fraction_start {
                return Err("TOML fractional second requires digits");
            }
        }
    } else if bytes.get(pos) == Some(&b'.') {
        return Err("TOML fractional second requires seconds");
    }
    Ok(pos)
}

fn parse_toml_offset(bytes: &[u8], start: usize) -> Result<usize, &'static str> {
    let end = start
        .checked_add(6)
        .ok_or("TOML date/time offset overflows input position")?;
    if end > bytes.len()
        || !matches!(bytes.get(start), Some(b'+' | b'-'))
        || bytes.get(start + 3) != Some(&b':')
    {
        return Err("invalid TOML date/time offset");
    }
    let hour =
        parse_fixed_decimal(bytes, start + 1, 2).ok_or("invalid TOML date/time offset hour")?;
    let minute =
        parse_fixed_decimal(bytes, start + 4, 2).ok_or("invalid TOML date/time offset minute")?;
    if hour > 23 || minute > 59 {
        return Err("TOML date/time offset is out of range");
    }
    Ok(end)
}

fn parse_fixed_decimal(bytes: &[u8], start: usize, count: usize) -> Option<u32> {
    let end = start.checked_add(count)?;
    let digits = bytes.get(start..end)?;
    let mut value = 0u32;
    for digit in digits {
        if !digit.is_ascii_digit() {
            return None;
        }
        value = value * 10 + u32::from(*digit - b'0');
    }
    Some(value)
}

fn days_in_toml_month(year: u32, month: u32) -> u32 {
    match month {
        2 if year.is_multiple_of(400) || (year.is_multiple_of(4) && !year.is_multiple_of(100)) => {
            29
        }
        2 => 28,
        4 | 6 | 9 | 11 => 30,
        _ => 31,
    }
}

fn is_toml_value_boundary(byte: u8) -> bool {
    matches!(
        byte,
        b' ' | b'\t' | b'\n' | b'\r' | b'#' | b',' | b']' | b'}'
    )
}

fn consume_digit_sequence(
    bytes: &[u8],
    pos: &mut usize,
    valid_digit: fn(u8) -> bool,
) -> Result<(), &'static str> {
    if !bytes.get(*pos).copied().is_some_and(valid_digit) {
        return Err("expected TOML number digit");
    }
    *pos += 1;
    loop {
        match bytes.get(*pos).copied() {
            Some(byte) if valid_digit(byte) => *pos += 1,
            Some(b'_') => {
                if !bytes.get(*pos + 1).copied().is_some_and(valid_digit) {
                    return Err("invalid underscore in TOML number");
                }
                *pos += 1;
            }
            _ => return Ok(()),
        }
    }
}

fn without_underscores(value: &str) -> String {
    value.chars().filter(|ch| *ch != '_').collect()
}

fn is_hex_digit(byte: u8) -> bool {
    byte.is_ascii_hexdigit()
}

fn is_decimal_digit(byte: u8) -> bool {
    byte.is_ascii_digit()
}

fn is_octal_digit(byte: u8) -> bool {
    matches!(byte, b'0'..=b'7')
}

fn is_binary_digit(byte: u8) -> bool {
    matches!(byte, b'0' | b'1')
}

fn enter_container(depth: usize) -> Result<usize, &'static str> {
    if depth >= MAX_DEPTH {
        return Err("maximum TOML nesting depth exceeded");
    }
    Ok(depth + 1)
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

fn finish_root_line(bytes: &[u8], pos: &mut usize) -> Result<(), &'static str> {
    skip_horizontal_ws(bytes, pos);
    if bytes.get(*pos) == Some(&b'#') {
        skip_comment(bytes, pos)?;
    }
    match bytes.get(*pos).copied() {
        None => Ok(()),
        Some(b'\n') => {
            *pos += 1;
            Ok(())
        }
        Some(b'\r') if bytes.get(*pos + 1) == Some(&b'\n') => {
            *pos += 2;
            Ok(())
        }
        Some(b'\r') => Err("bare carriage return in TOML document"),
        Some(_) => Err("trailing characters after TOML value"),
    }
}

fn skip_document_layout(bytes: &[u8], pos: &mut usize) -> Result<(), &'static str> {
    loop {
        match bytes.get(*pos).copied() {
            Some(b' ' | b'\t' | b'\n') => *pos += 1,
            Some(b'\r') if bytes.get(*pos + 1) == Some(&b'\n') => *pos += 2,
            Some(b'\r') => return Err("bare carriage return in TOML document"),
            Some(b'#') => skip_comment(bytes, pos)?,
            _ => return Ok(()),
        }
    }
}

fn skip_array_layout(bytes: &[u8], pos: &mut usize) -> Result<(), &'static str> {
    loop {
        match bytes.get(*pos).copied() {
            Some(b' ' | b'\t' | b'\n') => *pos += 1,
            Some(b'\r') if bytes.get(*pos + 1) == Some(&b'\n') => *pos += 2,
            Some(b'\r') => return Err("bare carriage return in TOML array"),
            Some(b'#') => skip_comment(bytes, pos)?,
            _ => return Ok(()),
        }
    }
}

fn skip_inline_layout(bytes: &[u8], pos: &mut usize) -> Result<(), &'static str> {
    skip_array_layout(bytes, pos)
}

fn skip_comment(bytes: &[u8], pos: &mut usize) -> Result<(), &'static str> {
    while *pos < bytes.len() && bytes[*pos] != b'\n' && bytes[*pos] != b'\r' {
        let byte = bytes[*pos];
        if (byte < 0x20 && byte != b'\t') || byte == 0x7f {
            return Err("control character in TOML comment");
        }
        *pos += 1;
    }
    Ok(())
}

fn skip_horizontal_ws(bytes: &[u8], pos: &mut usize) {
    while matches!(bytes.get(*pos), Some(b' ' | b'\t')) {
        *pos += 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parses(input: &str) -> bool {
        TomlReader::parse(input).is_ok()
    }

    #[test]
    fn writer_formats_float32_at_float32_precision() {
        let mut writer = TomlWriter::new();
        writer.write_float32(0.1).unwrap();
        assert_eq!(writer.into_bytes(), b"0.1");

        let mut writer = TomlWriter::new();
        writer.write_float32(1.0).unwrap();
        assert_eq!(writer.into_bytes(), b"1.0");

        let mut writer = TomlWriter::new();
        writer.write_float32(f32::NEG_INFINITY).unwrap();
        assert_eq!(writer.into_bytes(), b"-inf");
    }

    #[test]
    fn root_values_require_a_line_ending_or_eof() {
        assert!(parses(""));
        assert!(parses("name = \"vo\" # comment\ncount = 2\n"));
        assert!(!parses("name = \"vo\" trailing"));
        assert!(!parses("name = \"vo\", count = 2"));
    }

    #[test]
    fn inline_tables_and_arrays_follow_toml_1_1_delimiters() {
        assert!(parses("items = [1, 2, 3,]\n"));
        assert!(!parses("point = {x = 1 y = 2}\n"));
        assert!(parses("point = {x = 1,}\n"));
        assert!(parses("point = {\n x = 1, # comment\n}\n"));
        assert!(!parses("items = [1 2]\n"));
        assert!(parses("items = [1, \"two\"]\n"));
        assert!(parses("items = [[1], [\"two\"]]\n"));
        assert!(!parses("point = {x\n= 1}\n"));
        assert!(!parses("point = {x =\n1}\n"));
    }

    #[test]
    fn arrays_are_materialized_for_typed_unmarshal() {
        let ParsedValue::Object(mut root) = TomlReader::parse("items = [1, 2]\n").unwrap() else {
            panic!("expected root object");
        };
        let (_, ParsedValue::Array(values)) =
            TomlReader::next_field(&mut root).unwrap().expect("field")
        else {
            panic!("expected array");
        };
        assert_eq!(values.len(), 2);
    }

    #[test]
    fn strings_preserve_utf8_and_reject_controls() {
        let ParsedValue::Object(mut root) =
            TomlReader::parse("text = \"中文\\u0021\\x21\\e\"\n").unwrap()
        else {
            panic!("expected root object");
        };
        let (_, ParsedValue::String(value)) =
            TomlReader::next_field(&mut root).unwrap().expect("field")
        else {
            panic!("expected string");
        };
        assert_eq!(value, "中文!!\u{1b}");
        assert!(parses("basic = \"a\tb\"\nliteral = 'a\tb'\n"));
        assert!(!parses("text = \"bad\u{7f}\"\n"));
        assert!(!parses("text = \"bad\u{000b}\"\n"));
        assert!(!parses(r#"text = "\uD800""#));
    }

    #[test]
    fn numbers_follow_the_supported_toml_grammar() {
        for valid in ["v = +1", "v = 1_000", "v = 0xff", "v = 1.5e-2", "v = -inf"] {
            assert!(parses(valid), "rejected {valid:?}");
        }
        for invalid in [
            "v = 01",
            "v = 1_",
            "v = 1.",
            "v = 1e+",
            "v = 0x",
            "v = 1e400",
        ] {
            assert!(!parses(invalid), "accepted {invalid:?}");
        }
    }

    #[test]
    fn duplicate_keys_are_rejected() {
        assert!(!parses("v = 1\nv = 2\n"));
        assert!(!parses("v = {x = 1, x = 2}\n"));
    }

    #[test]
    fn canonical_transport_rejects_document_only_syntax() {
        assert_eq!(
            TomlReader::parse("[table]\nvalue = 1\n").err(),
            Some("TOML table headers are not supported by struct Unmarshal")
        );
        assert_eq!(
            TomlReader::parse("{value = 1}").err(),
            Some("standalone TOML inline table is not a document")
        );
        assert_eq!(
            TomlReader::parse("value = \"\"\"multi\"\"\"\n").err(),
            Some("multiline TOML strings are not supported by struct Unmarshal")
        );
    }

    #[test]
    fn date_time_tokens_preserve_category_and_precision() {
        let cases = [
            (
                "1979-05-27T07:32:00.999999999999-07:00",
                TomlDateTimeKind::OffsetDateTime,
            ),
            ("1979-05-27 07:32z", TomlDateTimeKind::OffsetDateTime),
            ("1979-05-27t07:32:00.5", TomlDateTimeKind::LocalDateTime),
            ("2000-02-29", TomlDateTimeKind::LocalDate),
            ("12:34:60.000", TomlDateTimeKind::LocalTime),
            ("07:32", TomlDateTimeKind::LocalTime),
        ];
        for (raw, expected_kind) in cases {
            let input = format!("value = {raw}\n");
            let ParsedValue::Object(mut root) = TomlReader::parse(&input).unwrap() else {
                panic!("expected root object for {raw}");
            };
            let (_, ParsedValue::NamedString { value, type_name }) =
                TomlReader::next_field(&mut root).unwrap().expect("field")
            else {
                panic!("expected semantic string for {raw}");
            };
            assert_eq!(value, raw);
            assert_eq!(type_name, expected_kind.type_name());
        }
    }

    #[test]
    fn date_time_tokens_validate_calendar_time_and_offset_ranges() {
        for invalid in [
            "value = 1900-02-29",
            "value = 2000-02-30",
            "value = 1979-13-01",
            "value = 1979-00-01",
            "value = 24:00",
            "value = 23:60",
            "value = 23:59:61",
            "value = 07:32.1",
            "value = 07:32:00.",
            "value = 1979-05-27T07:32+24:00",
            "value = 1979-05-27T07:32+00:60",
            "value = 1979-05-27T07:32+7:00",
            "value = 1979-05-27T07:32Zjunk",
            "value = 1979-05-27  07:32",
        ] {
            assert!(!parses(invalid), "accepted {invalid:?}");
        }
        assert!(parses("value = 2000-02-29"));
        assert!(parses("value = 12:34:60"));
        assert!(parses("value = 1979-05-27T07:32+23:59"));
        assert!(parses("value = 1979-05-27T07:32-00:00"));
    }

    #[test]
    fn writer_emits_only_valid_exact_toml_date_time_types_as_raw_tokens() {
        let mut writer = TomlWriter::new();
        writer
            .write_typed_string_bytes(
                b"1979-05-27T07:32:00.123456789Z",
                Some(OFFSET_DATE_TIME_TYPE),
            )
            .unwrap();
        assert_eq!(writer.into_bytes(), b"1979-05-27T07:32:00.123456789Z");

        let mut spoofed = TomlWriter::new();
        spoofed
            .write_typed_string_bytes(
                b"1979-05-27T07:32:00Z",
                Some("example.com/user.OffsetDateTime"),
            )
            .unwrap();
        assert_eq!(spoofed.into_bytes(), b"\"1979-05-27T07:32:00Z\"");

        assert!(TomlWriter::new()
            .write_typed_string_bytes(b"1979-02-29", Some(LOCAL_DATE_TYPE))
            .is_err());
        assert!(TomlWriter::new()
            .write_typed_string_bytes(b"1979-05-27", Some(LOCAL_TIME_TYPE))
            .is_err());
    }

    #[test]
    fn date_time_string_targets_require_exact_categories() {
        assert!(
            TomlReader::validate_string_target(Some(LOCAL_DATE_TYPE), Some(LOCAL_DATE_TYPE))
                .is_ok()
        );
        assert!(TomlReader::validate_string_target(None, Some(LOCAL_DATE_TYPE)).is_err());
        assert!(TomlReader::validate_string_target(Some(LOCAL_DATE_TYPE), None).is_err());
        assert!(
            TomlReader::validate_string_target(Some(LOCAL_DATE_TYPE), Some(LOCAL_TIME_TYPE))
                .is_err()
        );
        assert!(
            TomlReader::validate_string_target(None, Some("example.com/user.NamedString")).is_ok()
        );
    }

    #[test]
    fn nesting_limit_is_checked_before_recursing() {
        let allowed = format!(
            "value = {}0{}",
            "[".repeat(MAX_DEPTH - 1),
            "]".repeat(MAX_DEPTH - 1)
        );
        assert!(parses(&allowed));

        let too_deep = format!(
            "value = {}0{}",
            "[".repeat(MAX_DEPTH),
            "]".repeat(MAX_DEPTH)
        );
        assert_eq!(
            TomlReader::parse(&too_deep).err(),
            Some("maximum TOML nesting depth exceeded")
        );
    }

    #[test]
    fn writer_uses_inline_tables_for_nested_objects_and_rejects_null() {
        let mut writer = TomlWriter::new();
        writer.write_object_start();
        writer.write_field_start("nested", true);
        writer.write_object_start();
        writer.write_field_start("value", true);
        writer.write_int(1);
        writer.write_field_end();
        writer.write_object_end();
        writer.write_field_end();
        writer.write_object_end();
        assert_eq!(writer.into_bytes(), b"nested = {value = 1}\n");

        assert_eq!(
            TomlWriter::new().write_null(),
            Err("TOML does not support null values")
        );
    }
}
