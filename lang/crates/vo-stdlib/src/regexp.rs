//! regexp package native function implementations.
//!
//! Regular expression matching using Rust's regex crate.
//! Works on both std and WASM targets while preserving Vo's arbitrary-byte
//! string contract.

#[cfg(not(feature = "std"))]
use alloc::{string::String, vec, vec::Vec};

use icu_properties::props::{GeneralCategory, GeneralCategoryGroup};
use icu_properties::CodePointMapData;
use regex::Captures;

use crate::raw_utf8;
use vo_ffi_macro::vostd_fn;
use vo_runtime::ffi::{ExternCallContext, ExternResult};

mod pattern;
use pattern::CompiledRegex;

const NO_RAW_BOUNDARY: usize = usize::MAX;

/// A valid UTF-8 view of arbitrary Vo string bytes.
///
/// Go's regexp package decodes every malformed input byte as one RuneError but
/// reports offsets into the original byte string. Rust's string regexp engine
/// requires valid UTF-8, so malformed bytes are expanded to the UTF-8 encoding
/// of RuneError and every resulting character boundary is mapped back to its
/// original byte offset.
struct Utf8View {
    text: String,
    raw_boundaries: Vec<usize>,
}

impl Utf8View {
    fn new(raw: &[u8]) -> Self {
        let mut text = Vec::with_capacity(raw.len());
        let mut raw_boundaries = vec![0];
        let mut raw_offset = 0;

        while raw_offset < raw.len() {
            let (rune, width) = raw_utf8::decode_first(&raw[raw_offset..]);
            let mut encoded = [0_u8; 4];
            text.extend_from_slice(rune.encode_utf8(&mut encoded).as_bytes());
            raw_boundaries.resize(text.len() + 1, NO_RAW_BOUNDARY);
            raw_offset += width;
            raw_boundaries[text.len()] = raw_offset;
        }

        Self {
            text: String::from_utf8(text)
                .expect("Utf8View encodes every decoded rune as valid UTF-8"),
            raw_boundaries,
        }
    }

    fn raw_offset(&self, text_offset: usize) -> usize {
        let raw_offset = self
            .raw_boundaries
            .get(text_offset)
            .copied()
            .unwrap_or(NO_RAW_BOUNDARY);
        assert_ne!(
            raw_offset, NO_RAW_BOUNDARY,
            "regexp engine returned a non-character UTF-8 boundary"
        );
        raw_offset
    }
}

type RawCapture = Option<(usize, usize)>;
type RawCaptures = Vec<RawCapture>;

fn compile_pattern(pattern: &[u8]) -> Option<CompiledRegex> {
    pattern::compile(pattern)
}

fn capture_ranges(view: &Utf8View, captures: &Captures<'_>) -> RawCaptures {
    captures
        .iter()
        .map(|capture| {
            capture.map(|found| (view.raw_offset(found.start()), view.raw_offset(found.end())))
        })
        .collect()
}

fn first_capture_ranges(re: &CompiledRegex, view: &Utf8View) -> Option<RawCaptures> {
    re.captures(&view.text)
        .map(|captures| capture_ranges(view, &captures))
}

/// Return successive non-overlapping matches using Go's empty-match rule.
/// An empty match immediately following another match is discarded, and an
/// empty match otherwise advances by one decoded rune.
fn all_capture_ranges(re: &CompiledRegex, view: &Utf8View, n: i64) -> Vec<RawCaptures> {
    if n == 0 {
        return Vec::new();
    }
    let limit = match_limit(n).unwrap_or(usize::MAX);
    let mut result = Vec::new();
    let mut search_pos = 0;
    let mut previous_match_end = None;
    let text_end = view.text.len();

    while result.len() < limit && search_pos <= text_end {
        let Some(captures) = re.captures_at(&view.text, search_pos) else {
            break;
        };
        let whole = captures
            .get(0)
            .expect("a successful regexp capture set contains the whole match");
        let mut accept = true;

        if whole.end() == search_pos {
            if previous_match_end == Some(whole.start()) {
                accept = false;
            }
            if let Some(rune) = view.text[search_pos..].chars().next() {
                search_pos += rune.len_utf8();
            } else {
                search_pos = text_end.saturating_add(1);
            }
        } else {
            search_pos = whole.end();
        }
        previous_match_end = Some(whole.end());

        if accept {
            result.push(capture_ranges(view, &captures));
        }
    }
    result
}

fn match_limit(n: i64) -> Option<usize> {
    if n < 0 {
        None
    } else {
        Some(usize::try_from(n).unwrap_or(usize::MAX))
    }
}

fn ret_int_slice(call: &mut ExternCallContext, ret_slot: u16, values: &[i64]) {
    use vo_common_core::types::{ValueKind, ValueMeta};
    use vo_runtime::objects::slice;

    if values.is_empty() {
        call.ret_nil(ret_slot);
        return;
    }

    let result = call.alloc_slice(ValueMeta::new(0, ValueKind::Int), 8, values.len());
    for (index, value) in values.iter().copied().enumerate() {
        // Safety: `result` was allocated immediately above with exactly
        // `values.len()` integer elements.
        unsafe { slice::set(result, index, value as u64, 8) };
    }
    call.ret_ref(ret_slot, result);
}

fn ret_optional_string_bytes_slice(
    call: &mut ExternCallContext,
    ret_slot: u16,
    values: &[Vec<u8>],
) {
    if values.is_empty() {
        call.ret_nil(ret_slot);
    } else {
        call.ret_string_bytes_slice(ret_slot, values);
    }
}

enum ReplacementReference<'a> {
    Index(usize),
    Name(&'a [u8]),
}

fn replacement_name_char(rune: char) -> bool {
    let category = CodePointMapData::<GeneralCategory>::new().get(rune);
    rune == '_'
        || GeneralCategoryGroup::Letter.contains(category)
        || GeneralCategoryGroup::DecimalNumber.contains(category)
}

/// Parse the name after a replacement-template `$` using Go's longest-name
/// rule. The returned byte count includes an optional closing brace.
fn extract_replacement_reference(template: &[u8]) -> Option<(ReplacementReference<'_>, usize)> {
    if template.is_empty() {
        return None;
    }

    let braced = template[0] == b'{';
    let name_start = usize::from(braced);
    let mut name_end = name_start;
    while name_end < template.len() {
        let (rune, width) = raw_utf8::decode_first(&template[name_end..]);
        if !replacement_name_char(rune) {
            break;
        }
        name_end += width;
    }
    if name_end == name_start {
        return None;
    }

    let consumed = if braced {
        if template.get(name_end) != Some(&b'}') {
            return None;
        }
        name_end + 1
    } else {
        name_end
    };
    let name = &template[name_start..name_end];

    let mut number = 0_usize;
    let mut numeric = true;
    for &byte in name {
        if !byte.is_ascii_digit() || number >= 100_000_000 {
            numeric = false;
            break;
        }
        number = number * 10 + usize::from(byte - b'0');
    }
    if name[0] == b'0' && name.len() > 1 {
        numeric = false;
    }

    let reference = if numeric {
        ReplacementReference::Index(number)
    } else {
        ReplacementReference::Name(name)
    };
    Some((reference, consumed))
}

fn append_capture(result: &mut Vec<u8>, raw: &[u8], capture: RawCapture) {
    if let Some((start, end)) = capture {
        result.extend_from_slice(&raw[start..end]);
    }
}

fn expand_replacement(
    result: &mut Vec<u8>,
    re: &CompiledRegex,
    template: &[u8],
    raw: &[u8],
    captures: &[RawCapture],
) {
    let mut cursor = 0;
    while let Some(relative) = template[cursor..].iter().position(|&byte| byte == b'$') {
        let dollar = cursor + relative;
        result.extend_from_slice(&template[cursor..dollar]);
        let after_dollar = dollar + 1;

        if template.get(after_dollar) == Some(&b'$') {
            result.push(b'$');
            cursor = after_dollar + 1;
            continue;
        }

        let Some((reference, consumed)) = extract_replacement_reference(&template[after_dollar..])
        else {
            result.push(b'$');
            cursor = after_dollar;
            continue;
        };

        match reference {
            ReplacementReference::Index(index) => {
                append_capture(result, raw, captures.get(index).copied().flatten());
            }
            ReplacementReference::Name(name) => {
                for (index, candidate) in re.capture_names_go().iter().enumerate() {
                    if candidate
                        .as_ref()
                        .is_some_and(|candidate| candidate.as_bytes() == name)
                        && captures.get(index).copied().flatten().is_some()
                    {
                        append_capture(result, raw, captures[index]);
                        break;
                    }
                }
            }
        }
        cursor = after_dollar + consumed;
    }
    result.extend_from_slice(&template[cursor..]);
}

fn replace_all_raw(re: &CompiledRegex, raw: &[u8], replacement: &[u8], literal: bool) -> Vec<u8> {
    let view = Utf8View::new(raw);
    let matches = all_capture_ranges(re, &view, -1);
    let mut result = Vec::new();
    let mut last_match_end = 0;

    for captures in matches {
        let (start, end) =
            captures[0].expect("a successful regexp capture set contains the whole match");
        result.extend_from_slice(&raw[last_match_end..start]);
        if literal {
            result.extend_from_slice(replacement);
        } else {
            expand_replacement(&mut result, re, replacement, raw, &captures);
        }
        last_match_end = end;
    }
    result.extend_from_slice(&raw[last_match_end..]);
    result
}

fn quote_meta_raw(raw: &[u8]) -> Vec<u8> {
    const SPECIAL: &[u8] = br"\.+*?()|[]{}^$";

    let mut result = Vec::with_capacity(raw.len());
    for &byte in raw {
        if SPECIAL.contains(&byte) {
            result.push(b'\\');
        }
        result.push(byte);
    }
    result
}

fn split_raw(re: &CompiledRegex, raw: &[u8], n: i64) -> Vec<Vec<u8>> {
    if n == 0 {
        return Vec::new();
    }
    if !re.expression_is_empty() && raw.is_empty() {
        return vec![Vec::new()];
    }

    let view = Utf8View::new(raw);
    let matches = all_capture_ranges(re, &view, n);
    let result_limit = match_limit(n).unwrap_or(usize::MAX);
    let mut result = Vec::with_capacity(matches.len().saturating_add(1).min(result_limit));
    let mut beginning = 0;
    let mut last_match_start = 0;

    for captures in matches {
        if result.len().saturating_add(1) >= result_limit {
            break;
        }
        let (start, end) =
            captures[0].expect("a successful regexp capture set contains the whole match");
        last_match_start = start;
        if end != 0 {
            result.push(raw[beginning..start].to_vec());
        }
        beginning = end;
    }
    if last_match_start != raw.len() {
        result.push(raw[beginning..].to_vec());
    }
    result
}

// ==================== Matching ====================

#[vostd_fn("regexp", "matchString")]
fn match_string(call: &mut ExternCallContext) -> ExternResult {
    let pattern = call.arg_string_bytes(slots::ARG_PATTERN);
    let raw = call.arg_string_bytes(slots::ARG_S);
    let (matched, valid) = match compile_pattern(&pattern) {
        Some(re) => (re.is_match(&Utf8View::new(&raw).text), true),
        None => (false, false),
    };
    call.ret_bool(slots::RET_0, matched);
    call.ret_bool(slots::RET_1, valid);
    ExternResult::Ok
}

#[vostd_fn("regexp", "matchBytes")]
fn match_bytes(call: &mut ExternCallContext) -> ExternResult {
    let pattern = call.arg_string_bytes(slots::ARG_PATTERN);
    let raw = call.arg_bytes(slots::ARG_B);
    let (matched, valid) = match compile_pattern(&pattern) {
        Some(re) => (re.is_match(&Utf8View::new(raw).text), true),
        None => (false, false),
    };
    call.ret_bool(slots::RET_0, matched);
    call.ret_bool(slots::RET_1, valid);
    ExternResult::Ok
}

// ==================== Find ====================

#[vostd_fn("regexp", "findString")]
fn find_string(call: &mut ExternCallContext) -> ExternResult {
    let pattern = call.arg_string_bytes(slots::ARG_PATTERN);
    let raw = call.arg_string_bytes(slots::ARG_S);
    let result = match compile_pattern(&pattern) {
        Some(re) => {
            let view = Utf8View::new(&raw);
            re.find(&view.text)
                .map(|found| {
                    raw[view.raw_offset(found.start())..view.raw_offset(found.end())].to_vec()
                })
                .unwrap_or_default()
        }
        None => Vec::new(),
    };
    call.ret_string_bytes(slots::RET_0, &result);
    ExternResult::Ok
}

#[vostd_fn("regexp", "findStringIndex")]
fn find_string_index(call: &mut ExternCallContext) -> ExternResult {
    let pattern = call.arg_string_bytes(slots::ARG_PATTERN);
    let raw = call.arg_string_bytes(slots::ARG_S);
    let (start, end) = match compile_pattern(&pattern) {
        Some(re) => {
            let view = Utf8View::new(&raw);
            re.find(&view.text)
                .map(|found| {
                    (
                        i64::try_from(view.raw_offset(found.start())).unwrap_or(i64::MAX),
                        i64::try_from(view.raw_offset(found.end())).unwrap_or(i64::MAX),
                    )
                })
                .unwrap_or((-1, -1))
        }
        None => (-1, -1),
    };
    call.ret_i64(slots::RET_0, start);
    call.ret_i64(slots::RET_1, end);
    ExternResult::Ok
}

#[vostd_fn("regexp", "findAllString")]
fn find_all_string(call: &mut ExternCallContext) -> ExternResult {
    let pattern = call.arg_string_bytes(slots::ARG_PATTERN);
    let raw = call.arg_string_bytes(slots::ARG_S);
    let n = call.arg_i64(slots::ARG_N);

    let results = match compile_pattern(&pattern) {
        Some(re) => {
            let view = Utf8View::new(&raw);
            all_capture_ranges(&re, &view, n)
                .into_iter()
                .map(|captures| {
                    let (start, end) = captures[0]
                        .expect("a successful regexp capture set contains the whole match");
                    raw[start..end].to_vec()
                })
                .collect::<Vec<_>>()
        }
        None => Vec::new(),
    };

    ret_optional_string_bytes_slice(call, slots::RET_0, &results);
    ExternResult::Ok
}

// ==================== Replace ====================

#[vostd_fn("regexp", "replaceAllString")]
fn replace_all_string(call: &mut ExternCallContext) -> ExternResult {
    let pattern = call.arg_string_bytes(slots::ARG_PATTERN);
    let src = call.arg_string_bytes(slots::ARG_SRC);
    let replacement = call.arg_string_bytes(slots::ARG_REPL);
    let result = match compile_pattern(&pattern) {
        Some(re) => replace_all_raw(&re, &src, &replacement, false),
        None => src,
    };
    call.ret_string_bytes(slots::RET_0, &result);
    ExternResult::Ok
}

#[vostd_fn("regexp", "replaceAllLiteralString")]
fn replace_all_literal_string(call: &mut ExternCallContext) -> ExternResult {
    let pattern = call.arg_string_bytes(slots::ARG_PATTERN);
    let src = call.arg_string_bytes(slots::ARG_SRC);
    let replacement = call.arg_string_bytes(slots::ARG_REPL);
    let result = match compile_pattern(&pattern) {
        Some(re) => replace_all_raw(&re, &src, &replacement, true),
        None => src,
    };
    call.ret_string_bytes(slots::RET_0, &result);
    ExternResult::Ok
}

// ==================== Split ====================

#[vostd_fn("regexp", "splitString")]
fn split_string(call: &mut ExternCallContext) -> ExternResult {
    let pattern = call.arg_string_bytes(slots::ARG_PATTERN);
    let raw = call.arg_string_bytes(slots::ARG_S);
    let n = call.arg_i64(slots::ARG_N);

    if n == 0 {
        call.ret_nil(slots::RET_0);
        return ExternResult::Ok;
    }

    let results = match compile_pattern(&pattern) {
        Some(re) => split_raw(&re, &raw, n),
        None => Vec::new(),
    };

    call.ret_string_bytes_slice(slots::RET_0, &results);
    ExternResult::Ok
}

// ==================== Submatch ====================

#[vostd_fn("regexp", "findStringSubmatch")]
fn find_string_submatch(call: &mut ExternCallContext) -> ExternResult {
    let pattern = call.arg_string_bytes(slots::ARG_PATTERN);
    let raw = call.arg_string_bytes(slots::ARG_S);

    let results = match compile_pattern(&pattern) {
        Some(re) => {
            let view = Utf8View::new(&raw);
            first_capture_ranges(&re, &view)
                .map(|captures| {
                    captures
                        .into_iter()
                        .map(|capture| {
                            capture
                                .map(|(start, end)| raw[start..end].to_vec())
                                .unwrap_or_default()
                        })
                        .collect::<Vec<_>>()
                })
                .unwrap_or_default()
        }
        None => Vec::new(),
    };

    ret_optional_string_bytes_slice(call, slots::RET_0, &results);
    ExternResult::Ok
}

#[vostd_fn("regexp", "findAllStringIndexFlat")]
fn find_all_string_index_flat(call: &mut ExternCallContext) -> ExternResult {
    let pattern = call.arg_string_bytes(slots::ARG_PATTERN);
    let raw = call.arg_string_bytes(slots::ARG_S);
    let n = call.arg_i64(slots::ARG_N);

    let mut indices = Vec::new();
    if let Some(re) = compile_pattern(&pattern) {
        let view = Utf8View::new(&raw);
        for captures in all_capture_ranges(&re, &view, n) {
            let (start, end) =
                captures[0].expect("a successful regexp capture set contains the whole match");
            indices.push(i64::try_from(start).unwrap_or(i64::MAX));
            indices.push(i64::try_from(end).unwrap_or(i64::MAX));
        }
    }
    ret_int_slice(call, slots::RET_0, &indices);
    ExternResult::Ok
}

#[vostd_fn("regexp", "findAllStringSubmatchFlat")]
fn find_all_string_submatch_flat(call: &mut ExternCallContext) -> ExternResult {
    let pattern = call.arg_string_bytes(slots::ARG_PATTERN);
    let raw = call.arg_string_bytes(slots::ARG_S);
    let n = call.arg_i64(slots::ARG_N);

    let mut values = Vec::new();
    let mut width = 0;
    if let Some(re) = compile_pattern(&pattern) {
        width = re.captures_len();
        let view = Utf8View::new(&raw);
        for captures in all_capture_ranges(&re, &view, n) {
            values.extend(captures.into_iter().map(|capture| {
                capture
                    .map(|(start, end)| raw[start..end].to_vec())
                    .unwrap_or_default()
            }));
        }
    }

    ret_optional_string_bytes_slice(call, slots::RET_0, &values);
    call.ret_i64(slots::RET_1, i64::try_from(width).unwrap_or(i64::MAX));
    ExternResult::Ok
}

#[vostd_fn("regexp", "findAllStringSubmatchIndexFlat")]
fn find_all_string_submatch_index_flat(call: &mut ExternCallContext) -> ExternResult {
    let pattern = call.arg_string_bytes(slots::ARG_PATTERN);
    let raw = call.arg_string_bytes(slots::ARG_S);
    let n = call.arg_i64(slots::ARG_N);

    let mut indices = Vec::new();
    let mut width = 0;
    if let Some(re) = compile_pattern(&pattern) {
        width = re.captures_len();
        let view = Utf8View::new(&raw);
        for captures in all_capture_ranges(&re, &view, n) {
            for capture in captures {
                if let Some((start, end)) = capture {
                    indices.push(i64::try_from(start).unwrap_or(i64::MAX));
                    indices.push(i64::try_from(end).unwrap_or(i64::MAX));
                } else {
                    indices.push(-1);
                    indices.push(-1);
                }
            }
        }
    }

    ret_int_slice(call, slots::RET_0, &indices);
    call.ret_i64(slots::RET_1, i64::try_from(width).unwrap_or(i64::MAX));
    ExternResult::Ok
}

#[vostd_fn("regexp", "subexpNames")]
fn subexp_names(call: &mut ExternCallContext) -> ExternResult {
    let pattern = call.arg_string_bytes(slots::ARG_PATTERN);
    let names = compile_pattern(&pattern)
        .map(|re| {
            re.capture_names_go()
                .iter()
                .map(|name| {
                    name.as_ref()
                        .map(|name| name.as_bytes().to_vec())
                        .unwrap_or_default()
                })
                .collect::<Vec<_>>()
        })
        .unwrap_or_default();
    ret_optional_string_bytes_slice(call, slots::RET_0, &names);
    ExternResult::Ok
}

#[vostd_fn("regexp", "findBytesSubmatchIndex")]
fn find_bytes_submatch_index(call: &mut ExternCallContext) -> ExternResult {
    let pattern = call.arg_string_bytes(slots::ARG_PATTERN);
    let raw = call.arg_bytes(slots::ARG_B);

    let mut indices = Vec::new();
    if let Some(re) = compile_pattern(&pattern) {
        let view = Utf8View::new(raw);
        if let Some(captures) = first_capture_ranges(&re, &view) {
            for capture in captures {
                if let Some((start, end)) = capture {
                    indices.push(i64::try_from(start).unwrap_or(i64::MAX));
                    indices.push(i64::try_from(end).unwrap_or(i64::MAX));
                } else {
                    indices.push(-1);
                    indices.push(-1);
                }
            }
        }
    }
    ret_int_slice(call, slots::RET_0, &indices);
    ExternResult::Ok
}

#[vostd_fn("regexp", "findAllBytesIndexFlat")]
fn find_all_bytes_index_flat(call: &mut ExternCallContext) -> ExternResult {
    let pattern = call.arg_string_bytes(slots::ARG_PATTERN);
    let raw = call.arg_bytes(slots::ARG_B);
    let n = call.arg_i64(slots::ARG_N);

    let mut indices = Vec::new();
    if let Some(re) = compile_pattern(&pattern) {
        let view = Utf8View::new(raw);
        for captures in all_capture_ranges(&re, &view, n) {
            let (start, end) =
                captures[0].expect("a successful regexp capture set contains the whole match");
            indices.push(i64::try_from(start).unwrap_or(i64::MAX));
            indices.push(i64::try_from(end).unwrap_or(i64::MAX));
        }
    }
    ret_int_slice(call, slots::RET_0, &indices);
    ExternResult::Ok
}

#[vostd_fn("regexp", "replaceAllBytes")]
fn replace_all_bytes(call: &mut ExternCallContext) -> ExternResult {
    let pattern = call.arg_string_bytes(slots::ARG_PATTERN);
    let src = call.arg_bytes_owned(slots::ARG_SRC);
    let replacement = call.arg_bytes_owned(slots::ARG_REPL);
    let result = compile_pattern(&pattern)
        .map(|re| replace_all_raw(&re, &src, &replacement, false))
        .unwrap_or(src);
    if result.is_empty() {
        call.ret_nil(slots::RET_0);
    } else {
        call.ret_bytes(slots::RET_0, &result);
    }
    ExternResult::Ok
}

#[vostd_fn("regexp", "replaceAllLiteralBytes")]
fn replace_all_literal_bytes(call: &mut ExternCallContext) -> ExternResult {
    let pattern = call.arg_string_bytes(slots::ARG_PATTERN);
    let src = call.arg_bytes_owned(slots::ARG_SRC);
    let replacement = call.arg_bytes_owned(slots::ARG_REPL);
    let result = compile_pattern(&pattern)
        .map(|re| replace_all_raw(&re, &src, &replacement, true))
        .unwrap_or(src);
    if result.is_empty() {
        call.ret_nil(slots::RET_0);
    } else {
        call.ret_bytes(slots::RET_0, &result);
    }
    ExternResult::Ok
}

// ==================== Quote ====================

#[vostd_fn("regexp", "quoteMeta")]
fn quote_meta(call: &mut ExternCallContext) -> ExternResult {
    let raw = call.arg_string_bytes(slots::ARG_S);
    let result = quote_meta_raw(&raw);
    call.ret_string_bytes(slots::RET_0, &result);
    ExternResult::Ok
}

vo_ffi_macro::vostd_register!("regexp":
    matchString,
    matchBytes,
    findString,
    findStringIndex,
    findAllString,
    replaceAllString,
    replaceAllLiteralString,
    splitString,
    findStringSubmatch,
    findAllStringIndexFlat,
    findAllStringSubmatchFlat,
    findAllStringSubmatchIndexFlat,
    subexpNames,
    findBytesSubmatchIndex,
    findAllBytesIndexFlat,
    replaceAllBytes,
    replaceAllLiteralBytes,
    quoteMeta,
);

#[cfg(test)]
mod tests {
    use super::*;

    fn whole_ranges(re: &CompiledRegex, raw: &[u8], n: i64) -> Vec<(usize, usize)> {
        let view = Utf8View::new(raw);
        all_capture_ranges(re, &view, n)
            .into_iter()
            .map(|captures| captures[0].expect("every match has a whole-match capture"))
            .collect()
    }

    #[test]
    fn utf8_view_maps_each_malformed_byte_to_one_rune_error() {
        let view = Utf8View::new(&[0xff, 0xfe, b'A']);
        assert_eq!(view.text, "\u{fffd}\u{fffd}A");
        assert_eq!(view.raw_offset(0), 0);
        assert_eq!(view.raw_offset(3), 1);
        assert_eq!(view.raw_offset(6), 2);
        assert_eq!(view.raw_offset(7), 3);

        let rune_error = compile_pattern("\u{fffd}".as_bytes()).expect("valid pattern");
        assert_eq!(
            whole_ranges(&rune_error, &[0xff, 0xfe, b'A'], -1),
            [(0, 1), (1, 2)]
        );

        let anchored = compile_pattern(b"^..A$").expect("valid pattern");
        assert_eq!(whole_ranges(&anchored, &[0xff, 0xfe, b'A'], -1), [(0, 3)]);
    }

    #[test]
    fn empty_matches_advance_over_original_rune_widths() {
        let empty = compile_pattern(b"").expect("valid pattern");
        let raw = [0xff, 0xc3, 0xa9, 0xfe];
        assert_eq!(
            whole_ranges(&empty, &raw, -1),
            [(0, 0), (1, 1), (3, 3), (4, 4)]
        );

        let optional = compile_pattern(b"a*").expect("valid pattern");
        assert_eq!(
            whole_ranges(&optional, b"abaab", -1),
            [(0, 1), (2, 4), (5, 5)]
        );
        assert_eq!(whole_ranges(&optional, b"abaab", 2), [(0, 1), (2, 4)]);
        assert!(whole_ranges(&optional, b"abaab", 0).is_empty());
    }

    #[test]
    fn replacement_expansion_preserves_raw_captures_and_templates() {
        let re = compile_pattern(b"(?P<word>.)(a)?").expect("valid pattern");
        let raw = [0xff];

        assert_eq!(
            replace_all_raw(&re, &raw, b"<$word>|${word}|$0|$1|$2|$$", false),
            [b'<', 0xff, b'>', b'|', 0xff, b'|', 0xff, b'|', 0xff, b'|', b'|', b'$']
        );
        assert_eq!(
            replace_all_raw(&re, &raw, b"$0-X", false),
            [0xff, b'-', b'X']
        );
        assert_eq!(replace_all_raw(&re, &raw, b"$00-X", false), b"-X");
        assert_eq!(replace_all_raw(&re, &raw, b"$01-X", false), b"-X");
        assert_eq!(replace_all_raw(&re, &raw, b"$1x-X", false), b"-X");
        assert_eq!(
            replace_all_raw(&re, &raw, b"${1}x-X", false),
            [0xff, b'x', b'-', b'X']
        );
        assert_eq!(replace_all_raw(&re, &raw, b"$999999999-X", false), b"-X");
        assert_eq!(replace_all_raw(&re, &raw, "$名-X".as_bytes(), false), b"-X");
        assert_eq!(
            replace_all_raw(&re, &raw, b"${missing-X", false),
            b"${missing-X"
        );

        let duplicate = compile_pattern(b"(?P<x>a)|(?P<x>b)").expect("valid duplicate names");
        assert_eq!(replace_all_raw(&duplicate, b"b", b"$x", false), b"b");

        let raw_template = [b'<', 0xfe, b'>'];
        assert_eq!(
            replace_all_raw(&re, &raw, &raw_template, false),
            raw_template
        );
        assert_eq!(
            replace_all_raw(&re, &raw, &raw_template, true),
            raw_template
        );
    }

    #[test]
    fn quote_meta_preserves_every_non_metacharacter_byte() {
        assert_eq!(
            quote_meta_raw(&[0xff, b'.', 0xfe, b'$', b'a']),
            [0xff, b'\\', b'.', 0xfe, b'\\', b'$', b'a']
        );
    }

    #[test]
    fn split_matches_go_empty_match_and_limit_rules() {
        let comma = compile_pattern(b",").expect("valid pattern");
        assert_eq!(
            split_raw(&comma, b"a,b,c", -1),
            [b"a".to_vec(), b"b".to_vec(), b"c".to_vec()]
        );
        assert_eq!(
            split_raw(&comma, b"a,b,c", 2),
            [b"a".to_vec(), b"b,c".to_vec()]
        );
        assert_eq!(split_raw(&comma, b"", -1), [Vec::<u8>::new()]);
        assert!(split_raw(&comma, b"a,b", 0).is_empty());

        let empty = compile_pattern(b"").expect("valid pattern");
        assert_eq!(
            split_raw(&empty, "é🙂".as_bytes(), -1),
            ["é".as_bytes().to_vec(), "🙂".as_bytes().to_vec()]
        );
        assert_eq!(split_raw(&empty, b"", -1), Vec::<Vec<u8>>::new());
    }

    #[test]
    fn malformed_utf8_patterns_are_rejected_before_compilation() {
        assert!(compile_pattern(&[0xff]).is_none());
        assert!(compile_pattern(b"(").is_none());
        assert!(compile_pattern(b"a+").is_some());
    }
}
