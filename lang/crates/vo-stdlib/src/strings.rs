//! strings package native function implementations.
//!
//! Native functions for strings that benefit from Rust's optimized implementations:
//! - Index/LastIndex: Boyer-Moore-like search
//! - ToLower/ToUpper/ToTitle: Unicode case conversion
//! - Split/Fields: Memory allocation optimization
//! - Replace: Complex string manipulation
//! - EqualFold: Unicode case folding

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use crate::raw_utf8;
use memchr::memmem;
use vo_ffi_macro::vostd_fn;
use vo_runtime::ffi::{ExternCallContext, ExternResult};

// ==================== Search ====================

#[vostd_fn("strings", "Index")]
fn index(call: &mut ExternCallContext) -> ExternResult {
    let s = call.arg_string_bytes(slots::ARG_S);
    let substr = call.arg_string_bytes(slots::ARG_SUBSTR);
    let result = if substr.is_empty() {
        0
    } else {
        memmem::find(&s, &substr)
            .map(|offset| offset as i64)
            .unwrap_or(-1)
    };
    call.ret_i64(slots::RET_0, result);
    ExternResult::Ok
}

#[vostd_fn("strings", "LastIndex")]
fn last_index(call: &mut ExternCallContext) -> ExternResult {
    let s = call.arg_string_bytes(slots::ARG_S);
    let substr = call.arg_string_bytes(slots::ARG_SUBSTR);
    let result = if substr.is_empty() {
        s.len() as i64
    } else {
        memmem::rfind(&s, &substr)
            .map(|offset| offset as i64)
            .unwrap_or(-1)
    };
    call.ret_i64(slots::RET_0, result);
    ExternResult::Ok
}

#[vostd_fn("strings", "Count")]
fn count(call: &mut ExternCallContext) -> ExternResult {
    let s = call.arg_string_bytes(slots::ARG_S);
    let substr = call.arg_string_bytes(slots::ARG_SUBSTR);
    if substr.is_empty() {
        let result = i64::try_from(raw_utf8::rune_count(&s))
            .ok()
            .and_then(|count| count.checked_add(1))
            .expect("strings.Count result exceeds the language int range");
        call.ret_i64(slots::RET_0, result);
        return ExternResult::Ok;
    }
    let matches = i64::try_from(memmem::find_iter(&s, &substr).count())
        .expect("strings.Count result exceeds the language int range");
    call.ret_i64(slots::RET_0, matches);
    ExternResult::Ok
}

// ==================== Case conversion ====================

#[vostd_fn("strings", "ToLower")]
fn to_lower(call: &mut ExternCallContext) -> ExternResult {
    let result = raw_utf8::map_runes(
        &call.arg_string_bytes(slots::ARG_S),
        crate::unicode::to_lower_char,
    );
    call.ret_string_bytes(slots::RET_0, &result);
    ExternResult::Ok
}

#[vostd_fn("strings", "ToUpper")]
fn to_upper(call: &mut ExternCallContext) -> ExternResult {
    let result = raw_utf8::map_runes(
        &call.arg_string_bytes(slots::ARG_S),
        crate::unicode::to_upper_char,
    );
    call.ret_string_bytes(slots::RET_0, &result);
    ExternResult::Ok
}

#[vostd_fn("strings", "ToTitle")]
fn to_title(call: &mut ExternCallContext) -> ExternResult {
    let result = raw_utf8::map_runes(
        &call.arg_string_bytes(slots::ARG_S),
        crate::unicode::to_title_char,
    );
    call.ret_string_bytes(slots::RET_0, &result);
    ExternResult::Ok
}

// ==================== Splitting ====================

fn positive_limit(n: i64) -> usize {
    debug_assert!(n > 0);
    usize::try_from(n).unwrap_or(usize::MAX)
}

fn explode(s: &[u8], n: i64) -> Vec<Vec<u8>> {
    let rune_count = raw_utf8::rune_count(s);
    let limit = if n < 0 {
        rune_count
    } else {
        positive_limit(n).min(rune_count)
    };
    if limit == 0 {
        return Vec::new();
    }

    let mut result = Vec::with_capacity(limit);
    let mut start = 0;
    for _ in 0..limit - 1 {
        let (_, width) = raw_utf8::decode_first(&s[start..]);
        let end = start + width;
        result.push(s[start..end].to_vec());
        start = end;
    }
    result.push(s[start..].to_vec());
    result
}

fn split_impl(s: &[u8], sep: &[u8], keep_separator: bool, n: i64) -> Vec<Vec<u8>> {
    if n == 0 {
        return Vec::new();
    }
    if sep.is_empty() {
        return explode(s, n);
    }

    let max_parts = if n < 0 { usize::MAX } else { positive_limit(n) };
    let mut result = Vec::new();
    let mut rest = s;
    while result.len().saturating_add(1) < max_parts {
        let Some(offset) = memmem::find(rest, sep) else {
            break;
        };
        let split_at = offset + sep.len();
        let result_end = if keep_separator { split_at } else { offset };
        result.push(rest[..result_end].to_vec());
        rest = &rest[split_at..];
    }
    result.push(rest.to_vec());
    result
}

#[vostd_fn("strings", "Split")]
fn split(call: &mut ExternCallContext) -> ExternResult {
    let result = split_impl(
        &call.arg_string_bytes(slots::ARG_S),
        &call.arg_string_bytes(slots::ARG_SEP),
        false,
        -1,
    );
    call.ret_string_bytes_slice(slots::RET_0, &result);
    ExternResult::Ok
}

#[vostd_fn("strings", "SplitN")]
fn split_n(call: &mut ExternCallContext) -> ExternResult {
    let n = call.arg_i64(slots::ARG_N);
    let result = split_impl(
        &call.arg_string_bytes(slots::ARG_S),
        &call.arg_string_bytes(slots::ARG_SEP),
        false,
        n,
    );
    if n == 0 {
        call.ret_nil(slots::RET_0);
    } else {
        call.ret_string_bytes_slice(slots::RET_0, &result);
    }
    ExternResult::Ok
}

#[vostd_fn("strings", "SplitAfter")]
fn split_after(call: &mut ExternCallContext) -> ExternResult {
    let result = split_impl(
        &call.arg_string_bytes(slots::ARG_S),
        &call.arg_string_bytes(slots::ARG_SEP),
        true,
        -1,
    );
    call.ret_string_bytes_slice(slots::RET_0, &result);
    ExternResult::Ok
}

#[vostd_fn("strings", "SplitAfterN")]
fn split_after_n(call: &mut ExternCallContext) -> ExternResult {
    let n = call.arg_i64(slots::ARG_N);
    let result = split_impl(
        &call.arg_string_bytes(slots::ARG_S),
        &call.arg_string_bytes(slots::ARG_SEP),
        true,
        n,
    );
    if n == 0 {
        call.ret_nil(slots::RET_0);
    } else {
        call.ret_string_bytes_slice(slots::RET_0, &result);
    }
    ExternResult::Ok
}

#[vostd_fn("strings", "Fields")]
fn fields(call: &mut ExternCallContext) -> ExternResult {
    let s = call.arg_string_bytes(slots::ARG_S);
    let mut result = Vec::new();
    let mut field_start = None;
    let mut offset = 0;
    while offset < s.len() {
        let (rune, width) = raw_utf8::decode_first(&s[offset..]);
        if crate::unicode::is_space_char(rune) {
            if let Some(start) = field_start.take() {
                result.push(s[start..offset].to_vec());
            }
        } else if field_start.is_none() {
            field_start = Some(offset);
        }
        offset += width;
    }
    if let Some(start) = field_start {
        result.push(s[start..].to_vec());
    }
    call.ret_string_bytes_slice(slots::RET_0, &result);
    ExternResult::Ok
}

// ==================== Replace ====================

#[vostd_fn("strings", "Replace")]
fn replace(call: &mut ExternCallContext) -> ExternResult {
    let result = raw_utf8::replace(
        &call.arg_string_bytes(slots::ARG_S),
        &call.arg_string_bytes(slots::ARG_OLD),
        &call.arg_string_bytes(slots::ARG_NEW),
        call.arg_i64(slots::ARG_N),
    );
    call.ret_string_bytes(slots::RET_0, &result);
    ExternResult::Ok
}

// ==================== Comparison ====================

#[vostd_fn("strings", "EqualFold")]
fn equal_fold(call: &mut ExternCallContext) -> ExternResult {
    let result = raw_utf8::equal_fold(
        &call.arg_string_bytes(slots::ARG_S),
        &call.arg_string_bytes(slots::ARG_T),
    );
    call.ret_bool(slots::RET_0, result);
    ExternResult::Ok
}

vo_ffi_macro::vostd_register!("strings":
    Index, LastIndex, Count, EqualFold,
    ToLower, ToUpper, ToTitle,
    Split, SplitN, SplitAfter, SplitAfterN,
    Fields, Replace,
);

#[cfg(test)]
mod tests {
    use super::{positive_limit, split_impl};
    use crate::raw_utf8;

    #[test]
    fn oversized_limits_never_wrap_to_smaller_operations() {
        let oversized = i64::MAX;
        assert_eq!(
            positive_limit(oversized),
            usize::try_from(oversized).unwrap_or(usize::MAX)
        );
        assert_eq!(
            split_impl(b"a,b", b",", false, oversized),
            [b"a".to_vec(), b"b".to_vec()]
        );
        assert_eq!(
            split_impl(b"a,b", b",", true, oversized),
            [b"a,".to_vec(), b"b".to_vec()]
        );
        assert_eq!(raw_utf8::replace(b"aaa", b"a", b"b", oversized), b"bbb");
    }

    #[test]
    fn split_preserves_remainder_and_empty_fields() {
        assert_eq!(
            split_impl("é🙂z".as_bytes(), b"", false, 2),
            ["é".as_bytes().to_vec(), "🙂z".as_bytes().to_vec()]
        );
        assert_eq!(
            split_impl(b"a,", b",", false, -1),
            [b"a".to_vec(), Vec::new()]
        );
        assert_eq!(
            split_impl(b"a,", b",", true, -1),
            [b"a,".to_vec(), Vec::new()]
        );
        assert_eq!(split_impl(b"", b",", false, -1), [Vec::<u8>::new()]);
        assert!(split_impl(b"anything", b",", false, 0).is_empty());
        assert_eq!(
            split_impl(&[0xFF, b'A'], b"", false, -1),
            [vec![0xFF], vec![b'A']]
        );
    }

    #[test]
    fn equal_fold_uses_unicode_simple_case_folding() {
        assert!(raw_utf8::equal_fold("KΣς".as_bytes(), "KσΣ".as_bytes()));
        assert!(raw_utf8::equal_fold("ſ".as_bytes(), b"S"));
        assert!(!raw_utf8::equal_fold("ß".as_bytes(), b"ss"));
        assert!(!raw_utf8::equal_fold(b"hello", b"world"));
        assert!(raw_utf8::equal_fold(&[0xFF], &[0xFE]));
    }
}
