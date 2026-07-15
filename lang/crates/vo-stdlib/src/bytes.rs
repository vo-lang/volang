//! bytes package native function implementations.
//!
//! Native functions for bytes that benefit from Rust's optimized implementations:
//! - Index/LastIndex: Fast byte pattern search
//! - ToLower/ToUpper/ToTitle/EqualFold: Unicode-aware byte processing
//! - Split/Fields: Memory allocation optimization
//! - Replace: Complex byte manipulation

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use crate::raw_utf8;
use memchr::memmem;
use vo_ffi_macro::vostd_fn;
use vo_runtime::ffi::{ExternCallContext, ExternResult};

// ==================== Search ====================

#[vostd_fn("bytes", "Index")]
fn index(s: &[u8], sep: &[u8]) -> i64 {
    if sep.is_empty() {
        return 0;
    }
    memmem::find(s, sep).map(|i| i as i64).unwrap_or(-1)
}

#[vostd_fn("bytes", "LastIndex")]
fn last_index(s: &[u8], sep: &[u8]) -> i64 {
    if sep.is_empty() {
        return s.len() as i64;
    }
    memmem::rfind(s, sep).map(|i| i as i64).unwrap_or(-1)
}

#[vostd_fn("bytes", "Count")]
fn count(s: &[u8], sep: &[u8]) -> i64 {
    if sep.is_empty() {
        return i64::try_from(raw_utf8::rune_count(s))
            .ok()
            .and_then(|count| count.checked_add(1))
            .expect("bytes.Count result exceeds the language int range");
    }
    i64::try_from(memmem::find_iter(s, sep).count())
        .expect("bytes.Count result exceeds the language int range")
}

// ==================== Case conversion ====================

#[vostd_fn("bytes", "ToLower")]
fn to_lower(s: &[u8]) -> Vec<u8> {
    raw_utf8::map_runes(s, crate::unicode::to_lower_char)
}

#[vostd_fn("bytes", "ToUpper")]
fn to_upper(s: &[u8]) -> Vec<u8> {
    raw_utf8::map_runes(s, crate::unicode::to_upper_char)
}

#[vostd_fn("bytes", "ToTitle")]
fn to_title(s: &[u8]) -> Vec<u8> {
    raw_utf8::map_runes(s, crate::unicode::to_title_char)
}

#[vostd_fn("bytes", "EqualFold")]
fn equal_fold(s: &[u8], t: &[u8]) -> bool {
    raw_utf8::equal_fold(s, t)
}

// ==================== Replace ====================

#[vostd_fn("bytes", "Replace")]
fn replace(call: &mut ExternCallContext) -> ExternResult {
    let s = call.arg_bytes(slots::ARG_S);
    let old = call.arg_bytes(slots::ARG_OLD);
    let new = call.arg_bytes(slots::ARG_NEW);
    let n = call.arg_i64(slots::ARG_N);
    let result = raw_utf8::replace(s, old, new, n);

    // The no-match copy path starts from a nil slice, so an empty input stays
    // nil. An empty old pattern with n != 0 performs a real replacement and
    // returns an allocated (possibly empty) slice.
    if s.is_empty() && (n == 0 || !old.is_empty()) {
        call.ret_nil(slots::RET_0);
    } else {
        call.ret_bytes(slots::RET_0, &result);
    }
    ExternResult::Ok
}

vo_ffi_macro::vostd_register!("bytes":
    Index, LastIndex, Count,
    ToLower, ToUpper, ToTitle,
    EqualFold, Replace,
);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_pattern_count_uses_utf8_sequence_boundaries() {
        assert_eq!(count("A世".as_bytes(), b""), 3);
        assert_eq!(count(&[0xFF, 0xFE], b""), 3);
    }
}
