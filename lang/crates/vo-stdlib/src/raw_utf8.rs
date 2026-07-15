//! Shared arbitrary-byte UTF-8 semantics for Go-compatible string and byte APIs.

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

pub(crate) const REPLACEMENT_CHARACTER: char = '\u{FFFD}';

/// Decode the first UTF-8 sequence with Go's `utf8.DecodeRune` recovery rule:
/// every malformed or truncated encoding consumes exactly one byte.
pub(crate) fn decode_first(s: &[u8]) -> (char, usize) {
    if s.is_empty() {
        return (REPLACEMENT_CHARACTER, 0);
    }
    if s[0].is_ascii() {
        return (char::from(s[0]), 1);
    }

    let width = match s[0] {
        0xC2..=0xDF => 2,
        0xE0..=0xEF => 3,
        0xF0..=0xF4 => 4,
        _ => return (REPLACEMENT_CHARACTER, 1),
    };
    let Some(prefix) = s.get(..width) else {
        return (REPLACEMENT_CHARACTER, 1);
    };
    match core::str::from_utf8(prefix)
        .ok()
        .and_then(|valid| valid.chars().next())
    {
        Some(rune) => (rune, width),
        None => (REPLACEMENT_CHARACTER, 1),
    }
}

pub(crate) fn rune_count(s: &[u8]) -> usize {
    let mut count = 0;
    let mut offset = 0;
    while offset < s.len() {
        let (_, width) = decode_first(&s[offset..]);
        offset += width;
        count += 1;
    }
    count
}

pub(crate) fn map_runes(s: &[u8], mapping: fn(char) -> char) -> Vec<u8> {
    let mut result = Vec::with_capacity(s.len());
    let mut offset = 0;
    while offset < s.len() {
        let (rune, width) = decode_first(&s[offset..]);
        let mapped = mapping(rune);
        let mut encoded = [0_u8; 4];
        result.extend_from_slice(mapped.encode_utf8(&mut encoded).as_bytes());
        offset += width;
    }
    result
}

pub(crate) fn equal_fold(s: &[u8], t: &[u8]) -> bool {
    let mut s_offset = 0;
    let mut t_offset = 0;

    while s_offset < s.len() && t_offset < t.len() {
        let (s_rune, s_width) = decode_first(&s[s_offset..]);
        let (t_rune, t_width) = decode_first(&t[t_offset..]);
        if s_rune != t_rune {
            let mut folded = crate::unicode::simple_fold_char(s_rune);
            let mut equivalent = false;
            while folded != s_rune {
                if folded == t_rune {
                    equivalent = true;
                    break;
                }
                folded = crate::unicode::simple_fold_char(folded);
            }
            if !equivalent {
                return false;
            }
        }
        s_offset += s_width;
        t_offset += t_width;
    }

    s_offset == s.len() && t_offset == t.len()
}

pub(crate) fn replace(s: &[u8], old: &[u8], new: &[u8], n: i64) -> Vec<u8> {
    if n == 0 {
        return s.to_vec();
    }

    if old.is_empty() {
        let limit = if n < 0 {
            usize::MAX
        } else {
            usize::try_from(n).unwrap_or(usize::MAX)
        };
        let mut result = Vec::new();
        result.extend_from_slice(new);
        let mut replacements = 1_usize;
        let mut offset = 0;
        while offset < s.len() && replacements < limit {
            let (_, width) = decode_first(&s[offset..]);
            result.extend_from_slice(&s[offset..offset + width]);
            result.extend_from_slice(new);
            offset += width;
            replacements += 1;
        }
        result.extend_from_slice(&s[offset..]);
        return result;
    }

    let mut result = Vec::new();
    let mut start = 0;
    let mut replacements = 0_i64;
    while start <= s.len() {
        if n >= 0 && replacements >= n {
            result.extend_from_slice(&s[start..]);
            break;
        }
        if let Some(pos) = memchr::memmem::find(&s[start..], old) {
            result.extend_from_slice(&s[start..start + pos]);
            result.extend_from_slice(new);
            start += pos + old.len();
            replacements += 1;
        } else {
            result.extend_from_slice(&s[start..]);
            break;
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn malformed_sequences_consume_one_byte() {
        assert_eq!(decode_first(b"A\xFF"), ('A', 1));
        assert_eq!(decode_first(&[0xE2, 0x82]), (REPLACEMENT_CHARACTER, 1));
        assert_eq!(
            decode_first(&[0xE2, b'A', 0xAC]),
            (REPLACEMENT_CHARACTER, 1)
        );
        assert_eq!(decode_first("€tail".as_bytes()), ('€', 3));
        assert_eq!(
            map_runes(&[0xE2, 0x82], core::convert::identity),
            "\u{FFFD}\u{FFFD}".as_bytes()
        );
    }

    #[test]
    fn empty_pattern_operations_use_rune_boundaries() {
        assert_eq!(rune_count("A世".as_bytes()), 2);
        assert_eq!(rune_count(&[0xFF, 0xFE]), 2);
        assert_eq!(
            replace("A世".as_bytes(), b"", b"-", -1),
            "-A-世-".as_bytes()
        );
        assert_eq!(replace("A世".as_bytes(), b"", b"-", 2), "-A-世".as_bytes());
    }
}
