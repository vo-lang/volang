//! Deterministic parsing for Go-style structure-field tags.
//!
//! Volang field tags use ASCII spaces between `key:"value"` entries. Keeping
//! the parser here gives the runtime reflection and standard-library encoders
//! one grammar in both `std` and `no_std` builds.

/// Return the raw quoted value associated with `key` in a Go-style field tag.
///
/// Entries have the form `key:"value"` and may be separated by ASCII spaces.
/// Backslash-escaped bytes do not terminate the quoted value. Malformed input
/// stops parsing, so a later entry cannot become visible through a corrupted
/// prefix. The returned slice retains escape sequences exactly as written.
pub fn lookup_struct_tag_value<'a>(mut tag: &'a str, key: &str) -> Option<&'a str> {
    loop {
        tag = tag.trim_start_matches(' ');
        if tag.is_empty() {
            return None;
        }

        let bytes = tag.as_bytes();
        let mut key_end = 0usize;
        while key_end < bytes.len()
            && bytes[key_end] > b' '
            && bytes[key_end] != b':'
            && bytes[key_end] != b'"'
            && bytes[key_end] != 0x7f
        {
            key_end += 1;
        }
        if key_end == 0
            || key_end + 1 >= bytes.len()
            || bytes[key_end] != b':'
            || bytes[key_end + 1] != b'"'
        {
            return None;
        }

        let current_key = &tag[..key_end];
        let value_start = key_end + 2;
        let mut cursor = value_start;
        loop {
            let byte = *bytes.get(cursor)?;
            match byte {
                b'"' => {
                    let value = &tag[value_start..cursor];
                    tag = &tag[cursor + 1..];
                    if current_key == key {
                        return Some(value);
                    }
                    break;
                }
                b'\\' => {
                    bytes.get(cursor + 1)?;
                    cursor += 2;
                }
                _ => cursor += 1,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::lookup_struct_tag_value;

    #[test]
    fn finds_exact_keys_and_preserves_escaped_values() {
        let tag = r#"json:"name" dyn:"say\"hi""#;
        assert_eq!(lookup_struct_tag_value(tag, "json"), Some("name"));
        assert_eq!(lookup_struct_tag_value(tag, "dyn"), Some(r#"say\"hi"#));
        assert_eq!(lookup_struct_tag_value(tag, "xml"), None);
    }

    #[test]
    fn accepts_only_ascii_space_as_an_entry_separator() {
        assert_eq!(
            lookup_struct_tag_value(r#"json:"x"  dyn:"visible""#, "dyn"),
            Some("visible")
        );
        for separator in ['\t', '\n', '\u{0085}', '\u{00a0}'] {
            let tag = format!(r#"json:"x"{separator}dyn:"hidden""#);
            assert_eq!(lookup_struct_tag_value(&tag, "dyn"), None);
        }
    }

    #[test]
    fn malformed_prefixes_cannot_reveal_later_entries() {
        for tag in [
            r#"json:x dyn:"hidden""#,
            r#"json:"unterminated dyn:"hidden""#,
            "json:\"x\" \u{7f}dyn:\"hidden\"",
        ] {
            assert_eq!(lookup_struct_tag_value(tag, "dyn"), None);
        }
    }
}
