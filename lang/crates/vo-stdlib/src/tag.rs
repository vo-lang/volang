//! Struct tag parsing utilities.
//!
//! Parses Go-style struct tags like `json:"name,omitempty" toml:"other"`.

/// Get the value for a specific key from a struct tag.
///
/// Tag format: `key:"value" key2:"value2"`
///
/// Returns None if the key is not found.
pub fn get_tag_value<'a>(tag: &'a str, key: &str) -> Option<&'a str> {
    vo_common_core::lookup_struct_tag_value(tag, key)
}

/// Parse field options from a tag value like "name,omitempty".
///
/// Returns (field_name, omitempty).
/// If field_name is empty, the original field name should be used.
/// If field_name is "-", the field should be skipped.
pub fn parse_field_options(value: &str) -> (&str, bool) {
    let mut parts = value.split(',');
    let name = parts.next().unwrap_or("");
    let omitempty = parts.any(|p| p == "omitempty");
    (name, omitempty)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_tag_value() {
        assert_eq!(get_tag_value(r#"json:"name""#, "json"), Some("name"));
        assert_eq!(
            get_tag_value(r#"json:"name" toml:"other""#, "json"),
            Some("name")
        );
        assert_eq!(
            get_tag_value(r#"json:"name" toml:"other""#, "toml"),
            Some("other")
        );
        assert_eq!(get_tag_value(r#"json:"name""#, "toml"), None);
        assert_eq!(get_tag_value(r#"json:"-""#, "json"), Some("-"));
        assert_eq!(
            get_tag_value(r#"json:"name,omitempty""#, "json"),
            Some("name,omitempty")
        );
        assert_eq!(
            get_tag_value("json:\"x\"\u{00a0}dyn:\"hidden\"", "dyn"),
            None
        );
        assert_eq!(get_tag_value(r#"json:x dyn:"hidden""#, "dyn"), None);
        assert_eq!(get_tag_value(r#"json:"unterminated"#, "json"), None);
    }

    #[test]
    fn test_parse_field_options() {
        assert_eq!(parse_field_options("name"), ("name", false));
        assert_eq!(parse_field_options("name,omitempty"), ("name", true));
        assert_eq!(parse_field_options(",omitempty"), ("", true));
        assert_eq!(parse_field_options("-"), ("-", false));
    }
}
