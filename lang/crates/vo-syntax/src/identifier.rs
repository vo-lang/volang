//! Parser-facing re-exports of the language's shared Unicode name profile.
//!
//! The implementation belongs to `vo-common-core` so source parsing,
//! bytecode verification, and provider validation cannot drift.

pub use vo_common::identifier::{is_exported_name, is_identifier_continue, is_identifier_start};

#[cfg(test)]
mod tests {
    use super::*;
    use vo_common::identifier::{is_identifier, is_named_declaration_identifier};

    #[test]
    fn identifier_properties_are_pinned_to_the_language_unicode_profile() {
        assert!(is_identifier_start('A'));
        assert!(is_identifier_start('变'));
        assert!(!is_identifier_start('\u{1e6c0}'));

        assert!(is_identifier_continue('２'));
        assert!(!is_identifier_continue('²'));
        assert!(is_identifier("变量２"));
        assert!(!is_named_declaration_identifier("func"));
    }

    #[test]
    fn exported_names_use_the_unicode_uppercase_property() {
        assert!(is_exported_name("A"));
        assert!(is_exported_name("Ⅰtem"));
        assert!(!is_exported_name("ⅰtem"));
        assert!(!is_exported_name("_Hidden"));
        assert!(!is_exported_name(""));
    }
}
