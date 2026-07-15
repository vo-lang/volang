//! Unicode package native function implementations.
//!
//! All package semantics are pinned to Unicode 16.0. Classification and case
//! conversion use ICU4X 2.0 data; simple folding uses regex-syntax 0.8.10, the
//! same tables as package regexp. Keep those dependency pins, `unicode.Version`,
//! and the cross-package language regression in sync when upgrading Unicode.

use icu_casemap::CaseMapper;
use icu_properties::props::{GeneralCategory, GeneralCategoryGroup, Uppercase, WhiteSpace};
use icu_properties::{CodePointMapData, CodePointSetData};
use regex_syntax::hir::{ClassUnicode, ClassUnicodeRange};
use vo_ffi_macro::vostd_fn;

const PRINTABLE_CATEGORIES: GeneralCategoryGroup = GeneralCategoryGroup::Letter
    .union(GeneralCategoryGroup::Mark)
    .union(GeneralCategoryGroup::Number)
    .union(GeneralCategoryGroup::Punctuation)
    .union(GeneralCategoryGroup::Symbol);

const GRAPHIC_CATEGORIES: GeneralCategoryGroup =
    PRINTABLE_CATEGORIES.union(GeneralCategoryGroup::SpaceSeparator);

fn rune_char(r: i32) -> Option<char> {
    u32::try_from(r).ok().and_then(char::from_u32)
}

fn category(c: char) -> GeneralCategory {
    CodePointMapData::<GeneralCategory>::new().get(c)
}

fn rune_has_category(r: i32, categories: GeneralCategoryGroup) -> bool {
    rune_char(r).is_some_and(|c| categories.contains(category(c)))
}

fn map_rune(r: i32, map: fn(char) -> char) -> i32 {
    rune_char(r).map_or(r, |c| map(c) as i32)
}

// ==================== Character classification ====================

#[vostd_fn("unicode", "IsLetter")]
fn is_letter(r: i32) -> bool {
    rune_has_category(r, GeneralCategoryGroup::Letter)
}

#[vostd_fn("unicode", "IsDigit")]
fn is_digit(r: i32) -> bool {
    rune_has_category(r, GeneralCategoryGroup::DecimalNumber)
}

pub(crate) fn is_space_char(c: char) -> bool {
    CodePointSetData::new::<WhiteSpace>().contains(c)
}

#[vostd_fn("unicode", "IsSpace")]
fn is_space(r: i32) -> bool {
    rune_char(r).is_some_and(is_space_char)
}

#[vostd_fn("unicode", "IsUpper")]
fn is_upper(r: i32) -> bool {
    rune_has_category(r, GeneralCategoryGroup::UppercaseLetter)
}

#[vostd_fn("unicode", "IsLower")]
fn is_lower(r: i32) -> bool {
    rune_has_category(r, GeneralCategoryGroup::LowercaseLetter)
}

#[vostd_fn("unicode", "IsTitle")]
fn is_title(r: i32) -> bool {
    rune_has_category(r, GeneralCategoryGroup::TitlecaseLetter)
}

#[vostd_fn("unicode", "IsControl")]
fn is_control(r: i32) -> bool {
    rune_has_category(r, GeneralCategoryGroup::Control)
}

#[vostd_fn("unicode", "IsPrint")]
pub(crate) fn is_print(r: i32) -> bool {
    rune_char(r).is_some_and(|c| c == ' ' || PRINTABLE_CATEGORIES.contains(category(c)))
}

#[vostd_fn("unicode", "IsPunct")]
fn is_punct(r: i32) -> bool {
    rune_has_category(r, GeneralCategoryGroup::Punctuation)
}

#[vostd_fn("unicode", "IsGraphic")]
fn is_graphic(r: i32) -> bool {
    rune_has_category(r, GRAPHIC_CATEGORIES)
}

#[vostd_fn("unicode", "IsNumber")]
fn is_number(r: i32) -> bool {
    rune_has_category(r, GeneralCategoryGroup::Number)
}

#[vostd_fn("unicode", "IsMark")]
fn is_mark(r: i32) -> bool {
    rune_has_category(r, GeneralCategoryGroup::Mark)
}

#[vostd_fn("unicode", "IsSymbol")]
fn is_symbol(r: i32) -> bool {
    rune_has_category(r, GeneralCategoryGroup::Symbol)
}

// ==================== Case conversion ====================

pub(crate) fn to_lower_char(c: char) -> char {
    CaseMapper::new().simple_lowercase(c)
}

/// Reports the language-spec exported-identifier predicate. This deliberately
/// uses Unicode's binary `Uppercase` property, which also includes characters
/// such as U+2160 outside General_Category=Lu.
pub(crate) fn is_exported_char(c: char) -> bool {
    CodePointSetData::new::<Uppercase>().contains(c)
}

pub(crate) fn to_upper_char(c: char) -> char {
    CaseMapper::new().simple_uppercase(c)
}

pub(crate) fn to_title_char(c: char) -> char {
    CaseMapper::new().simple_titlecase(c)
}

#[vostd_fn("unicode", "ToLower")]
fn to_lower(r: i32) -> i32 {
    map_rune(r, to_lower_char)
}

#[vostd_fn("unicode", "ToUpper")]
fn to_upper(r: i32) -> i32 {
    map_rune(r, to_upper_char)
}

#[vostd_fn("unicode", "ToTitle")]
fn to_title(r: i32) -> i32 {
    map_rune(r, to_title_char)
}

/// Returns the next scalar value in `c`'s Unicode simple-fold orbit.
///
/// The HIR class API closes the singleton under Unicode simple folding and
/// keeps the resulting ranges in scalar-value order. This directly implements
/// the `unicode.SimpleFold` successor rule: choose the least member greater
/// than the input, wrapping to the least member when necessary.
pub(crate) fn simple_fold_char(c: char) -> char {
    // Keep the overwhelmingly common ASCII path allocation-free. K/k and
    // S/s have one additional non-ASCII orbit member under default folding.
    match c {
        'A'..='Z' => return char::from_u32(u32::from(c) + 0x20).unwrap_or(c),
        'a'..='z' => {
            return match c {
                'k' => '\u{212A}',
                's' => '\u{017F}',
                _ => char::from_u32(u32::from(c) - 0x20).unwrap_or(c),
            };
        }
        '\0'..='\u{7F}' => return c,
        _ => {}
    }

    let mut equivalents = ClassUnicode::new([ClassUnicodeRange::new(c, c)]);
    if equivalents.try_case_fold_simple().is_err() {
        return c;
    }

    let mut first = None;
    for range in equivalents.iter() {
        first.get_or_insert(range.start());
        if c < range.start() {
            return range.start();
        }
        if c < range.end() && range.start() <= c {
            return char::from_u32(u32::from(c) + 1).unwrap_or(c);
        }
    }
    first.unwrap_or(c)
}

#[vostd_fn("unicode", "SimpleFold")]
fn simple_fold(r: i32) -> i32 {
    map_rune(r, simple_fold_char)
}

vo_ffi_macro::vostd_register!("unicode":
    IsLetter, IsDigit, IsSpace, IsUpper, IsLower, IsTitle, IsControl,
    IsPrint, IsPunct, IsGraphic, IsNumber, IsMark, IsSymbol,
    ToLower, ToUpper, ToTitle, SimpleFold,
);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn classifications_follow_unicode_properties() {
        assert!(is_letter('木' as i32));
        assert!(is_letter('ʰ' as i32));
        assert!(!is_letter('\u{0301}' as i32));

        assert!(is_digit('٥' as i32));
        assert!(!is_digit('²' as i32));
        assert!(is_number('²' as i32));
        assert!(is_number('Ⅷ' as i32));

        for c in [
            ' ', '\t', '\n', '\u{0085}', '\u{00A0}', '\u{1680}', '\u{2028}',
        ] {
            assert!(
                is_space(c as i32),
                "expected U+{:04X} to be space",
                c as u32
            );
        }
        assert!(!is_space('\u{200B}' as i32));

        assert!(is_upper('Ǆ' as i32));
        assert!(is_title('ǅ' as i32));
        assert!(is_lower('ǆ' as i32));
        assert!(!is_upper('ǅ' as i32));
        assert!(!is_lower('ǅ' as i32));
        assert!(!is_upper('Ⅰ' as i32));
        assert!(is_exported_char('Ⅰ'));
        assert!(!is_exported_char('ⅰ'));

        assert!(is_control('\u{0085}' as i32));
        assert!(!is_control('\u{200D}' as i32));
        assert!(is_mark('\u{20DD}' as i32));
        assert!(is_punct('\u{2E4F}' as i32));
        assert!(is_symbol('🫨' as i32));
    }

    #[test]
    fn print_and_graphic_have_the_go_spacing_contract() {
        for c in ['A', '\u{0301}', '²', '。', '✓', '\u{FFFD}', ' '] {
            assert!(
                is_print(c as i32),
                "expected U+{:04X} to be printable",
                c as u32
            );
            assert!(
                is_graphic(c as i32),
                "expected U+{:04X} to be graphic",
                c as u32
            );
        }
        assert!(!is_print('\u{00A0}' as i32));
        assert!(is_graphic('\u{00A0}' as i32));
        assert!(!is_print('\n' as i32));
        assert!(!is_graphic('\n' as i32));
        assert!(!is_print('\u{E000}' as i32));
        assert!(!is_graphic('\u{E000}' as i32));
    }

    #[test]
    fn simple_case_mapping_never_expands_a_rune() {
        assert_eq!(to_lower('İ' as i32), 'i' as i32);
        assert_eq!(to_upper('ı' as i32), 'I' as i32);
        assert_eq!(to_upper('ß' as i32), 'ß' as i32);
        assert_eq!(to_lower('ẞ' as i32), 'ß' as i32);
        assert_eq!(to_upper('ΐ' as i32), 'ΐ' as i32);
        assert_eq!(to_upper('ﬀ' as i32), 'ﬀ' as i32);

        for (input, upper, lower, title) in [
            ('Ǆ', 'Ǆ', 'ǆ', 'ǅ'),
            ('ǅ', 'Ǆ', 'ǆ', 'ǅ'),
            ('ǆ', 'Ǆ', 'ǆ', 'ǅ'),
        ] {
            assert_eq!(to_upper(input as i32), upper as i32);
            assert_eq!(to_lower(input as i32), lower as i32);
            assert_eq!(to_title(input as i32), title as i32);
        }
    }

    #[test]
    fn simple_fold_returns_the_next_sorted_orbit_member() {
        for orbit in [
            &['K', 'k', 'K'][..],
            &['S', 's', 'ſ'][..],
            &['ß', 'ẞ'][..],
            &['Ǆ', 'ǅ', 'ǆ'][..],
        ] {
            for (index, &c) in orbit.iter().enumerate() {
                assert_eq!(simple_fold_char(c), orbit[(index + 1) % orbit.len()]);
            }
        }
        assert_eq!(simple_fold_char('İ'), 'İ');
        assert_eq!(simple_fold_char('ı'), 'ı');
        assert_eq!(simple_fold_char('1'), '1');
    }

    #[test]
    fn invalid_runes_are_rejected_or_preserved() {
        for r in [-1, 0xD800, 0xDFFF, 0x11_0000] {
            assert!(!is_letter(r));
            assert!(!is_space(r));
            assert!(!is_print(r));
            assert_eq!(to_lower(r), r);
            assert_eq!(to_upper(r), r);
            assert_eq!(to_title(r), r);
            assert_eq!(simple_fold(r), r);
        }
    }
}
