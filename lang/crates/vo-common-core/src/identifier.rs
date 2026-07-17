//! Unicode properties and reserved words shared by source and bytecode names.
//!
//! These predicates live below the parser so untrusted bytecode and provider
//! tables cannot accept a function identity that source text could never
//! declare. Exact ICU pins keep the language profile independent of the Rust
//! toolchain's Unicode version.

#[cfg(not(feature = "std"))]
use alloc::{string::String, vec::Vec};
#[cfg(feature = "std")]
use std::{string::String, vec::Vec};

use core::fmt;
use icu_casemap::CaseMapperBorrowed;
use icu_normalizer::ComposingNormalizerBorrowed;
use icu_properties::props::{Alnum, Alphabetic, GeneralCategory, Uppercase, WhiteSpace};
use icu_properties::{CodePointMapDataBorrowed, CodePointSetData, CodePointSetDataBorrowed};

/// Unicode profile shared by the lexer, extern identities, and generated JS validators.
pub const UNICODE_PROFILE_VERSION: &str = "16.0.0";

const ALPHABETIC: CodePointSetDataBorrowed<'static> = CodePointSetData::new::<Alphabetic>();
const ALNUM: CodePointSetDataBorrowed<'static> = CodePointSetData::new::<Alnum>();
const UPPERCASE: CodePointSetDataBorrowed<'static> = CodePointSetData::new::<Uppercase>();
const WHITE_SPACE: CodePointSetDataBorrowed<'static> = CodePointSetData::new::<WhiteSpace>();
const GENERAL_CATEGORY: CodePointMapDataBorrowed<'static, GeneralCategory> =
    CodePointMapDataBorrowed::new();

/// Complete reserved-word set for the current Vo language grammar.
pub const VO_KEYWORDS: &[&str] = &[
    "break",
    "case",
    "chan",
    "const",
    "continue",
    "default",
    "defer",
    "else",
    "errdefer",
    "fail",
    "fallthrough",
    "for",
    "func",
    "go",
    "goto",
    "if",
    "import",
    "interface",
    "island",
    "map",
    "package",
    "port",
    "range",
    "return",
    "select",
    "struct",
    "switch",
    "type",
    "var",
];

/// Compiler-owned namespace separating function-local named types from
/// package-level source identities.
///
/// `@` cannot occur in a canonical source package path or a Vo identifier, so
/// source text cannot manufacture an identity in this namespace.
pub const LOCAL_TYPE_IDENTITY_MARKER: &str = "@local:";

/// Maximum byte length of any bytecode-visible named type identity.
///
/// This accommodates the full canonical package path, the injective local
/// discriminator, and ordinary source declarations while bounding verifier
/// work and serialized metadata growth.
pub const MAX_NAMED_TYPE_IDENTITY_BYTES: usize = 8 * 1024;

/// Failure to construct a canonical compiler-owned local type identity.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LocalTypeIdentityError {
    InvalidPackage,
    InvalidSourceFile,
    InvalidDeclaration,
    TooLong { len: usize, max: usize },
}

impl fmt::Display for LocalTypeIdentityError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidPackage => formatter.write_str("local type package must be canonical"),
            Self::InvalidSourceFile => formatter
                .write_str("local type source file must be a canonical portable .vo file name"),
            Self::InvalidDeclaration => {
                formatter.write_str("local type declaration must be a named Vo identifier")
            }
            Self::TooLong { len, max } => write!(
                formatter,
                "local type identity is {len} bytes, exceeding the {max}-byte limit"
            ),
        }
    }
}

impl core::error::Error for LocalTypeIdentityError {}

const LOWER_HEX: &[u8; 16] = b"0123456789abcdef";

/// Return whether `character` may begin a Vo identifier.
#[inline]
pub fn is_identifier_start(character: char) -> bool {
    character == '_' || ALPHABETIC.contains(character)
}

/// Return whether `character` may continue a Vo identifier.
#[inline]
pub fn is_identifier_continue(character: char) -> bool {
    character == '_' || ALNUM.contains(character)
}

/// Return whether `name` is one complete lexical Vo identifier.
#[inline]
pub fn is_identifier(name: &str) -> bool {
    let mut characters = name.chars();
    characters.next().is_some_and(is_identifier_start) && characters.all(is_identifier_continue)
}

/// Return whether `name` is a language keyword.
#[inline]
pub fn is_keyword(name: &str) -> bool {
    VO_KEYWORDS.contains(&name)
}

/// Return whether `name` can identify a named source declaration.
///
/// The blank identifier is lexical identifier syntax, but it never denotes a
/// callable declaration. Keywords are tokenized before identifier parsing.
#[inline]
pub fn is_named_declaration_identifier(name: &str) -> bool {
    name != "_" && is_identifier(name) && !is_keyword(name)
}

/// Return whether `name` begins with a Unicode 16.0 `Uppercase` code point.
#[inline]
pub fn is_exported_name(name: &str) -> bool {
    name.chars()
        .next()
        .is_some_and(|character| UPPERCASE.contains(character))
}

/// Return whether `character` has Unicode 16.0 `White_Space`.
#[inline]
pub fn is_unicode_white_space(character: char) -> bool {
    WHITE_SPACE.contains(character)
}

/// Return whether `character` has Unicode 16.0 General Category `Control`.
#[inline]
pub fn is_unicode_control(character: char) -> bool {
    GENERAL_CATEGORY.get(character) == GeneralCategory::Control
}

/// Return whether a string begins or ends with Unicode 16.0 white space.
#[inline]
pub fn has_unicode_white_space_boundary(value: &str) -> bool {
    value.chars().next().is_some_and(is_unicode_white_space)
        || value
            .chars()
            .next_back()
            .is_some_and(is_unicode_white_space)
}

/// Construct the canonical runtime identity for one function-local named
/// type. The source file name is encoded injectively as lowercase UTF-8 hex;
/// the byte offset is local to that file, so unrelated file ordering and file
/// sizes cannot perturb the identity.
pub fn build_local_type_identity(
    package: &str,
    source_file: &str,
    source_offset: u32,
    declaration: &str,
) -> Result<String, LocalTypeIdentityError> {
    if crate::extern_key::validate_canonical_package_path(package).is_err() {
        return Err(LocalTypeIdentityError::InvalidPackage);
    }
    if !is_portable_vo_source_file_name(source_file) {
        return Err(LocalTypeIdentityError::InvalidSourceFile);
    }
    if !is_named_declaration_identifier(declaration) {
        return Err(LocalTypeIdentityError::InvalidDeclaration);
    }

    let mut encoded_file = String::with_capacity(source_file.len() * 2);
    for byte in source_file.bytes() {
        encoded_file.push(LOWER_HEX[usize::from(byte >> 4)] as char);
        encoded_file.push(LOWER_HEX[usize::from(byte & 0x0f)] as char);
    }
    let identity = format_local_type_identity(package, &encoded_file, source_offset, declaration);
    if identity.len() > MAX_NAMED_TYPE_IDENTITY_BYTES {
        return Err(LocalTypeIdentityError::TooLong {
            len: identity.len(),
            max: MAX_NAMED_TYPE_IDENTITY_BYTES,
        });
    }
    Ok(identity)
}

#[cfg(not(feature = "std"))]
fn format_local_type_identity(
    package: &str,
    encoded_file: &str,
    source_offset: u32,
    declaration: &str,
) -> String {
    alloc::format!(
        "{package}{LOCAL_TYPE_IDENTITY_MARKER}{encoded_file}:{source_offset}.{declaration}"
    )
}

#[cfg(feature = "std")]
fn format_local_type_identity(
    package: &str,
    encoded_file: &str,
    source_offset: u32,
    declaration: &str,
) -> String {
    format!("{package}{LOCAL_TYPE_IDENTITY_MARKER}{encoded_file}:{source_offset}.{declaration}")
}

/// Return whether an identity exactly matches the compiler-owned local type
/// grammar emitted by [`build_local_type_identity`].
pub fn is_local_type_identity(identity: &str) -> bool {
    if identity.len() > MAX_NAMED_TYPE_IDENTITY_BYTES {
        return false;
    }
    let Some((qualified, declaration)) = identity.rsplit_once('.') else {
        return false;
    };
    if !is_named_declaration_identifier(declaration) {
        return false;
    }
    let Some((package, discriminator)) = qualified.split_once(LOCAL_TYPE_IDENTITY_MARKER) else {
        return false;
    };
    if crate::extern_key::validate_canonical_package_path(package).is_err() {
        return false;
    }
    let Some((encoded_file, source_offset)) = discriminator.split_once(':') else {
        return false;
    };
    if source_offset.is_empty()
        || (source_offset.len() > 1 && source_offset.starts_with('0'))
        || !source_offset.bytes().all(|byte| byte.is_ascii_digit())
        || source_offset.parse::<u32>().is_err()
    {
        return false;
    }
    let Some(source_file) = decode_lower_hex_utf8(encoded_file) else {
        return false;
    };
    is_portable_vo_source_file_name(&source_file)
}

fn decode_lower_hex_utf8(encoded: &str) -> Option<String> {
    if encoded.is_empty()
        || encoded.len() > crate::extern_key::MAX_PORTABLE_PACKAGE_COMPONENT_BYTES * 2
        || !encoded.len().is_multiple_of(2)
    {
        return None;
    }
    let mut decoded = Vec::with_capacity(encoded.len() / 2);
    for pair in encoded.as_bytes().chunks_exact(2) {
        let high = lower_hex_value(pair[0])?;
        let low = lower_hex_value(pair[1])?;
        decoded.push((high << 4) | low);
    }
    String::from_utf8(decoded).ok()
}

fn lower_hex_value(byte: u8) -> Option<u8> {
    match byte {
        b'0'..=b'9' => Some(byte - b'0'),
        b'a'..=b'f' => Some(byte - b'a' + 10),
        _ => None,
    }
}

fn is_portable_vo_source_file_name(value: &str) -> bool {
    if value.is_empty()
        || value.len() > crate::extern_key::MAX_PORTABLE_PACKAGE_COMPONENT_BYTES
        || !value.ends_with(".vo")
        || value == "."
        || value == ".."
        || has_unicode_white_space_boundary(value)
        || value.ends_with('.')
        || value.contains('/')
        || value.contains('\\')
        || value
            .chars()
            .any(|character| is_unicode_control(character) || r#"<>:"|?*"#.contains(character))
        || !ComposingNormalizerBorrowed::new_nfc().is_normalized(value)
    {
        return false;
    }
    let stem = value.split('.').next().unwrap_or(value);
    let stem_key = portable_case_key(stem);
    if ["con", "prn", "aux", "nul", "conin$", "conout$"].contains(&stem_key.as_str()) {
        return false;
    }
    !is_numbered_windows_device_name(&stem_key, "com")
        && !is_numbered_windows_device_name(&stem_key, "lpt")
}

fn is_numbered_windows_device_name(value: &str, prefix: &str) -> bool {
    let Some(suffix) = value.strip_prefix(prefix) else {
        return false;
    };
    let mut suffix = suffix.chars();
    matches!(suffix.next(), Some('1'..='9' | '¹' | '²' | '³')) && suffix.next().is_none()
}

/// Canonical Unicode 16.0 collision key shared by lower-layer portable-name
/// validators. Full case folding may expand a scalar, so NFC is applied after
/// folding to keep canonically equivalent spellings on one key.
pub(crate) fn portable_case_key(value: &str) -> String {
    let folded = CaseMapperBorrowed::new().fold_string(value);
    ComposingNormalizerBorrowed::new_nfc()
        .normalize(&folded)
        .into_owned()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn identifier_properties_are_pinned_to_unicode_16() {
        assert_eq!(UNICODE_PROFILE_VERSION, "16.0.0");
        assert!(is_identifier_start('A'));
        assert!(is_identifier_start('变'));
        assert!(!is_identifier_start('\u{1e6c0}'));

        assert!(is_identifier_continue('２'));
        assert!(!is_identifier_continue('²'));
        assert!(is_identifier("绘制２"));
        assert!(!is_identifier("2draw"));
        assert_eq!(portable_case_key("Straße"), "strasse");
        assert_eq!(portable_case_key("Straẞe"), "strasse");
        assert_eq!(portable_case_key("İ"), portable_case_key("i\u{0307}"));
    }

    #[test]
    fn named_declarations_exclude_blank_and_keywords() {
        assert!(is_named_declaration_identifier("绘制"));
        assert!(!is_named_declaration_identifier("_"));
        for keyword in VO_KEYWORDS {
            assert!(is_identifier(keyword));
            assert!(is_keyword(keyword));
            assert!(!is_named_declaration_identifier(keyword));
        }
    }

    #[test]
    fn exported_names_use_the_pinned_uppercase_property() {
        assert!(is_exported_name("A"));
        assert!(is_exported_name("Ⅰtem"));
        assert!(!is_exported_name("ⅰtem"));
        assert!(!is_exported_name("_Hidden"));
        assert!(!is_exported_name(""));
    }

    #[test]
    fn portable_text_properties_use_the_same_pinned_unicode_profile() {
        assert!(is_unicode_white_space('\u{0085}'));
        assert!(is_unicode_white_space('\u{00a0}'));
        assert!(!is_unicode_white_space('\u{200b}'));
        assert!(is_unicode_control('\u{0085}'));
        assert!(!is_unicode_control('\u{200e}'));
        assert!(has_unicode_white_space_boundary("\u{00a0}name"));
        assert!(has_unicode_white_space_boundary("name\u{0085}"));
        assert!(!has_unicode_white_space_boundary("na me"));
    }

    #[test]
    fn local_type_identities_are_injective_and_strictly_canonical() {
        let identity =
            build_local_type_identity("github.com/acme/app/pkg", "local.part@类型.vo", 42, "类型")
                .unwrap();
        assert_eq!(
            identity,
            "github.com/acme/app/pkg@local:6c6f63616c2e7061727440e7b1bbe59e8b2e766f:42.类型"
        );
        assert!(is_local_type_identity(&identity));
        assert_ne!(
            identity,
            build_local_type_identity("github.com/acme/app/pkg", "other.vo", 42, "类型").unwrap()
        );
        for invalid_file in [
            "left/main.vo",
            "right/main.vo",
            "/main.vo",
            "../main.vo",
            "with:colon.vo",
            "decomposed-e\u{301}.vo",
        ] {
            assert!(
                build_local_type_identity("main", invalid_file, 0, "Point").is_err(),
                "accepted {invalid_file:?}"
            );
        }
        assert!(matches!(
            build_local_type_identity(
                "main",
                "main.vo",
                0,
                &"T".repeat(MAX_NAMED_TYPE_IDENTITY_BYTES)
            ),
            Err(LocalTypeIdentityError::TooLong { .. })
        ));
        let oversized_file_hex =
            "61".repeat(crate::extern_key::MAX_PORTABLE_PACKAGE_COMPONENT_BYTES.saturating_add(1));
        assert!(!is_local_type_identity(&format_local_type_identity(
            "main",
            &oversized_file_hex,
            0,
            "Point"
        )));

        for forged in [
            "github.com/acme/app/pkg@local:6d61696e2e766f:042.Point",
            "github.com/acme/app/pkg@local:6D61696E2E766F:42.Point",
            "github.com/acme/app/pkg@local:6d61696e2e766f:4294967296.Point",
            "github.com/acme/app/pkg@local:6d61696e2e766f:42.type",
            "github.com/acme/app/pkg@local::42.Point",
            "github.com/acme/app/pkg@local:6:42.Point",
            "github.com/acme/app/pkg@local:ff:42.Point",
            "main@local:6e6f742d766f2e747874:42.Point",
        ] {
            assert!(!is_local_type_identity(forged), "accepted {forged:?}");
        }
    }
}
