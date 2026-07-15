pub mod lockfile;
pub mod manifest;
pub mod modfile;
pub mod source_files;
pub mod web_manifest;
pub mod workfile;

pub use source_files::{
    canonical_source_file_set, is_source_file_set_candidate, CanonicalSourceFileSet,
    SourceFileEntry,
};
pub use web_manifest::{WebManifest, WebManifestArtifact, WebManifestExtension};

use std::collections::{BTreeMap, BTreeSet};
use std::marker::PhantomData;
use std::path::{Component, Path};

use icu_casemap::CaseMapperBorrowed;
use icu_normalizer::ComposingNormalizerBorrowed;
use serde::de::{self, IgnoredAny, MapAccess, SeqAccess, Visitor};
use serde::{Deserialize, Deserializer};

pub(crate) struct BoundedTextOutput {
    value: String,
    limit: usize,
}

pub(crate) struct BoundedBytesOutput {
    value: Vec<u8>,
    limit: usize,
    failure: Option<String>,
}

/// Deserialize an untrusted sequence without first allocating every wire
/// element and checking its length afterwards.
pub(crate) fn deserialize_bounded_vec<'de, D, T>(
    deserializer: D,
    limit: usize,
    field: &'static str,
) -> Result<Vec<T>, D::Error>
where
    D: Deserializer<'de>,
    T: Deserialize<'de>,
{
    struct BoundedVecVisitor<T> {
        limit: usize,
        field: &'static str,
        marker: PhantomData<T>,
    }

    impl<'de, T> Visitor<'de> for BoundedVecVisitor<T>
    where
        T: Deserialize<'de>,
    {
        type Value = Vec<T>;

        fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(
                formatter,
                "a {} array with at most {} entries",
                self.field, self.limit
            )
        }

        fn visit_seq<A>(self, mut sequence: A) -> Result<Self::Value, A::Error>
        where
            A: SeqAccess<'de>,
        {
            if sequence
                .size_hint()
                .is_some_and(|length| length > self.limit)
            {
                return Err(de::Error::custom(format!(
                    "{} contains more than {} entries",
                    self.field, self.limit
                )));
            }
            let capacity = sequence.size_hint().unwrap_or(0).min(self.limit);
            let mut values = Vec::new();
            values.try_reserve(capacity).map_err(|_| {
                de::Error::custom(format!("failed to reserve {} entries", self.field))
            })?;
            loop {
                if values.len() == self.limit {
                    if sequence.next_element::<IgnoredAny>()?.is_some() {
                        return Err(de::Error::custom(format!(
                            "{} contains more than {} entries",
                            self.field, self.limit
                        )));
                    }
                    break;
                }
                let Some(value) = sequence.next_element::<T>()? else {
                    break;
                };
                values.push(value);
            }
            Ok(values)
        }
    }

    deserializer.deserialize_seq(BoundedVecVisitor {
        limit,
        field,
        marker: PhantomData,
    })
}

/// Deserialize a bounded canonical map while rejecting repeated wire keys.
/// Serde's standard map implementations otherwise retain the last duplicate,
/// which can make a signed manifest's meaning parser-dependent.
pub(crate) fn deserialize_bounded_btree_map<'de, D, K, V>(
    deserializer: D,
    limit: usize,
    field: &'static str,
) -> Result<BTreeMap<K, V>, D::Error>
where
    D: Deserializer<'de>,
    K: Deserialize<'de> + Ord,
    V: Deserialize<'de>,
{
    struct BoundedMapVisitor<K, V> {
        limit: usize,
        field: &'static str,
        marker: PhantomData<(K, V)>,
    }

    impl<'de, K, V> Visitor<'de> for BoundedMapVisitor<K, V>
    where
        K: Deserialize<'de> + Ord,
        V: Deserialize<'de>,
    {
        type Value = BTreeMap<K, V>;

        fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(
                formatter,
                "a {} map with at most {} entries",
                self.field, self.limit
            )
        }

        fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
        where
            A: MapAccess<'de>,
        {
            if map.size_hint().is_some_and(|length| length > self.limit) {
                return Err(de::Error::custom(format!(
                    "{} contains more than {} entries",
                    self.field, self.limit
                )));
            }
            let mut values = BTreeMap::new();
            loop {
                if values.len() == self.limit {
                    if map.next_entry::<IgnoredAny, IgnoredAny>()?.is_some() {
                        return Err(de::Error::custom(format!(
                            "{} contains more than {} entries",
                            self.field, self.limit
                        )));
                    }
                    break;
                }
                let Some((key, value)) = map.next_entry::<K, V>()? else {
                    break;
                };
                if values.insert(key, value).is_some() {
                    return Err(de::Error::custom(format!(
                        "{} contains a duplicate key",
                        self.field
                    )));
                }
            }
            Ok(values)
        }
    }

    deserializer.deserialize_map(BoundedMapVisitor {
        limit,
        field,
        marker: PhantomData,
    })
}

impl BoundedBytesOutput {
    pub(crate) fn new(limit: usize) -> Result<Self, String> {
        let mut value = Vec::new();
        value
            .try_reserve(limit.min(4 * 1024))
            .map_err(|_| "failed to reserve output buffer".to_string())?;
        Ok(Self {
            value,
            limit,
            failure: None,
        })
    }

    pub(crate) fn finish(self) -> Result<Vec<u8>, String> {
        match self.failure {
            Some(failure) => Err(failure),
            None => Ok(self.value),
        }
    }
}

impl std::io::Write for BoundedBytesOutput {
    fn write(&mut self, buffer: &[u8]) -> std::io::Result<usize> {
        let failure = match self.value.len().checked_add(buffer.len()) {
            None => Some("rendered output length overflow".to_string()),
            Some(next) if next > self.limit => Some(format!(
                "rendered output exceeds the {}-byte limit",
                self.limit
            )),
            Some(_) => None,
        };
        if let Some(failure) = failure {
            self.failure = Some(failure.clone());
            return Err(std::io::Error::other(failure));
        }
        if self.value.try_reserve(buffer.len()).is_err() {
            let failure = "failed to reserve rendered output".to_string();
            self.failure = Some(failure.clone());
            return Err(std::io::Error::other(failure));
        }
        self.value.extend_from_slice(buffer);
        Ok(buffer.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

impl BoundedTextOutput {
    pub(crate) fn new(limit: usize) -> Result<Self, String> {
        let mut value = String::new();
        value
            .try_reserve(limit.min(4 * 1024))
            .map_err(|_| "failed to reserve output buffer".to_string())?;
        Ok(Self { value, limit })
    }

    pub(crate) fn push_str(&mut self, value: &str) -> Result<(), String> {
        let next_len = self
            .value
            .len()
            .checked_add(value.len())
            .ok_or_else(|| "rendered output length overflow".to_string())?;
        if next_len > self.limit {
            return Err(format!(
                "rendered output exceeds the {}-byte limit",
                self.limit
            ));
        }
        self.value
            .try_reserve(value.len())
            .map_err(|_| "failed to reserve rendered output".to_string())?;
        self.value.push_str(value);
        Ok(())
    }

    pub(crate) fn push_toml_string(&mut self, value: &str) -> Result<(), String> {
        self.push_str("\"")?;
        for character in value.chars() {
            match character {
                '"' => self.push_str(r#"\""#)?,
                '\\' => self.push_str(r#"\\"#)?,
                '\u{0008}' => self.push_str(r#"\b"#)?,
                '\t' => self.push_str(r#"\t"#)?,
                '\n' => self.push_str(r#"\n"#)?,
                '\u{000c}' => self.push_str(r#"\f"#)?,
                '\r' => self.push_str(r#"\r"#)?,
                character if character.is_control() => {
                    let codepoint = character as u32;
                    if codepoint <= u16::MAX as u32 {
                        self.push_str(&format!(r#"\u{codepoint:04X}"#))?;
                    } else {
                        self.push_str(&format!(r#"\U{codepoint:08X}"#))?;
                    }
                }
                character => {
                    let mut bytes = [0u8; 4];
                    self.push_str(character.encode_utf8(&mut bytes))?;
                }
            }
        }
        self.push_str("\"")
    }

    pub(crate) fn finish(self) -> String {
        self.value
    }
}

/// Conservative byte ceiling shared by every module-relative wire path.
pub const MAX_PORTABLE_PATH_BYTES: usize = 4 * 1024;
/// Conservative byte ceiling shared by every component of a wire path.
pub const MAX_PORTABLE_PATH_COMPONENT_BYTES: usize = 255;
/// Maximum component depth shared by manifests and bounded filesystem walks.
pub const MAX_PORTABLE_PATH_COMPONENTS: usize = 256;

/// Render one TOML basic-string value with canonical escaping.
#[cfg(test)]
pub(crate) fn render_toml_string(value: &str) -> String {
    // Keep the wire spelling independent of the TOML serializer's heuristic
    // choice between basic, literal, and multiline strings.  A single-line
    // basic string gives every non-Rust consumer one exact canonical form.
    let mut rendered = String::with_capacity(
        rendered_toml_string_len(value).unwrap_or_else(|| value.len().saturating_add(2)),
    );
    rendered.push('"');
    for character in value.chars() {
        match character {
            '"' => rendered.push_str(r#"\""#),
            '\\' => rendered.push_str(r#"\\"#),
            '\u{0008}' => rendered.push_str(r#"\b"#),
            '\t' => rendered.push_str(r#"\t"#),
            '\n' => rendered.push_str(r#"\n"#),
            '\u{000c}' => rendered.push_str(r#"\f"#),
            '\r' => rendered.push_str(r#"\r"#),
            character if character.is_control() => {
                use std::fmt::Write as _;
                let codepoint = character as u32;
                if codepoint <= u16::MAX as u32 {
                    write!(&mut rendered, r#"\u{codepoint:04X}"#)
                        .expect("writing into a String cannot fail");
                } else {
                    write!(&mut rendered, r#"\U{codepoint:08X}"#)
                        .expect("writing into a String cannot fail");
                }
            }
            character => rendered.push(character),
        }
    }
    rendered.push('"');
    rendered
}

pub(crate) fn rendered_toml_string_len(value: &str) -> Option<usize> {
    value.chars().try_fold(2usize, |length, character| {
        let encoded = match character {
            '"' | '\\' | '\u{0008}' | '\t' | '\n' | '\u{000c}' | '\r' => 2,
            character if character.is_control() => {
                if (character as u32) <= u16::MAX as u32 {
                    6
                } else {
                    10
                }
            }
            character => character.len_utf8(),
        };
        length.checked_add(encoded)
    })
}

pub(crate) fn validate_file_name(value: &str) -> Result<(), String> {
    if value.is_empty()
        || vo_common::identifier::has_unicode_white_space_boundary(value)
        || value == "."
        || value == ".."
        || value.contains('/')
        || value.contains('\\')
        || value.chars().any(vo_common::identifier::is_unicode_control)
        || validate_portable_path_component(value).is_err()
    {
        return Err("must be a non-empty file name".to_string());
    }
    Ok(())
}

/// Validate one cross-platform component used by module wire paths.
pub fn validate_portable_path_component(value: &str) -> Result<(), String> {
    if value.is_empty()
        || value.len() > MAX_PORTABLE_PATH_COMPONENT_BYTES
        || value == "."
        || value == ".."
        || vo_common::identifier::has_unicode_white_space_boundary(value)
        || value.ends_with('.')
        || value.contains('/')
        || value.contains('\\')
        || value.chars().any(|character| {
            vo_common::identifier::is_unicode_control(character) || r#"<>:"|?*"#.contains(character)
        })
    {
        return Err("must be a normalized portable path component".to_string());
    }
    if !ComposingNormalizerBorrowed::new_nfc().is_normalized(value) {
        return Err("must use canonical NFC Unicode normalization".to_string());
    }
    let stem = value.split('.').next().unwrap_or(value);
    let stem_key = portable_case_key(stem);
    if ["con", "prn", "aux", "nul", "conin$", "conout$"].contains(&stem_key.as_str())
        || is_numbered_windows_device_name(&stem_key, "com")
        || is_numbered_windows_device_name(&stem_key, "lpt")
    {
        return Err("must not use a reserved platform file name".to_string());
    }
    Ok(())
}

/// Validate a canonical `/`-separated path relative to a module root.
pub fn validate_portable_relative_path(value: &str) -> Result<(), String> {
    if value.is_empty()
        || value.len() > MAX_PORTABLE_PATH_BYTES
        || value.split('/').count() > MAX_PORTABLE_PATH_COMPONENTS
        || vo_common::identifier::has_unicode_white_space_boundary(value)
        || value.contains('\\')
        || value.chars().any(vo_common::identifier::is_unicode_control)
    {
        return Err("must be a normalized portable module-relative path".to_string());
    }
    for component in value.split('/') {
        validate_portable_path_component(component)
            .map_err(|_| "must be a normalized portable module-relative path".to_string())?;
    }
    Ok(())
}

/// Convert one native relative path into the canonical slash-separated wire
/// representation. Native separators are handled by `Path::components`, so
/// callers do not need platform-specific string replacement.
pub fn portable_relative_path_from_path(value: &Path) -> Result<String, String> {
    if value.as_os_str().is_empty() {
        return Err("must be a non-empty relative path".to_string());
    }
    let mut portable = String::new();
    for component in value.components() {
        let Component::Normal(component) = component else {
            return Err("must contain only normalized relative path components".to_string());
        };
        let component = component
            .to_str()
            .ok_or_else(|| "must contain only valid UTF-8 path components".to_string())?;
        validate_portable_path_component(component)?;
        if !portable.is_empty() {
            portable.push('/');
        }
        portable.push_str(component);
    }
    validate_portable_relative_path(&portable)?;
    Ok(portable)
}

/// Return whether a module-relative source path aliases cache-owned metadata.
/// The comparison follows case-insensitive host behavior so a package has the
/// same meaning on Linux, macOS, and Windows.
pub fn is_reserved_module_cache_path(value: &str) -> bool {
    let Some(first) = value.split('/').next() else {
        return false;
    };
    let first_key = portable_case_key(first);
    [
        "artifacts",
        ".vo-version",
        ".vo-source-digest",
        "vo.release.json",
    ]
    .contains(&first_key.as_str())
}

/// Tracks a set of portable paths while rejecting filesystem-dependent
/// spelling collisions and file/directory prefix conflicts.
///
/// The collision key uses full Unicode case folding followed by NFC. This
/// rejects names that can alias on common case-insensitive filesystems while
/// preserving the original spelling for canonical wire output.
#[derive(Debug, Default)]
pub struct PortablePathSet {
    spellings: BTreeMap<String, String>,
    paths: BTreeSet<String>,
    files: BTreeSet<String>,
    directories: BTreeSet<String>,
}

impl PortablePathSet {
    /// Insert an untyped path reference while allowing parent/child entries.
    /// This is suitable for include lists whose entries may independently
    /// refer to files or directories.
    pub fn insert_path(&mut self, value: &str) -> Result<bool, String> {
        let prefixes = portable_path_prefixes(value)?;
        self.validate_spellings(value, &prefixes)?;
        self.remember_spellings(&prefixes);
        let key = prefixes
            .last()
            .map(|(_, key)| key.clone())
            .ok_or_else(|| "portable path contains no components".to_string())?;
        Ok(self.paths.insert(key))
    }

    /// Insert a file path. Returns `false` for an exact duplicate.
    pub fn insert_file(&mut self, value: &str) -> Result<bool, String> {
        self.insert(value, false)
    }

    /// Insert a directory path. Returns `false` for an exact duplicate.
    pub fn insert_directory(&mut self, value: &str) -> Result<bool, String> {
        self.insert(value, true)
    }

    fn insert(&mut self, value: &str, is_directory: bool) -> Result<bool, String> {
        let prefixes = portable_path_prefixes(value)?;
        self.validate_spellings(value, &prefixes)?;

        for (index, (spelling, key)) in prefixes.iter().enumerate() {
            let last = index + 1 == prefixes.len();
            if !last {
                if self.files.contains(key) {
                    return Err(format!("path {value:?} descends through file {spelling:?}"));
                }
                continue;
            }

            if is_directory {
                if self.files.contains(key) {
                    return Err(format!(
                        "path {value:?} is declared as both a file and directory"
                    ));
                }
            } else if self.directories.contains(key) {
                return Err(format!(
                    "path {value:?} is declared as both a file and directory"
                ));
            }
        }

        self.remember_spellings(&prefixes);
        for (index, (_, key)) in prefixes.iter().enumerate() {
            if index + 1 < prefixes.len() {
                self.directories.insert(key.clone());
            }
        }
        let key = prefixes
            .last()
            .map(|(_, key)| key.clone())
            .expect("portable relative paths always contain one component");
        if is_directory {
            Ok(self.directories.insert(key))
        } else {
            Ok(self.files.insert(key))
        }
    }

    fn validate_spellings(&self, value: &str, prefixes: &[(String, String)]) -> Result<(), String> {
        for (spelling, key) in prefixes {
            if let Some(existing) = self.spellings.get(key) {
                if existing != spelling {
                    return Err(format!(
                        "path {value:?} conflicts with portable spelling {existing:?}"
                    ));
                }
            }
        }
        Ok(())
    }

    fn remember_spellings(&mut self, prefixes: &[(String, String)]) {
        for (spelling, key) in prefixes {
            self.spellings
                .entry(key.clone())
                .or_insert_with(|| spelling.clone());
        }
    }
}

fn portable_path_prefixes(value: &str) -> Result<Vec<(String, String)>, String> {
    validate_portable_relative_path(value)?;
    let components = value.split('/').collect::<Vec<_>>();
    let mut prefixes = Vec::with_capacity(components.len());
    let mut spelling = String::new();
    let mut key = String::new();
    for component in components {
        if !spelling.is_empty() {
            spelling.push('/');
            key.push('/');
        }
        spelling.push_str(component);
        key.push_str(&portable_case_key(component));
        prefixes.push((spelling.clone(), key.clone()));
    }
    Ok(prefixes)
}

/// Build the canonical Unicode 16.0 case-insensitive key used by every
/// portable module path comparison.
///
/// Full case folding can expand one scalar into multiple scalars. NFC after
/// folding keeps canonically equivalent spellings on one stable key.
pub fn portable_case_key(value: &str) -> String {
    let folded = CaseMapperBorrowed::new().fold_string(value);
    ComposingNormalizerBorrowed::new_nfc()
        .normalize(&folded)
        .into_owned()
}

fn is_numbered_windows_device_name(value: &str, prefix: &str) -> bool {
    let Some(suffix) = value.strip_prefix(prefix) else {
        return false;
    };
    let mut suffix = suffix.chars();
    matches!(suffix.next(), Some('1'..='9' | '¹' | '²' | '³')) && suffix.next().is_none()
}

/// Validate a Git commit hash: exactly 40 lowercase hex characters.
/// Returns `Ok(())` or an error message string for the caller to wrap.
pub(crate) fn validate_commit_hash(commit: &str) -> Result<(), String> {
    if commit.len() != 40 {
        return Err(format!(
            "commit must be 40-char hex, got {} chars",
            commit.len()
        ));
    }
    if !commit
        .chars()
        .all(|c| c.is_ascii_hexdigit() && !c.is_ascii_uppercase())
    {
        return Err("commit must be lowercase hex".into());
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(serde::Deserialize)]
    struct TwoStrings {
        #[serde(deserialize_with = "deserialize_two_strings")]
        values: Vec<String>,
    }

    fn deserialize_two_strings<'de, D>(deserializer: D) -> Result<Vec<String>, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserialize_bounded_vec(deserializer, 2, "test values")
    }

    #[derive(serde::Deserialize)]
    struct TwoEntries {
        #[serde(deserialize_with = "deserialize_two_entries")]
        values: BTreeMap<String, String>,
    }

    fn deserialize_two_entries<'de, D>(
        deserializer: D,
    ) -> Result<BTreeMap<String, String>, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserialize_bounded_btree_map(deserializer, 2, "test map")
    }

    #[test]
    fn bounded_serde_containers_reject_excess_and_duplicate_keys() {
        let values = serde_json::from_str::<TwoStrings>(r#"{"values":["a","b"]}"#).unwrap();
        assert_eq!(values.values, vec!["a".to_string(), "b".to_string()]);
        let error = serde_json::from_str::<TwoStrings>(r#"{"values":["a","b","c"]}"#)
            .err()
            .unwrap()
            .to_string();
        assert!(error.contains("more than 2"), "{error}");

        let values = serde_json::from_str::<TwoEntries>(r#"{"values":{"a":"1","b":"2"}}"#).unwrap();
        assert_eq!(values.values.len(), 2);
        let error = serde_json::from_str::<TwoEntries>(r#"{"values":{"a":"1","a":"2"}}"#)
            .err()
            .unwrap()
            .to_string();
        assert!(error.contains("duplicate key"), "{error}");
    }

    #[test]
    fn toml_strings_roundtrip_all_required_escapes() {
        let value = "quote\" slash\\ backspace\u{8} tab\t newline\n form\u{c} return\r nul\0 delete\u{7f} c1\u{85} 中 e\u{301} \u{1f642}";
        let rendered = render_toml_string(value);
        assert_eq!(
            rendered,
            "\"quote\\\" slash\\\\ backspace\\b tab\\t newline\\n form\\f return\\r nul\\u0000 delete\\u007F c1\\u0085 中 e\u{301} \u{1f642}\""
        );
        let parsed = format!("value = {rendered}")
            .parse::<toml::Value>()
            .expect("rendered value must remain valid TOML");
        assert_eq!(
            parsed.get("value").and_then(toml::Value::as_str),
            Some(value)
        );

        for codepoint in 0..=u16::MAX as u32 {
            let Some(character) = char::from_u32(codepoint) else {
                continue;
            };
            if !character.is_control() {
                continue;
            }
            let value = character.to_string();
            let rendered = render_toml_string(&value);
            let parsed = format!("value = {rendered}")
                .parse::<toml::Value>()
                .expect("every canonical control escape must parse");
            assert_eq!(
                parsed.get("value").and_then(toml::Value::as_str),
                Some(value.as_str()),
                "control U+{codepoint:04X}"
            );
        }
    }

    #[test]
    fn portable_paths_reject_platform_specific_and_ambiguous_components() {
        for path in [
            "a//b",
            "a/../b",
            r"a\b",
            "C:/b",
            "assets/CON.txt",
            "assets/trailing.",
            "assets/a:b",
            "assets/COM¹.log",
            "assets/CONOUT$",
            "\u{00a0}assets/file.vo",
            "assets/file.vo\u{0085}",
        ] {
            assert!(validate_portable_relative_path(path).is_err(), "{path}");
        }
        assert!(validate_portable_relative_path("assets/数据.json").is_ok());
        assert!(validate_portable_path_component("aaé").is_ok());
        assert!(validate_portable_path_component("a/b").is_err());
        assert!(validate_portable_path_component(r"a\b").is_err());
        assert!(validate_portable_path_component("aae\u{301}").is_err());
        assert!(validate_portable_path_component(&"a".repeat(256)).is_err());
        assert!(validate_portable_relative_path(&"a".repeat(4097)).is_err());
        let deepest = std::iter::repeat_n("a", MAX_PORTABLE_PATH_COMPONENTS)
            .collect::<Vec<_>>()
            .join("/");
        assert!(validate_portable_relative_path(&deepest).is_ok());
        assert!(validate_portable_relative_path(&format!("{deepest}/a")).is_err());
    }

    #[test]
    fn portable_path_sets_reject_aliases_and_file_directory_conflicts() {
        let mut paths = PortablePathSet::default();
        assert!(paths.insert_file("Source/Main.vo").unwrap());
        assert!(!paths.insert_file("Source/Main.vo").unwrap());
        assert!(paths.insert_file("source/other.vo").is_err());
        assert!(paths.insert_file("Source/other.vo").unwrap());

        let mut paths = PortablePathSet::default();
        assert!(paths.insert_file("assets").unwrap());
        assert!(paths.insert_file("assets/icon.svg").is_err());

        let mut paths = PortablePathSet::default();
        assert!(paths.insert_file("assets/icon.svg").unwrap());
        assert!(paths.insert_file("assets").is_err());

        let mut paths = PortablePathSet::default();
        assert!(paths.insert_file("Straße.vo").unwrap());
        assert!(paths.insert_file("STRASSE.vo").is_err());

        let mut paths = PortablePathSet::default();
        assert!(paths.insert_file("İ.vo").unwrap());
        assert!(paths.insert_file("i\u{0307}.vo").is_err());

        assert_eq!(portable_case_key("artifactſ"), "artifacts");
        assert_eq!(portable_case_key("Straße"), "strasse");
        assert_eq!(portable_case_key("Straẞe"), "strasse");
        assert_eq!(portable_case_key("İ"), portable_case_key("i\u{0307}"));

        let mut includes = PortablePathSet::default();
        assert!(includes.insert_path("assets").unwrap());
        assert!(includes.insert_path("assets/icons/logo.svg").unwrap());
        assert!(!includes.insert_path("assets").unwrap());
        assert!(includes.insert_path("ASSETS/other.svg").is_err());
        assert!(includes.insert_path("Straße/data").unwrap());
        assert!(includes.insert_path("STRASSE/other").is_err());
    }

    #[test]
    fn native_relative_paths_convert_to_canonical_wire_paths() {
        assert_eq!(
            portable_relative_path_from_path(&Path::new("src").join("数据.vo")).unwrap(),
            "src/数据.vo"
        );
        assert!(portable_relative_path_from_path(Path::new("../src/main.vo")).is_err());
        assert!(portable_relative_path_from_path(Path::new("/src/main.vo")).is_err());
    }

    #[test]
    fn cache_metadata_paths_are_reserved_across_host_case_rules() {
        for path in [
            "artifacts",
            "ARTIFACTS/demo.wasm",
            "artifactſ/demo.wasm",
            "VO.RELEASE.JSON",
            "vo.releaſe.json",
            "vo.release.json/child",
            ".VO-VERSION",
            ".vo-version/child",
            ".Vo-Source-Digest",
            ".vo-ſource-digest/child",
            ".vo-source-digeﬆ/child",
            ".VO-SOURCE-DIGEST/child",
        ] {
            assert!(is_reserved_module_cache_path(path), "{path}");
        }
        assert!(!is_reserved_module_cache_path("docs/vo.release.json"));
    }
}
