pub mod lockfile;
pub mod manifest;
pub mod modfile;
pub mod package_manifest;
pub mod source_files;
pub mod workfile;

pub use package_manifest::PackageManifest;
pub use source_files::{
    is_package_file_candidate, validate_package_file_set, PackageFileSet, SourceFileEntry,
    SourceFileMode,
};

use std::collections::BTreeMap;
use std::marker::PhantomData;
use std::path::{Component, Path, PathBuf};

use icu_casemap::CaseMapperBorrowed;
use icu_normalizer::ComposingNormalizerBorrowed;
use serde::de::{self, IgnoredAny, MapAccess, SeqAccess, Visitor};
use serde::{Deserialize, Deserializer};
use vo_common::vfs::{normalize_fs_path, sort_fs_paths, FileSystem, MAX_DIRECTORY_ENTRIES};

pub(crate) struct BoundedTextOutput {
    value: String,
    limit: usize,
}

/// Byte-stable JSON encoder shared by authenticated module protocol files.
///
/// Container layout and field order remain explicit at each schema call site;
/// this writer owns the bounded byte buffer, decimal integers, and the exact
/// canonical string escaping contract. Keeping those rules here prevents a
/// serde formatter update from changing digest-bearing wire bytes.
pub(crate) struct CanonicalJsonWriter {
    value: Vec<u8>,
    limit: usize,
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

impl CanonicalJsonWriter {
    pub(crate) fn new(limit: usize) -> Result<Self, String> {
        let mut value = Vec::new();
        value
            .try_reserve(limit.min(4 * 1024))
            .map_err(|_| "failed to reserve canonical JSON output buffer".to_string())?;
        Ok(Self { value, limit })
    }

    fn push_bytes(&mut self, bytes: &[u8]) -> Result<(), String> {
        let next_len = self
            .value
            .len()
            .checked_add(bytes.len())
            .ok_or_else(|| "canonical JSON output length overflow".to_string())?;
        if next_len > self.limit {
            return Err(format!(
                "canonical JSON output exceeds the {}-byte limit",
                self.limit
            ));
        }
        self.value
            .try_reserve(bytes.len())
            .map_err(|_| "failed to reserve canonical JSON output".to_string())?;
        self.value.extend_from_slice(bytes);
        Ok(())
    }

    /// Append trusted JSON punctuation, indentation, or field-name bytes.
    pub(crate) fn push_raw(&mut self, value: &str) -> Result<(), String> {
        self.push_bytes(value.as_bytes())
    }

    /// Append one JSON string using the module protocol's exact wire spelling.
    pub(crate) fn push_string(&mut self, value: &str) -> Result<(), String> {
        const HEX: &[u8; 16] = b"0123456789abcdef";

        self.push_bytes(b"\"")?;
        for character in value.chars() {
            match character {
                '"' => self.push_bytes(br#"\""#)?,
                '\\' => self.push_bytes(br#"\\"#)?,
                '\u{0008}' => self.push_bytes(br"\b")?,
                '\t' => self.push_bytes(br"\t")?,
                '\n' => self.push_bytes(br"\n")?,
                '\u{000c}' => self.push_bytes(br"\f")?,
                '\r' => self.push_bytes(br"\r")?,
                '\u{0000}'..='\u{001f}' => {
                    let codepoint = character as u8;
                    self.push_bytes(&[
                        b'\\',
                        b'u',
                        b'0',
                        b'0',
                        HEX[usize::from(codepoint >> 4)],
                        HEX[usize::from(codepoint & 0x0f)],
                    ])?;
                }
                character => {
                    let mut encoded = [0u8; 4];
                    self.push_bytes(character.encode_utf8(&mut encoded).as_bytes())?;
                }
            }
        }
        self.push_bytes(b"\"")
    }

    pub(crate) fn push_u64(&mut self, value: u64) -> Result<(), String> {
        self.push_raw(&value.to_string())
    }

    pub(crate) fn finish(self) -> Vec<u8> {
        self.value
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

#[cfg(test)]
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
        "vo.package.json",
        "source.tar.gz",
    ]
    .contains(&first_key.as_str())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PortablePathEntryKind {
    File,
    Directory,
}

#[derive(Debug, Default)]
struct PortablePathNode {
    /// Children are keyed by the canonical spelling of one component. Keeping
    /// the trie component-wise avoids retaining every complete path prefix.
    children: BTreeMap<String, usize>,
    spelling: String,
    referenced_path: bool,
    entry_kind: Option<PortablePathEntryKind>,
}

/// A component trie for portable paths.
///
/// Each distinct component is retained once at its parent node. This keeps
/// memory proportional to the authored path bytes and materialized nodes even
/// for many maximally deep paths. Full Unicode case folding followed by NFC
/// rejects spelling aliases on common case-insensitive filesystems.
#[derive(Debug)]
pub struct PortablePathSet {
    nodes: Vec<PortablePathNode>,
    materialized_entries: usize,
    root_materialized_entries: usize,
    path_key_bytes: usize,
    max_path_key_bytes: usize,
}

impl Default for PortablePathSet {
    fn default() -> Self {
        Self {
            nodes: vec![PortablePathNode::default()],
            materialized_entries: 0,
            root_materialized_entries: 0,
            path_key_bytes: 0,
            max_path_key_bytes: crate::MAX_PACKAGE_PATH_KEY_BYTES,
        }
    }
}

impl PortablePathSet {
    #[cfg(test)]
    pub(crate) fn with_max_path_key_bytes(max_path_key_bytes: usize) -> Self {
        Self {
            max_path_key_bytes,
            ..Self::default()
        }
    }

    /// Insert an untyped path reference while allowing parent/child entries.
    /// This is suitable for include lists whose entries may independently
    /// refer to files or directories.
    pub fn insert_path(&mut self, value: &str) -> Result<bool, String> {
        let components = portable_path_components(value)?;
        self.validate_insert(value, &components, None)?;
        self.charge_new_components(&components)?;
        let node = self.ensure_components(&components);
        let inserted = !self.nodes[node].referenced_path;
        self.nodes[node].referenced_path = true;
        Ok(inserted)
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
        let components = portable_path_components(value)?;
        let requested = if is_directory {
            PortablePathEntryKind::Directory
        } else {
            PortablePathEntryKind::File
        };
        self.validate_insert(value, &components, Some(requested))?;
        self.charge_new_components(&components)?;

        let mut node = 0;
        let mut inserted = false;
        for (index, component) in components.iter().enumerate() {
            node = self.ensure_child(node, component);
            let last = index + 1 == components.len();
            let kind = if last {
                requested
            } else {
                PortablePathEntryKind::Directory
            };
            let newly_materialized = self.mark_materialized(node, index == 0, kind);
            if last {
                inserted = newly_materialized;
            }
        }
        Ok(inserted)
    }

    /// Number of typed file and directory nodes materialized by insertions.
    pub(crate) fn materialized_entry_count(&self) -> usize {
        self.materialized_entries
    }

    /// Number of typed entries directly below the root.
    pub(crate) fn root_materialized_entry_count(&self) -> usize {
        self.root_materialized_entries
    }

    /// Aggregate UTF-8 bytes charged for retained complete path keys.
    pub fn path_key_bytes(&self) -> usize {
        self.path_key_bytes
    }

    fn charge_new_components(
        &mut self,
        components: &[PortablePathComponent<'_>],
    ) -> Result<(), String> {
        let additional = self.additional_path_key_bytes(components)?;
        let charged = self
            .path_key_bytes
            .checked_add(additional)
            .ok_or_else(|| "portable path-key byte count overflows usize".to_string())?;
        if charged > self.max_path_key_bytes {
            return Err(format!(
                "path closure exceeds the {}-byte path-key limit",
                self.max_path_key_bytes,
            ));
        }
        self.path_key_bytes = charged;
        Ok(())
    }

    fn additional_path_key_bytes(
        &self,
        components: &[PortablePathComponent<'_>],
    ) -> Result<usize, String> {
        let mut node = 0usize;
        let mut prefix_bytes = 0usize;
        for (index, component) in components.iter().enumerate() {
            prefix_bytes = prefix_bytes
                .checked_add(usize::from(index != 0))
                .and_then(|bytes| bytes.checked_add(component.spelling.len()))
                .ok_or_else(|| "portable path-key byte count overflows usize".to_string())?;
            let Some(&child) = self.nodes[node].children.get(&component.key) else {
                let mut additional = prefix_bytes;
                for remaining in &components[index + 1..] {
                    prefix_bytes = prefix_bytes
                        .checked_add(1)
                        .and_then(|bytes| bytes.checked_add(remaining.spelling.len()))
                        .ok_or_else(|| {
                            "portable path-key byte count overflows usize".to_string()
                        })?;
                    additional = additional.checked_add(prefix_bytes).ok_or_else(|| {
                        "portable path-key byte count overflows usize".to_string()
                    })?;
                }
                return Ok(additional);
            };
            node = child;
        }
        Ok(0)
    }

    fn validate_insert(
        &self,
        value: &str,
        components: &[PortablePathComponent<'_>],
        requested: Option<PortablePathEntryKind>,
    ) -> Result<(), String> {
        let mut node = 0;
        for (index, component) in components.iter().enumerate() {
            let Some(&child) = self.nodes[node].children.get(&component.key) else {
                return Ok(());
            };
            let existing = &self.nodes[child];
            if existing.spelling != component.spelling {
                let existing_path = existing_prefix_spelling(components, index, &existing.spelling);
                return Err(format!(
                    "path {value:?} conflicts with portable spelling {existing_path:?}"
                ));
            }

            let last = index + 1 == components.len();
            if requested.is_some()
                && !last
                && existing.entry_kind == Some(PortablePathEntryKind::File)
            {
                let file_path = existing_prefix_spelling(components, index, &existing.spelling);
                return Err(format!(
                    "path {value:?} descends through file {file_path:?}"
                ));
            }
            if last {
                if let (Some(requested), Some(existing)) = (requested, existing.entry_kind) {
                    if requested != existing {
                        return Err(format!(
                            "path {value:?} is declared as both a file and directory"
                        ));
                    }
                }
                return Ok(());
            }
            node = child;
        }
        Ok(())
    }

    fn ensure_components(&mut self, components: &[PortablePathComponent<'_>]) -> usize {
        let mut node = 0;
        for component in components {
            node = self.ensure_child(node, component);
        }
        node
    }

    fn ensure_child(&mut self, parent: usize, component: &PortablePathComponent<'_>) -> usize {
        if let Some(&child) = self.nodes[parent].children.get(&component.key) {
            return child;
        }
        let child = self.nodes.len();
        self.nodes.push(PortablePathNode {
            children: BTreeMap::new(),
            spelling: component.spelling.to_string(),
            referenced_path: false,
            entry_kind: None,
        });
        self.nodes[parent]
            .children
            .insert(component.key.clone(), child);
        child
    }

    fn mark_materialized(
        &mut self,
        node: usize,
        is_root_entry: bool,
        kind: PortablePathEntryKind,
    ) -> bool {
        if self.nodes[node].entry_kind.is_some() {
            return false;
        }
        self.nodes[node].entry_kind = Some(kind);
        self.materialized_entries += 1;
        if is_root_entry {
            self.root_materialized_entries += 1;
        }
        true
    }
}

struct PortablePathComponent<'a> {
    spelling: &'a str,
    key: String,
}

fn portable_path_components(value: &str) -> Result<Vec<PortablePathComponent<'_>>, String> {
    validate_portable_relative_path(value)?;
    Ok(value
        .split('/')
        .map(|spelling| PortablePathComponent {
            spelling,
            key: portable_case_key(spelling),
        })
        .collect())
}

fn existing_prefix_spelling(
    components: &[PortablePathComponent<'_>],
    last: usize,
    existing_last: &str,
) -> String {
    let mut path = String::new();
    for component in &components[..last] {
        if !path.is_empty() {
            path.push('/');
        }
        path.push_str(component.spelling);
    }
    if !path.is_empty() {
        path.push('/');
    }
    path.push_str(existing_last);
    path
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

/// Find the first directory entry whose portable spelling aliases one of the
/// canonical protocol names. Callers provide entries in their deterministic
/// enumeration order and decide which domain-specific error type to return.
pub(crate) fn first_portable_name_alias<'a>(
    entries: impl IntoIterator<Item = &'a PathBuf>,
    canonical_names: &[&str],
) -> Option<(PathBuf, String)> {
    for entry in entries {
        let Some(actual) = entry.file_name().and_then(|name| name.to_str()) else {
            continue;
        };
        let actual_key = portable_case_key(actual);
        for canonical in canonical_names {
            if actual != *canonical && actual_key == portable_case_key(canonical) {
                return Some((entry.clone(), (*canonical).to_string()));
            }
        }
    }
    None
}

/// Read one directory as a bounded, normalized, duplicate-free child set.
/// Protocol discovery uses this before portable-name checks so virtual
/// filesystems cannot inject aliases through malformed enumeration results.
pub(crate) fn read_canonical_directory_entries<F: FileSystem>(
    fs: &F,
    directory: &Path,
) -> std::io::Result<Vec<PathBuf>> {
    let directory = normalize_fs_path(directory);
    let raw_entries = fs.read_dir(&directory)?;
    if raw_entries.len() > MAX_DIRECTORY_ENTRIES {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!("directory contains more than {MAX_DIRECTORY_ENTRIES} entries"),
        ));
    }
    let mut entries = Vec::new();
    entries
        .try_reserve(raw_entries.len())
        .map_err(|_| std::io::Error::other("failed to reserve canonical directory entry index"))?;
    for raw_entry in raw_entries {
        let entry = normalize_fs_path(&raw_entry);
        let parent = normalize_fs_path(entry.parent().unwrap_or_else(|| Path::new(".")));
        if parent != directory {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                format!(
                    "directory {} returned non-child entry {}",
                    directory.display(),
                    raw_entry.display(),
                ),
            ));
        }
        entries.push(entry);
    }
    sort_fs_paths(&mut entries);
    if let Some(duplicate) = entries
        .windows(2)
        .find_map(|pair| (pair[0] == pair[1]).then(|| pair[0].clone()))
    {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!(
                "directory {} returned duplicate entry {}",
                directory.display(),
                duplicate.display(),
            ),
        ));
    }
    Ok(entries)
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
    fn canonical_json_strings_have_one_byte_stable_spelling() {
        let value = "\"\\/\u{0000}\u{0008}\t\n\u{000b}\u{000c}\r\u{001f}\u{2028}\u{2029}数据";
        let expected = "\"\\\"\\\\/\\u0000\\b\\t\\n\\u000b\\f\\r\\u001f\u{2028}\u{2029}数据\"";
        let mut writer = CanonicalJsonWriter::new(expected.len()).unwrap();

        writer.push_string(value).unwrap();

        assert_eq!(writer.finish(), expected.as_bytes());
        assert!(expected.contains('/'));
        assert!(expected.contains('\u{2028}'));
        assert!(expected.contains('\u{2029}'));
        assert!(!expected.contains("\\u2028"));
        assert!(!expected.contains("\\u2029"));

        let mut bounded = CanonicalJsonWriter::new(expected.len() - 1).unwrap();
        let error = bounded.push_string(value).unwrap_err();
        assert!(error.contains("byte limit"), "{error}");
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
    fn portable_path_set_retains_components_without_complete_prefix_copies() {
        let components = (0..MAX_PORTABLE_PATH_COMPONENTS)
            .map(|index| format!("d{index:03}"))
            .collect::<Vec<_>>();
        let path = components.join("/");
        let mut paths = PortablePathSet::default();

        assert!(paths.insert_file(&path).unwrap());
        assert_eq!(paths.nodes.len(), components.len() + 1);
        assert_eq!(paths.materialized_entry_count(), components.len());
        assert_eq!(paths.root_materialized_entry_count(), 1);

        let stored_spelling_bytes = paths
            .nodes
            .iter()
            .map(|node| node.spelling.len())
            .sum::<usize>();
        let stored_key_bytes = paths
            .nodes
            .iter()
            .flat_map(|node| node.children.keys())
            .map(String::len)
            .sum::<usize>();
        let component_bytes = components.iter().map(String::len).sum::<usize>();
        assert_eq!(stored_spelling_bytes, component_bytes);
        assert_eq!(stored_key_bytes, component_bytes);

        assert!(!paths.insert_file(&path).unwrap());
        assert_eq!(paths.materialized_entry_count(), components.len());
    }

    #[test]
    fn portable_path_set_charges_complete_new_prefixes_atomically() {
        let mut paths = PortablePathSet {
            max_path_key_bytes: 10,
            ..PortablePathSet::default()
        };

        // New nodes `a` and `a/bb` charge 1 + 4 bytes.
        assert!(paths.insert_file("a/bb").unwrap());
        assert_eq!(paths.path_key_bytes(), 5);

        // The shared `a` node is already retained; only `a/cc` is new.
        assert!(paths.insert_file("a/cc").unwrap());
        assert_eq!(paths.path_key_bytes(), 9);
        assert!(!paths.insert_file("a/cc").unwrap());
        assert_eq!(paths.path_key_bytes(), 9);

        assert!(paths.insert_file("d").unwrap());
        assert_eq!(paths.path_key_bytes(), 10);
        let node_count = paths.nodes.len();
        let error = paths.insert_file("e").unwrap_err();
        assert!(error.contains("10-byte path-key limit"), "{error}");
        assert_eq!(paths.path_key_bytes(), 10);
        assert_eq!(paths.nodes.len(), node_count);
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
            "VO.PACKAGE.JSON",
            "vo.package.jſon",
            "vo.package.json/child",
            "SOURCE.TAR.GZ",
            "ſource.tar.gz",
            "source.tar.gz/child",
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
        assert!(!is_reserved_module_cache_path("docs/vo.package.json"));
        assert!(!is_reserved_module_cache_path("docs/source.tar.gz"));
    }
}
