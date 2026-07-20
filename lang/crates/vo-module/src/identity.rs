use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::cmp::Ordering;
use std::fmt;

use crate::version::ExactVersion;
use crate::Error;

/// Reserved prefix for unpublished project, workspace, and bundle modules.
/// Registry and release boundaries reject this namespace.
pub const LOCAL_NAMESPACE_PREFIX: &str = "local/";

/// Maximum byte length of a release tag produced from validated protocol
/// identities. The module-root contribution is bounded by the complete
/// canonical module path, and exact versions are one byte below the portable
/// component limit.
pub const MAX_RELEASE_TAG_BYTES: usize = vo_common::abi::MAX_CANONICAL_MODULE_OWNER_BYTES
    + 2
    + (crate::schema::MAX_PORTABLE_PATH_COMPONENT_BYTES - 1);

// ============================================================
// ModulePath — canonical module path per spec §2
// ============================================================

/// A validated, transport-neutral public module path.
/// Format: `<dns-host>/<owner>/<repository>[/<subdir>...][/vN]`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModulePath {
    raw: String,
    local: bool,
    /// Index after the host and its following slash.
    owner_start: usize,
    /// Index of '/' after owner
    repo_start: usize,
    /// Parsed major version suffix, if any
    major: Option<u64>,
}

impl ModulePath {
    /// Parse and validate a canonical module path.
    pub fn parse(s: &str) -> Result<Self, Error> {
        if s.starts_with(LOCAL_NAMESPACE_PREFIX) {
            LocalName::parse(s)?;
            return Ok(Self {
                raw: s.to_string(),
                local: true,
                owner_start: 0,
                repo_start: 0,
                major: None,
            });
        }
        vo_common::abi::validate_canonical_module_owner(s)
            .map_err(|error| Error::InvalidModulePath(format!("{error}: {s}")))?;
        let segments: Vec<&str> = s.split('/').collect();

        let owner_start = segments[0].len() + 1;
        let repo_start = owner_start + segments[1].len() + 1;

        // A major-version suffix is a path segment *after* the repository.
        // Repository names such as `v1` and `v2` are ordinary unsuffixed
        // module roots and must never be reinterpreted as suffixes.
        let major = if segments.len() > 3 {
            parse_major_suffix(segments.last().unwrap())?
        } else {
            None
        };
        if let Some(n) = major {
            if n < 2 {
                return Err(Error::InvalidModulePath(format!(
                    "major version suffix must be >= v2, got v{n} in: {s}"
                )));
            }
        }

        Ok(ModulePath {
            raw: s.to_string(),
            local: false,
            owner_start,
            repo_start,
            major,
        })
    }

    pub fn host(&self) -> &str {
        if self.local {
            return "local";
        }
        &self.raw[..self.owner_start - 1]
    }

    pub fn is_local(&self) -> bool {
        self.local
    }

    pub fn as_str(&self) -> &str {
        &self.raw
    }

    pub fn owner(&self) -> &str {
        assert!(!self.local, "local ModuleId has no registry owner");
        let rest = &self.raw[self.owner_start..];
        rest.split('/').next().unwrap()
    }

    pub fn repo(&self) -> &str {
        assert!(!self.local, "local ModuleId has no registry repository");
        let rest = &self.raw[self.repo_start..];
        rest.split('/').next().unwrap()
    }

    /// The module root relative to the repository root.
    /// For `github.com/acme/lib` → "."
    /// For `github.com/acme/mono/graphics` → "graphics"
    /// For `github.com/acme/mono/graphics/v2` → "graphics/v2"
    pub fn module_root(&self) -> &str {
        assert!(!self.local, "local ModuleId has no registry module root");
        let after_repo = self.repo_start + self.repo().len();
        if after_repo >= self.raw.len() {
            "."
        } else {
            &self.raw[after_repo + 1..]
        }
    }

    /// Returns the major version suffix N if the path ends in `/vN` (N >= 2).
    pub fn major_suffix(&self) -> Option<u64> {
        self.major
    }

    /// Check if this module path owns the given import path.
    /// Returns the sub-path within the module (empty string for exact match),
    /// or `None` if this module does not own the import.
    pub fn owns_import<'a>(&self, import_path: &'a str) -> Option<&'a str> {
        let mp = self.as_str();
        if import_path == mp {
            Some("")
        } else if import_path.starts_with(mp) && import_path.as_bytes().get(mp.len()) == Some(&b'/')
        {
            Some(&import_path[mp.len() + 1..])
        } else {
            None
        }
    }

    /// Check if a version is compatible with this module path's major version rule.
    /// Unsuffixed paths accept v0.x.x and v1.x.x.
    /// Suffixed paths (e.g. /v2) accept only that major version.
    pub fn accepts_version(&self, v: &ExactVersion) -> bool {
        if self.local {
            return true;
        }
        let sv = v.semver();
        match self.major {
            None => sv.major() <= 1,
            Some(n) => sv.major() == n,
        }
    }

    /// Compute the Git tag for a given version published under this module path.
    /// Root modules: "vX.Y.Z"
    /// Non-root modules: "<module_root>/vX.Y.Z"
    pub fn version_tag(&self, version: &ExactVersion) -> String {
        assert!(!self.local, "local ModuleId cannot have a release tag");
        let root = self.module_root();
        let tag = if root == "." {
            format!("v{version}")
        } else {
            format!("{root}/v{version}")
        };
        debug_assert!(tag.len() <= MAX_RELEASE_TAG_BYTES);
        tag
    }
}

impl PartialOrd for ModulePath {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ModulePath {
    fn cmp(&self, other: &Self) -> Ordering {
        self.raw.cmp(&other.raw)
    }
}

impl fmt::Display for ModulePath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.raw)
    }
}

impl Serialize for ModulePath {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.raw)
    }
}

impl<'de> Deserialize<'de> for ModulePath {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let raw = String::deserialize(deserializer)?;
        ModulePath::parse(&raw).map_err(serde::de::Error::custom)
    }
}

/// If a post-repository segment matches `vN` where N >= 2, returns Some(N).
/// Returns None if not a version suffix.
/// Returns Err for invalid suffixes like v0, v1, v02.
fn parse_major_suffix(seg: &str) -> Result<Option<u64>, Error> {
    if !seg.starts_with('v') {
        return Ok(None);
    }
    let rest = &seg[1..];
    if rest.is_empty() || !rest.chars().all(|c| c.is_ascii_digit()) {
        return Ok(None); // not a version suffix, just a segment starting with 'v'
    }
    // Has leading zero?
    if rest.len() > 1 && rest.starts_with('0') {
        return Err(Error::InvalidModulePath(format!(
            "zero-padded major suffix '{seg}' is invalid"
        )));
    }
    let n: u64 = rest
        .parse()
        .map_err(|_| Error::InvalidModulePath(format!("invalid major suffix '{seg}'")))?;
    Ok(Some(n))
}

// ============================================================
// Import classification — spec §4.3
// ============================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ImportClass {
    Stdlib,
    External,
}

fn validate_import_path_shape(path: &str) -> Result<(), Error> {
    crate::schema::validate_portable_relative_path(path)
        .map_err(|detail| Error::InvalidImportPath(format!("{detail}: {path}")))?;
    if path.contains('@') {
        return Err(Error::InvalidImportPath(format!(
            "'@' is not part of import syntax: {path}"
        )));
    }
    Ok(())
}

/// Classify an import path. If the first segment contains `.`, it is an
/// external host-qualified import; otherwise it is a standard-library import.
/// Otherwise it is stdlib.
pub fn classify_import(path: &str) -> Result<ImportClass, Error> {
    if path.is_empty() {
        return Err(Error::InvalidImportPath("empty import path".to_string()));
    }
    if path.starts_with('/') || path.starts_with('.') {
        return Err(Error::InvalidImportPath(format!(
            "relative or absolute import paths are not allowed: {path}"
        )));
    }
    validate_import_path_shape(path)?;
    if path == "local" {
        return Err(Error::InvalidImportPath(
            "local imports require a module name".to_string(),
        ));
    }
    if path.starts_with(LOCAL_NAMESPACE_PREFIX) {
        let module_end = path[LOCAL_NAMESPACE_PREFIX.len()..]
            .find('/')
            .map(|offset| LOCAL_NAMESPACE_PREFIX.len() + offset)
            .unwrap_or(path.len());
        ModulePath::parse(&path[..module_end]).map_err(|error| {
            Error::InvalidImportPath(format!("invalid local module import {path}: {error}"))
        })?;
        return Ok(ImportClass::External);
    }
    let first_segment = path.split('/').next().unwrap();
    if first_segment.contains('.') {
        let mut segments = path.split('/');
        let host = segments.next().unwrap_or_default();
        let owner = segments.next().unwrap_or_default();
        let repository = segments.next().unwrap_or_default();
        if owner.is_empty() || repository.is_empty() {
            return Err(Error::InvalidImportPath(format!(
                "external import must contain <host>/<owner>/<repository>: {path}"
            )));
        }
        let root = format!("{host}/{owner}/{repository}");
        ModulePath::parse(&root).map_err(|error| {
            Error::InvalidImportPath(format!(
                "external import has a non-canonical module root: {path}: {error}"
            ))
        })?;
        Ok(ImportClass::External)
    } else {
        // Check for explicitly banned stdlib prefix
        if path == "std" || path.starts_with("std/") {
            return Err(Error::InvalidImportPath(format!(
                "'std/...' import prefix is not allowed, use bare package name: {path}"
            )));
        }
        Ok(ImportClass::Stdlib)
    }
}

/// Check internal package visibility per spec §9.4.
/// Returns true if `importer_path` is allowed to import `target_path`.
pub fn check_internal_visibility(importer_path: &str, target_path: &str) -> bool {
    let importer_is_canonical =
        vo_common::abi::validate_canonical_package_path(importer_path).is_ok();
    if !importer_is_canonical || classify_import(target_path).is_err() {
        return false;
    }
    // Scan normalized components so a terminal `internal` is considered
    // together with earlier boundaries. The last component match is the
    // authoritative (deepest) boundary.
    let mut component_start = 0usize;
    let mut required_prefix = None;
    for component in target_path.split('/') {
        if component == "internal" {
            let prefix_end = component_start.saturating_sub(usize::from(component_start > 0));
            required_prefix = Some(&target_path[..prefix_end]);
        }
        component_start += component.len() + 1;
    }
    let Some(required_prefix) = required_prefix else {
        return true;
    };
    required_prefix.is_empty()
        || (importer_path.starts_with(required_prefix)
            && (importer_path.len() == required_prefix.len()
                || importer_path.as_bytes().get(required_prefix.len()) == Some(&b'/')))
}

// ============================================================
// LocalName — ephemeral single-file module name (spec §3.5, §5.6.2)
// ============================================================

/// A validated `local/<name>` identity for an ephemeral single-file module.
///
/// `<name>` MUST match `[a-z0-9][a-z0-9._-]*` and one canonical portable
/// path component.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LocalName {
    raw: String,
}

impl LocalName {
    pub fn parse(s: &str) -> Result<Self, Error> {
        let Some(name) = s.strip_prefix(LOCAL_NAMESPACE_PREFIX) else {
            return Err(Error::InvalidModulePath(format!(
                "local module path must start with '{LOCAL_NAMESPACE_PREFIX}': {s}"
            )));
        };
        if name.is_empty() {
            return Err(Error::InvalidModulePath(format!(
                "local module path must have a name after '{LOCAL_NAMESPACE_PREFIX}': {s}"
            )));
        }
        if name.contains('/') {
            return Err(Error::InvalidModulePath(format!(
                "local module path must not contain '/' after '{LOCAL_NAMESPACE_PREFIX}': {s}"
            )));
        }
        validate_local_name_segment(name, s)?;
        Ok(LocalName { raw: s.to_string() })
    }

    pub fn as_str(&self) -> &str {
        &self.raw
    }

    /// The `<name>` portion after the `local/` prefix.
    pub fn name(&self) -> &str {
        &self.raw[LOCAL_NAMESPACE_PREFIX.len()..]
    }
}

impl PartialOrd for LocalName {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for LocalName {
    fn cmp(&self, other: &Self) -> Ordering {
        self.raw.cmp(&other.raw)
    }
}

impl fmt::Display for LocalName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.raw)
    }
}

impl Serialize for LocalName {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.raw)
    }
}

impl<'de> Deserialize<'de> for LocalName {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let raw = String::deserialize(deserializer)?;
        LocalName::parse(&raw).map_err(serde::de::Error::custom)
    }
}

fn validate_local_name_segment(name: &str, full: &str) -> Result<(), Error> {
    let mut chars = name.chars();
    let Some(first) = chars.next() else {
        return Err(Error::InvalidModulePath(format!(
            "local name is empty in: {full}"
        )));
    };
    if !(first.is_ascii_lowercase() || first.is_ascii_digit()) {
        return Err(Error::InvalidModulePath(format!(
            "local name '{name}' must start with [a-z0-9] in: {full}"
        )));
    }
    for c in std::iter::once(first).chain(chars) {
        if !(c.is_ascii_lowercase() || c.is_ascii_digit() || c == '.' || c == '_' || c == '-') {
            return Err(Error::InvalidModulePath(format!(
                "local name '{name}' contains invalid character '{c}' in: {full}"
            )));
        }
    }
    crate::schema::validate_portable_path_component(name).map_err(|detail| {
        Error::InvalidModulePath(format!(
            "local name '{name}' must be a canonical portable path component ({detail}) in: {full}"
        ))
    })?;
    Ok(())
}

// ============================================================
// ModIdentity — root-module identity for `vo.mod` / `vo.lock`
// ============================================================

/// Identity of a module root. `Public` covers transport-neutral, host-qualified
/// identities. `Local` covers the unpublished workspace/bundle namespace.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ModIdentity {
    Public(ModulePath),
    Local(LocalName),
}

impl ModIdentity {
    /// Parse a root-module identity from its canonical string form.
    ///
    /// Accepts either `local/<name>` or a host-qualified public ModuleId.
    pub fn parse(s: &str) -> Result<Self, Error> {
        if s.starts_with(LOCAL_NAMESPACE_PREFIX) {
            return LocalName::parse(s).map(ModIdentity::Local);
        }
        ModulePath::parse(s).map(ModIdentity::Public)
    }

    /// Canonical string form (e.g. `github.com/acme/app` or `local/gui_chat`).
    pub fn as_str(&self) -> &str {
        match self {
            ModIdentity::Public(mp) => mp.as_str(),
            ModIdentity::Local(name) => name.as_str(),
        }
    }

    pub fn is_local(&self) -> bool {
        matches!(self, ModIdentity::Local(_))
    }

    pub fn is_public(&self) -> bool {
        matches!(self, ModIdentity::Public(_))
    }

    /// Borrow the underlying public ModuleId, if any.
    pub fn as_public(&self) -> Option<&ModulePath> {
        match self {
            ModIdentity::Public(mp) => Some(mp),
            ModIdentity::Local(_) => None,
        }
    }

    /// Borrow the underlying local name, if any.
    pub fn as_local(&self) -> Option<&LocalName> {
        match self {
            ModIdentity::Local(name) => Some(name),
            ModIdentity::Public(_) => None,
        }
    }
}

impl PartialOrd for ModIdentity {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ModIdentity {
    fn cmp(&self, other: &Self) -> Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl fmt::Display for ModIdentity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl From<ModulePath> for ModIdentity {
    fn from(mp: ModulePath) -> Self {
        if mp.is_local() {
            ModIdentity::Local(LocalName::parse(mp.as_str()).expect("validated local ModuleId"))
        } else {
            ModIdentity::Public(mp)
        }
    }
}

impl From<LocalName> for ModIdentity {
    fn from(name: LocalName) -> Self {
        ModIdentity::Local(name)
    }
}

impl Serialize for ModIdentity {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(self.as_str())
    }
}

impl<'de> Deserialize<'de> for ModIdentity {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let raw = String::deserialize(deserializer)?;
        ModIdentity::parse(&raw).map_err(serde::de::Error::custom)
    }
}

// ============================================================
// ArtifactId — (kind, target, name) identity tuple
// ============================================================

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub struct ArtifactId {
    pub kind: String,
    pub target: String,
    pub name: String,
}

impl<'de> Deserialize<'de> for ArtifactId {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(deny_unknown_fields)]
        struct RawArtifactId {
            kind: String,
            target: String,
            name: String,
        }

        let raw = RawArtifactId::deserialize(deserializer)?;
        let artifact = Self {
            kind: raw.kind,
            target: raw.target,
            name: raw.name,
        };
        artifact.validate().map_err(serde::de::Error::custom)?;
        Ok(artifact)
    }
}

impl ArtifactId {
    /// Validate the canonical identity tuple before it is used in cache paths
    /// or registry requests.
    pub fn validate(&self) -> Result<(), String> {
        crate::schema::validate_file_name(&self.name)
            .map_err(|error| format!("artifact name {error}"))?;
        crate::schema::validate_portable_path_component(&self.target)
            .map_err(|_| "artifact target must be a portable path component".to_string())?;

        match self.kind.as_str() {
            "extension-native" => {
                validate_rust_target_triple(&self.target)?;
                if self.target.starts_with("wasm32-") || self.target.starts_with("wasm64-") {
                    Err(
                        "native artifact target must identify a native host target triple"
                            .to_string(),
                    )
                } else {
                    Ok(())
                }
            }
            "extension-wasm" | "extension-js-glue" => {
                if self.target == "wasm32-unknown-unknown" {
                    Ok(())
                } else {
                    Err(format!(
                        "{} artifacts must target wasm32-unknown-unknown",
                        self.kind
                    ))
                }
            }
            kind => Err(format!("unsupported artifact kind: {kind}")),
        }
    }
}

fn validate_rust_target_triple(value: &str) -> Result<(), String> {
    if value.matches('-').count() < 2
        || value.split('-').any(|segment| {
            segment.is_empty()
                || !segment.chars().all(|ch| {
                    ch.is_ascii_lowercase() || ch.is_ascii_digit() || ch == '_' || ch == '.'
                })
        })
    {
        return Err("native artifact target must be a canonical Rust target triple".to_string());
    }
    Ok(())
}

impl PartialOrd for ArtifactId {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ArtifactId {
    fn cmp(&self, other: &Self) -> Ordering {
        self.kind
            .cmp(&other.kind)
            .then(self.target.cmp(&other.target))
            .then(self.name.cmp(&other.name))
    }
}

impl fmt::Display for ArtifactId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.kind, self.target, self.name)
    }
}

// ============================================================
// Import resolution helpers — spec §9.1
// ============================================================

/// Given an import path and a set of module paths, find the owning module
/// by longest prefix-boundary match.
/// Returns (module_path, package_subpath) or None.
///
/// Accepts any iterable of `&ModulePath` — slices, mapped iterators over
/// structs containing a `ModulePath` field, etc.
pub fn find_owning_module<'m, 'i>(
    import_path: &'i str,
    modules: impl IntoIterator<Item = &'m ModulePath>,
) -> Option<(&'m ModulePath, &'i str)> {
    if classify_import(import_path).ok()? != ImportClass::External {
        return None;
    }
    let mut best: Option<(&ModulePath, &str)> = None;
    for mp in modules {
        if let Some(sub) = mp.owns_import(import_path) {
            let dominated = best
                .as_ref()
                .is_some_and(|(b, _)| b.as_str().len() >= mp.as_str().len());
            if !dominated {
                best = Some((mp, sub));
            }
        }
    }
    best
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cache_key_no_collision() {
        // Module paths with underscores must not collide after encoding.
        let a = ModulePath::parse("github.com/acme/b_c").unwrap();
        let b = ModulePath::parse("github.com/acme_b/c").unwrap();
        let encoded_a = a.as_str().replace('/', "@");
        let encoded_b = b.as_str().replace('/', "@");
        assert_ne!(encoded_a, encoded_b);
    }

    #[test]
    fn artifact_targets_require_canonical_lowercase_portable_triples() {
        let artifact = ArtifactId {
            kind: "extension-native".to_string(),
            target: "AARCH64-apple-darwin".to_string(),
            name: "libdemo.dylib".to_string(),
        };
        assert!(artifact.validate().is_err());

        let artifact = ArtifactId {
            target: "aarch64-apple-darwin.".to_string(),
            ..artifact
        };
        assert!(artifact.validate().is_err());

        for target in ["wasm32-unknown-unknown", "wasm64-unknown-unknown"] {
            let artifact = ArtifactId {
                kind: "extension-native".to_string(),
                target: target.to_string(),
                name: "demo.wasm".to_string(),
            };
            let error = artifact.validate().unwrap_err();
            assert!(
                error.contains("native host target triple"),
                "{target}: {error}"
            );
        }
    }

    #[test]
    fn artifact_deserialization_enforces_identity_invariants() {
        let valid = serde_json::from_str::<ArtifactId>(
            r#"{"kind":"extension-wasm","target":"wasm32-unknown-unknown","name":"demo.wasm"}"#,
        )
        .unwrap();
        assert_eq!(valid.name, "demo.wasm");

        for invalid in [
            r#"{"kind":"extension-wasm","target":"wasm32-unknown-unknown","name":"../demo.wasm"}"#,
            r#"{"kind":"extension-wasm","target":"aarch64-apple-darwin","name":"demo.wasm"}"#,
            r#"{"kind":"extension-native","target":"wasm32-unknown-unknown","name":"demo.wasm"}"#,
            r#"{"kind":"extension-native","target":"wasm64-unknown-unknown","name":"demo.wasm"}"#,
            r#"{"kind":"unknown","target":"wasm32-unknown-unknown","name":"demo.wasm"}"#,
            r#"{"kind":"extension-wasm","target":"wasm32-unknown-unknown","name":"demo.wasm","future":true}"#,
        ] {
            assert!(
                serde_json::from_str::<ArtifactId>(invalid).is_err(),
                "{invalid}"
            );
        }
    }

    // --- ModulePath ---

    #[test]
    fn test_module_path_basic() {
        let mp = ModulePath::parse("github.com/vo-lang/vogui").unwrap();
        assert_eq!(mp.owner(), "vo-lang");
        assert_eq!(mp.repo(), "vogui");
        assert_eq!(mp.module_root(), ".");
        assert_eq!(mp.major_suffix(), None);
    }

    #[test]
    fn test_module_path_subdir() {
        let mp = ModulePath::parse("github.com/vo-lang/mono/graphics").unwrap();
        assert_eq!(mp.owner(), "vo-lang");
        assert_eq!(mp.repo(), "mono");
        assert_eq!(mp.module_root(), "graphics");
    }

    #[test]
    fn test_module_path_major_suffix() {
        let mp = ModulePath::parse("github.com/acme/lib/v2").unwrap();
        assert_eq!(mp.major_suffix(), Some(2));
        assert_eq!(mp.module_root(), "v2");
    }

    #[test]
    fn repository_name_is_never_a_major_suffix() {
        for repository in ["v0", "v1", "v2", "v02"] {
            let module = ModulePath::parse(&format!("github.com/acme/{repository}"))
                .unwrap_or_else(|error| panic!("rejected repository {repository:?}: {error}"));
            assert_eq!(module.repo(), repository);
            assert_eq!(module.module_root(), ".");
            assert_eq!(module.major_suffix(), None);
            assert!(module.accepts_version(&ExactVersion::parse("1.2.3").unwrap()));
            assert_eq!(
                module.version_tag(&ExactVersion::parse("1.2.3").unwrap()),
                "v1.2.3"
            );
        }
    }

    #[test]
    fn test_module_path_subdir_with_major() {
        let mp = ModulePath::parse("github.com/acme/mono/graphics/v2").unwrap();
        assert_eq!(mp.module_root(), "graphics/v2");
        assert_eq!(mp.major_suffix(), Some(2));
    }

    #[test]
    fn test_module_path_reject_v0_suffix() {
        assert!(ModulePath::parse("github.com/acme/lib/v0").is_err());
    }

    #[test]
    fn test_module_path_reject_v1_suffix() {
        assert!(ModulePath::parse("github.com/acme/lib/v1").is_err());
    }

    #[test]
    fn test_module_path_reject_uppercase() {
        assert!(ModulePath::parse("github.com/Acme/lib").is_err());
    }

    #[test]
    fn test_module_path_reject_relative() {
        assert!(ModulePath::parse("/github.com/acme/lib").is_err());
    }

    #[test]
    fn test_module_path_accepts_transport_neutral_hosts() {
        let module = ModulePath::parse("gitlab.com/acme/lib").unwrap();
        assert_eq!(module.host(), "gitlab.com");
        assert_eq!(module.owner(), "acme");
        assert_eq!(module.repo(), "lib");
    }

    #[test]
    fn module_path_rejects_ambiguous_or_oversized_cache_components() {
        for path in [
            "github.com/acme/.",
            "github.com/acme/../lib",
            "github.com/acme/lib.",
        ] {
            assert!(ModulePath::parse(path).is_err(), "{path}");
        }

        let prefix = "github.com/a/";
        let maximum = format!(
            "{prefix}{}",
            "r".repeat(crate::schema::MAX_PORTABLE_PATH_COMPONENT_BYTES - prefix.len())
        );
        assert_eq!(
            maximum.len(),
            crate::schema::MAX_PORTABLE_PATH_COMPONENT_BYTES
        );
        assert!(ModulePath::parse(&maximum).is_ok());
        assert!(ModulePath::parse(&format!("{maximum}r")).is_err());
    }

    #[test]
    fn public_module_identity_is_independent_of_git_reference_spelling() {
        let version = ExactVersion::parse("1.2.3").unwrap();
        for repository in ["foo..bar", "foo.lock"] {
            let module = ModulePath::parse(&format!("github.com/acme/{repository}")).unwrap();
            assert_eq!(module.version_tag(&version), "v1.2.3");
        }
        for path in [
            "github.com/acme/repository/foo..bar",
            "github.com/acme/repository/foo.lock",
        ] {
            assert!(ModulePath::parse(path).is_ok(), "{path}");
        }
    }

    #[test]
    fn test_module_path_accepts_version() {
        let unsuffixed = ModulePath::parse("github.com/acme/lib").unwrap();
        assert!(unsuffixed.accepts_version(&ExactVersion::parse("0.1.0").unwrap()));
        assert!(unsuffixed.accepts_version(&ExactVersion::parse("1.5.0").unwrap()));
        assert!(!unsuffixed.accepts_version(&ExactVersion::parse("2.0.0").unwrap()));

        let suffixed = ModulePath::parse("github.com/acme/lib/v2").unwrap();
        assert!(!suffixed.accepts_version(&ExactVersion::parse("1.0.0").unwrap()));
        assert!(suffixed.accepts_version(&ExactVersion::parse("2.0.0").unwrap()));
        assert!(suffixed.accepts_version(&ExactVersion::parse("2.3.1").unwrap()));
        assert!(!suffixed.accepts_version(&ExactVersion::parse("3.0.0").unwrap()));
    }

    #[test]
    fn test_version_tag() {
        let root = ModulePath::parse("github.com/acme/lib").unwrap();
        assert_eq!(
            root.version_tag(&ExactVersion::parse("1.4.2").unwrap()),
            "v1.4.2"
        );

        let nested = ModulePath::parse("github.com/acme/mono/graphics").unwrap();
        assert_eq!(
            nested.version_tag(&ExactVersion::parse("0.8.0").unwrap()),
            "graphics/v0.8.0"
        );

        let nested_v2 = ModulePath::parse("github.com/acme/mono/graphics/v2").unwrap();
        assert_eq!(
            nested_v2.version_tag(&ExactVersion::parse("2.1.0").unwrap()),
            "graphics/v2/v2.1.0"
        );
        assert!(
            nested_v2
                .version_tag(&ExactVersion::parse("2.1.0").unwrap())
                .len()
                <= MAX_RELEASE_TAG_BYTES
        );
        assert_eq!(
            MAX_RELEASE_TAG_BYTES,
            vo_common::abi::MAX_CANONICAL_MODULE_OWNER_BYTES
                + 2
                + crate::schema::MAX_PORTABLE_PATH_COMPONENT_BYTES
                - 1
        );
    }

    // --- Import classification ---

    #[test]
    fn test_classify_stdlib() {
        assert_eq!(classify_import("fmt").unwrap(), ImportClass::Stdlib);
        assert_eq!(
            classify_import("encoding/json").unwrap(),
            ImportClass::Stdlib
        );
    }

    #[test]
    fn test_classify_external() {
        assert_eq!(
            classify_import("github.com/acme/lib/util").unwrap(),
            ImportClass::External
        );
    }

    #[test]
    fn test_classify_reject_relative() {
        assert!(classify_import("./util").is_err());
        assert!(classify_import("../shared").is_err());
    }

    #[test]
    fn test_classify_reject_absolute() {
        assert!(classify_import("/tmp/pkg").is_err());
    }

    #[test]
    fn test_classify_reject_at_sign() {
        assert!(classify_import("github.com/acme/lib@v1.2.3").is_err());
    }

    #[test]
    fn test_classify_accepts_transport_neutral_external_import() {
        assert_eq!(
            classify_import("example.com/acme/lib").unwrap(),
            ImportClass::External
        );
    }

    #[test]
    fn test_classify_reject_std_prefix() {
        assert!(classify_import("std/fmt").is_err());
    }

    #[test]
    fn test_classify_accepts_local_module_imports() {
        assert_eq!(
            classify_import("local/demo").unwrap(),
            ImportClass::External
        );
        assert_eq!(
            classify_import("local/demo/subpackage").unwrap(),
            ImportClass::External
        );
        assert!(classify_import("local").is_err());
    }

    #[test]
    fn runtime_package_identity_validation_matches_the_module_frontend() {
        for path in [
            "fmt",
            "encoding/json",
            "github.com/acme/lib",
            "example.com/acme/lib",
            "local/scratch-1",
            "github.com/acme/v1",
            "github.com/acme/lib/graphics/\u{00e9}",
        ] {
            assert!(classify_import(path).is_ok(), "frontend rejected {path:?}");
            assert!(
                vo_common::abi::validate_canonical_package_path(path).is_ok(),
                "runtime identity rejected {path:?}"
            );
        }

        for path in [
            "",
            "std/fmt",
            "github.com/acme",
            "github.com/Acme/lib",
            "github.com/acme/lib/e\u{301}",
            "github.com/acme/lib/COM\u{00b9}.txt",
        ] {
            assert!(classify_import(path).is_err(), "frontend accepted {path:?}");
            assert!(
                vo_common::abi::validate_canonical_package_path(path).is_err(),
                "runtime identity accepted {path:?}"
            );
        }

        for path in ["local/scratch-1", "local/demo_name.v1"] {
            assert!(LocalName::parse(path).is_ok(), "frontend rejected {path:?}");
            assert!(
                vo_common::abi::validate_canonical_package_path(path).is_ok(),
                "runtime identity rejected {path:?}"
            );
        }
        for path in [
            "local",
            "local/Upper",
            "local/trailing.",
            "local/con",
            "local/com1.txt",
        ] {
            assert!(
                LocalName::parse(path).is_err(),
                "frontend accepted {path:?}"
            );
            assert!(
                vo_common::abi::validate_canonical_package_path(path).is_err(),
                "runtime identity accepted {path:?}"
            );
        }
        assert!(LocalName::parse("local/name/child").is_err());
        assert!(classify_import("local/name/child").is_ok());
        assert!(vo_common::abi::validate_canonical_package_path("local/name/child").is_ok());
        assert!(LocalName::parse(&format!("local/{}", "a".repeat(255))).is_ok());
        assert!(LocalName::parse(&format!("local/{}", "a".repeat(256))).is_err());
    }

    #[test]
    fn test_classify_rejects_noncanonical_path_structure() {
        for path in [
            "encoding//json",
            "encoding/./json",
            "encoding/../json",
            "encoding\\json",
            "encoding/json/",
            " encoding/json",
            "encoding/json ",
            "encoding/CON",
            "encoding/e\u{301}",
            "github.com/acme/lib/../../other",
            "github.com/acme/lib/<bad>",
        ] {
            assert!(
                classify_import(path).is_err(),
                "non-canonical import path {path:?} must be rejected"
            );
        }
    }

    #[test]
    fn test_classify_external_requires_canonical_root() {
        for path in [
            "github.com/acme",
            "github.com/Acme/lib",
            "github.com/acme/Lib",
            "github.com/con/lib",
            "github.com/acme/com1",
        ] {
            assert!(
                classify_import(path).is_err(),
                "non-canonical external root {path:?} must be rejected"
            );
        }
        assert_eq!(
            classify_import("github.com/acme/lib/\u{56fe}\u{5f62}/\u{00e9}").unwrap(),
            ImportClass::External
        );
    }

    #[test]
    fn test_classify_rejects_import_path_size_limits() {
        let oversized_component = format!(
            "encoding/{}",
            "x".repeat(crate::schema::MAX_PORTABLE_PATH_COMPONENT_BYTES + 1)
        );
        assert!(classify_import(&oversized_component).is_err());

        let oversized_path = (0..crate::schema::MAX_PORTABLE_PATH_BYTES)
            .map(|_| "x")
            .collect::<Vec<_>>()
            .join("/");
        assert!(classify_import(&oversized_path).is_err());
    }

    // --- Internal visibility ---

    #[test]
    fn test_internal_visibility_allowed() {
        assert!(check_internal_visibility(
            "github.com/acme/app/cmd/tool",
            "github.com/acme/app/internal/cache"
        ));
    }

    #[test]
    fn test_internal_visibility_denied() {
        assert!(!check_internal_visibility(
            "github.com/other/project/tool",
            "github.com/acme/app/internal/cache"
        ));
    }

    #[test]
    fn test_internal_visibility_no_marker() {
        assert!(check_internal_visibility(
            "github.com/x/y",
            "github.com/a/b/util"
        ));
        assert!(!check_internal_visibility(
            "github.com/acme/app",
            "github.com/acme/app/../secret"
        ));
        assert!(!check_internal_visibility(
            "github.com/acme/app/../tool",
            "github.com/acme/app/util"
        ));
        assert!(check_internal_visibility(
            "local/demo",
            "github.com/acme/app/util"
        ));
    }

    #[test]
    fn test_nested_internal_visibility_uses_deepest_boundary() {
        let target = "github.com/acme/app/internal/feature/internal/secret";
        assert!(!check_internal_visibility(
            "github.com/acme/app/cmd/tool",
            target
        ));
        assert!(check_internal_visibility(
            "github.com/acme/app/internal/feature/cmd/tool",
            target
        ));

        let terminal_target = "github.com/acme/app/internal/feature/internal";
        assert!(!check_internal_visibility(
            "github.com/acme/app/cmd/tool",
            terminal_target
        ));
        assert!(check_internal_visibility(
            "github.com/acme/app/internal/feature/cmd/tool",
            terminal_target
        ));
        assert!(check_internal_visibility(
            "github.com/acme/app/cmd/tool",
            "github.com/acme/app/internalized/cache"
        ));
    }

    // --- find_owning_module ---

    #[test]
    fn test_find_owning_module() {
        let modules = vec![
            ModulePath::parse("github.com/acme/app").unwrap(),
            ModulePath::parse("github.com/vo-lang/vogui").unwrap(),
        ];
        let (owner, sub) = find_owning_module("github.com/acme/app/util", &modules).unwrap();
        assert_eq!(owner.as_str(), "github.com/acme/app");
        assert_eq!(sub, "util");

        let (owner, sub) = find_owning_module("github.com/vo-lang/vogui/widget", &modules).unwrap();
        assert_eq!(owner.as_str(), "github.com/vo-lang/vogui");
        assert_eq!(sub, "widget");

        let (owner, sub) = find_owning_module("github.com/acme/app", &modules).unwrap();
        assert_eq!(owner.as_str(), "github.com/acme/app");
        assert_eq!(sub, "");
    }

    #[test]
    fn test_find_owning_module_none() {
        let modules = vec![ModulePath::parse("github.com/acme/app").unwrap()];
        assert!(find_owning_module("github.com/other/lib/util", &modules).is_none());
        assert!(find_owning_module("github.com/acme/app/../other", &modules).is_none());
        assert!(find_owning_module("fmt", &modules).is_none());
    }
}
