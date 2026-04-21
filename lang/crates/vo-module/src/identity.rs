use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::cmp::Ordering;
use std::fmt;

use crate::version::ExactVersion;
use crate::Error;

/// Reserved prefix (spec §3.5) that introduces an ephemeral single-file
/// module identity. `local/<name>` paths are only valid as the `module`
/// directive of an inline `/*vo:mod*/` block (or of a toolchain-synthesized
/// ephemeral `vo.mod` that mirrors it); they MUST NOT appear in import
/// statements, `require` entries, or published registry metadata.
pub const LOCAL_NAMESPACE_PREFIX: &str = "local/";

// ============================================================
// ModulePath — canonical module path per spec §2
// ============================================================

/// A validated canonical module path.
/// Format: `github.com/<owner>/<repo>[/<subdir>...][/vN]`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModulePath {
    raw: String,
    /// Index of '/' after "github.com"
    owner_start: usize,
    /// Index of '/' after owner
    repo_start: usize,
    /// Parsed major version suffix, if any
    major: Option<u64>,
}

impl ModulePath {
    /// Parse and validate a canonical module path.
    pub fn parse(s: &str) -> Result<Self, Error> {
        if s.is_empty() {
            return Err(Error::InvalidModulePath("empty module path".into()));
        }
        if s.starts_with('/') || s.ends_with('/') {
            return Err(Error::InvalidModulePath(format!(
                "module path must not start or end with '/': {s}"
            )));
        }
        if !s.starts_with("github.com/") {
            return Err(Error::InvalidModulePath(format!(
                "module path must start with 'github.com/': {s}"
            )));
        }

        let segments: Vec<&str> = s.split('/').collect();
        // Minimum: github.com / owner / repo = 3 segments
        if segments.len() < 3 {
            return Err(Error::InvalidModulePath(format!(
                "module path must have at least github.com/<owner>/<repo>: {s}"
            )));
        }
        for seg in &segments {
            if seg.is_empty() {
                return Err(Error::InvalidModulePath(format!(
                    "empty segment in module path: {s}"
                )));
            }
            validate_segment(seg, s)?;
        }

        let owner_start = "github.com/".len();
        let repo_start = owner_start + segments[1].len() + 1;

        // Check for major version suffix on the last segment
        let last = segments.last().unwrap();
        let major = parse_major_suffix(last)?;
        if let Some(n) = major {
            if n < 2 {
                return Err(Error::InvalidModulePath(format!(
                    "major version suffix must be >= v2, got v{n} in: {s}"
                )));
            }
        }

        Ok(ModulePath {
            raw: s.to_string(),
            owner_start,
            repo_start,
            major,
        })
    }

    pub fn as_str(&self) -> &str {
        &self.raw
    }

    pub fn owner(&self) -> &str {
        let rest = &self.raw[self.owner_start..];
        rest.split('/').next().unwrap()
    }

    pub fn repo(&self) -> &str {
        let rest = &self.raw[self.repo_start..];
        rest.split('/').next().unwrap()
    }

    /// The module root relative to the repository root.
    /// For `github.com/acme/lib` → "."
    /// For `github.com/acme/mono/graphics` → "graphics"
    /// For `github.com/acme/mono/graphics/v2` → "graphics/v2"
    pub fn module_root(&self) -> &str {
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
        let sv = v.semver();
        match self.major {
            None => sv.major <= 1,
            Some(n) => sv.major == n,
        }
    }

    /// Compute the Git tag for a given version published under this module path.
    /// Root modules: "vX.Y.Z"
    /// Non-root modules: "<module_root>/vX.Y.Z"
    pub fn version_tag(&self, version: &ExactVersion) -> String {
        let root = self.module_root();
        if root == "." {
            format!("{version}")
        } else {
            format!("{root}/{version}")
        }
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

fn validate_segment(seg: &str, full: &str) -> Result<(), Error> {
    for c in seg.chars() {
        if !(c.is_ascii_lowercase() || c.is_ascii_digit() || c == '.' || c == '_' || c == '-') {
            return Err(Error::InvalidModulePath(format!(
                "segment '{seg}' contains invalid character '{c}' in: {full}"
            )));
        }
    }
    Ok(())
}

/// If segment matches `vN` where N >= 2, returns Some(N).
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

/// Classify an import path per spec §4.3.
/// If the first path segment contains '.', it is external and MUST begin with "github.com/".
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
    if path.contains('@') {
        return Err(Error::InvalidImportPath(format!(
            "'@' is not part of import syntax: {path}"
        )));
    }
    let first_segment = path.split('/').next().unwrap();
    if first_segment.contains('.') {
        // External: must begin with github.com/
        if !path.starts_with("github.com/") {
            return Err(Error::InvalidImportPath(format!(
                "external import must begin with 'github.com/': {path}"
            )));
        }
        if path == "github.com" || path == "github.com/" {
            return Err(Error::InvalidImportPath(format!(
                "incomplete external import path: {path}"
            )));
        }
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

/// Extract the 3-segment module root from an external import path.
///
/// For example, `"github.com/acme/lib/util"` → `Some("github.com/acme/lib")`.
/// Returns `None` for stdlib imports or paths with fewer than 3 segments.
pub fn extract_module_root(import_path: &str) -> Option<String> {
    let first_segment = import_path.split('/').next()?;
    if !first_segment.contains('.') {
        return None;
    }
    let segments: Vec<&str> = import_path.splitn(4, '/').collect();
    if segments.len() >= 3 {
        Some(format!("{}/{}/{}", segments[0], segments[1], segments[2]))
    } else {
        None
    }
}

/// Check internal package visibility per spec §9.4.
/// Returns true if `importer_path` is allowed to import `target_path`.
pub fn check_internal_visibility(importer_path: &str, target_path: &str) -> bool {
    // Find "/internal/" in the target path
    let marker = "/internal/";
    if let Some(idx) = target_path.find(marker) {
        let required_prefix = &target_path[..idx]; // is everything before "/internal/"
                                                   // The importer must share this prefix
        if importer_path.starts_with(required_prefix)
            && (importer_path.len() == required_prefix.len()
                || importer_path.as_bytes().get(required_prefix.len()) == Some(&b'/'))
        {
            return true;
        }
        return false;
    }
    // Also check if the target path ends with "/internal"
    if let Some(required_prefix) = target_path.strip_suffix("/internal") {
        if importer_path.starts_with(required_prefix)
            && (importer_path.len() == required_prefix.len()
                || importer_path.as_bytes().get(required_prefix.len()) == Some(&b'/'))
        {
            return true;
        }
        return false;
    }
    // No internal marker — always visible
    true
}

// ============================================================
// LocalName — ephemeral single-file module name (spec §3.5, §5.6.2)
// ============================================================

/// A validated `local/<name>` identity for an ephemeral single-file module.
///
/// `<name>` MUST match `[a-z0-9][a-z0-9._-]*` and MUST NOT contain `/`.
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
    Ok(())
}

// ============================================================
// ModIdentity — root-module identity for `vo.mod` / `vo.lock`
// ============================================================

/// Identity of a root module (spec §5.6.2). `Github` covers canonical
/// published module paths (`github.com/<owner>/<repo>/...`). `Local` covers
/// the reserved ephemeral namespace and MUST NOT appear anywhere other than
/// the `module` directive of an inline `/*vo:mod*/` block (or a
/// toolchain-synthesized ephemeral `vo.mod` that mirrors it).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ModIdentity {
    Github(ModulePath),
    Local(LocalName),
}

impl ModIdentity {
    /// Parse a root-module identity from its canonical string form.
    ///
    /// Accepts either `local/<name>` (ephemeral) or `github.com/<owner>/<repo>[...]`
    /// (canonical published path). Any other shape is rejected.
    pub fn parse(s: &str) -> Result<Self, Error> {
        if s.starts_with(LOCAL_NAMESPACE_PREFIX) {
            return LocalName::parse(s).map(ModIdentity::Local);
        }
        ModulePath::parse(s).map(ModIdentity::Github)
    }

    /// Canonical string form (e.g. `github.com/acme/app` or `local/gui_chat`).
    pub fn as_str(&self) -> &str {
        match self {
            ModIdentity::Github(mp) => mp.as_str(),
            ModIdentity::Local(name) => name.as_str(),
        }
    }

    pub fn is_local(&self) -> bool {
        matches!(self, ModIdentity::Local(_))
    }

    pub fn is_github(&self) -> bool {
        matches!(self, ModIdentity::Github(_))
    }

    /// Borrow the underlying canonical github path, if any.
    pub fn as_github(&self) -> Option<&ModulePath> {
        match self {
            ModIdentity::Github(mp) => Some(mp),
            ModIdentity::Local(_) => None,
        }
    }

    /// Borrow the underlying local name, if any.
    pub fn as_local(&self) -> Option<&LocalName> {
        match self {
            ModIdentity::Local(name) => Some(name),
            ModIdentity::Github(_) => None,
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
        ModIdentity::Github(mp)
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ArtifactId {
    pub kind: String,
    pub target: String,
    pub name: String,
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
    fn test_module_path_reject_non_github() {
        assert!(ModulePath::parse("gitlab.com/acme/lib").is_err());
    }

    #[test]
    fn test_module_path_accepts_version() {
        let unsuffixed = ModulePath::parse("github.com/acme/lib").unwrap();
        assert!(unsuffixed.accepts_version(&ExactVersion::parse("v0.1.0").unwrap()));
        assert!(unsuffixed.accepts_version(&ExactVersion::parse("v1.5.0").unwrap()));
        assert!(!unsuffixed.accepts_version(&ExactVersion::parse("v2.0.0").unwrap()));

        let suffixed = ModulePath::parse("github.com/acme/lib/v2").unwrap();
        assert!(!suffixed.accepts_version(&ExactVersion::parse("v1.0.0").unwrap()));
        assert!(suffixed.accepts_version(&ExactVersion::parse("v2.0.0").unwrap()));
        assert!(suffixed.accepts_version(&ExactVersion::parse("v2.3.1").unwrap()));
        assert!(!suffixed.accepts_version(&ExactVersion::parse("v3.0.0").unwrap()));
    }

    #[test]
    fn test_version_tag() {
        let root = ModulePath::parse("github.com/acme/lib").unwrap();
        assert_eq!(
            root.version_tag(&ExactVersion::parse("v1.4.2").unwrap()),
            "v1.4.2"
        );

        let nested = ModulePath::parse("github.com/acme/mono/graphics").unwrap();
        assert_eq!(
            nested.version_tag(&ExactVersion::parse("v0.8.0").unwrap()),
            "graphics/v0.8.0"
        );

        let nested_v2 = ModulePath::parse("github.com/acme/mono/graphics/v2").unwrap();
        assert_eq!(
            nested_v2.version_tag(&ExactVersion::parse("v2.1.0").unwrap()),
            "graphics/v2/v2.1.0"
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
    fn test_classify_reject_non_github() {
        assert!(classify_import("example.com/acme/lib").is_err());
    }

    #[test]
    fn test_classify_reject_std_prefix() {
        assert!(classify_import("std/fmt").is_err());
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
    }

    // --- extract_module_root ---

    #[test]
    fn test_extract_module_root_subpackage() {
        assert_eq!(
            extract_module_root("github.com/acme/lib/util"),
            Some("github.com/acme/lib".to_string()),
        );
    }

    #[test]
    fn test_extract_module_root_exact() {
        assert_eq!(
            extract_module_root("github.com/acme/lib"),
            Some("github.com/acme/lib".to_string()),
        );
    }

    #[test]
    fn test_extract_module_root_stdlib() {
        assert_eq!(extract_module_root("fmt"), None);
        assert_eq!(extract_module_root("encoding/json"), None);
    }

    #[test]
    fn test_extract_module_root_incomplete() {
        assert_eq!(extract_module_root("github.com/acme"), None);
        assert_eq!(extract_module_root("github.com"), None);
    }
}
