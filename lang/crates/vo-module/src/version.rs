use std::cmp::Ordering;
use std::fmt;
use std::hash::{Hash, Hasher};
use serde::{Deserialize, Deserializer, Serialize, Serializer};

use crate::Error;

// ============================================================
// Pre-release identifier
// ============================================================

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PreRelease {
    Num(u64),
    Alpha(String),
}

impl PartialOrd for PreRelease {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for PreRelease {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (PreRelease::Num(a), PreRelease::Num(b)) => a.cmp(b),
            (PreRelease::Num(_), PreRelease::Alpha(_)) => Ordering::Less,
            (PreRelease::Alpha(_), PreRelease::Num(_)) => Ordering::Greater,
            (PreRelease::Alpha(a), PreRelease::Alpha(b)) => a.cmp(b),
        }
    }
}

impl fmt::Display for PreRelease {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PreRelease::Num(n) => write!(f, "{n}"),
            PreRelease::Alpha(s) => write!(f, "{s}"),
        }
    }
}

impl Hash for PreRelease {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            PreRelease::Num(n) => {
                0u8.hash(state);
                n.hash(state);
            }
            PreRelease::Alpha(s) => {
                1u8.hash(state);
                s.hash(state);
            }
        }
    }
}

// ============================================================
// SemVer — shared version representation
// ============================================================

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemVer {
    pub major: u64,
    pub minor: u64,
    pub patch: u64,
    pub pre: Vec<PreRelease>,
}

impl SemVer {
    pub fn new(major: u64, minor: u64, patch: u64) -> Self {
        Self { major, minor, patch, pre: Vec::new() }
    }

    pub fn is_prerelease(&self) -> bool {
        !self.pre.is_empty()
    }

    /// Parse "MAJOR.MINOR.PATCH[-PRERELEASE]" (no prefix).
    pub fn parse(s: &str) -> Result<Self, Error> {
        if s.contains('+') {
            return Err(Error::InvalidVersion(
                format!("build metadata (+) is not allowed: {s}"),
            ));
        }
        let (version_part, pre_part) = match s.find('-') {
            Some(idx) => (&s[..idx], Some(&s[idx + 1..])),
            None => (s, None),
        };
        let parts: Vec<&str> = version_part.split('.').collect();
        if parts.len() != 3 {
            return Err(Error::InvalidVersion(
                format!("expected MAJOR.MINOR.PATCH, got: {s}"),
            ));
        }
        let major = parse_numeric_no_leading_zero(parts[0], s)?;
        let minor = parse_numeric_no_leading_zero(parts[1], s)?;
        let patch = parse_numeric_no_leading_zero(parts[2], s)?;
        let pre = match pre_part {
            Some(p) if p.is_empty() => {
                return Err(Error::InvalidVersion(format!("empty prerelease: {s}")));
            }
            Some(p) => parse_prerelease(p, s)?,
            None => Vec::new(),
        };
        Ok(SemVer { major, minor, patch, pre })
    }
}

fn parse_numeric_no_leading_zero(s: &str, full: &str) -> Result<u64, Error> {
    if s.is_empty() {
        return Err(Error::InvalidVersion(format!("empty numeric component in: {full}")));
    }
    if s.len() > 1 && s.starts_with('0') {
        return Err(Error::InvalidVersion(
            format!("leading zero in numeric component '{s}' in: {full}"),
        ));
    }
    s.parse::<u64>()
        .map_err(|_| Error::InvalidVersion(format!("invalid numeric '{s}' in: {full}")))
}

fn parse_prerelease(s: &str, full: &str) -> Result<Vec<PreRelease>, Error> {
    let mut result = Vec::new();
    for ident in s.split('.') {
        if ident.is_empty() {
            return Err(Error::InvalidVersion(
                format!("empty prerelease identifier in: {full}"),
            ));
        }
        if ident.chars().all(|c| c.is_ascii_digit()) {
            let n = parse_numeric_no_leading_zero(ident, full)?;
            result.push(PreRelease::Num(n));
        } else {
            if !ident.chars().all(|c| c.is_ascii_alphanumeric() || c == '-') {
                return Err(Error::InvalidVersion(
                    format!("invalid prerelease identifier '{ident}' in: {full}"),
                ));
            }
            result.push(PreRelease::Alpha(ident.to_string()));
        }
    }
    Ok(result)
}

impl PartialOrd for SemVer {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for SemVer {
    fn cmp(&self, other: &Self) -> Ordering {
        let base = self
            .major
            .cmp(&other.major)
            .then(self.minor.cmp(&other.minor))
            .then(self.patch.cmp(&other.patch));
        if base != Ordering::Equal {
            return base;
        }
        // no pre-release > has pre-release
        match (self.pre.is_empty(), other.pre.is_empty()) {
            (true, true) => Ordering::Equal,
            (true, false) => Ordering::Greater,
            (false, true) => Ordering::Less,
            (false, false) => {
                for (a, b) in self.pre.iter().zip(other.pre.iter()) {
                    match a.cmp(b) {
                        Ordering::Equal => continue,
                        ord => return ord,
                    }
                }
                self.pre.len().cmp(&other.pre.len())
            }
        }
    }
}

impl fmt::Display for SemVer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}.{}", self.major, self.minor, self.patch)?;
        if !self.pre.is_empty() {
            write!(f, "-")?;
            for (i, p) in self.pre.iter().enumerate() {
                if i > 0 {
                    write!(f, ".")?;
                }
                write!(f, "{p}")?;
            }
        }
        Ok(())
    }
}

impl Hash for SemVer {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.major.hash(state);
        self.minor.hash(state);
        self.patch.hash(state);
        self.pre.hash(state);
    }
}

// ============================================================
// ExactVersion — dependency version with `v` prefix
// ============================================================

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExactVersion(pub SemVer);

impl ExactVersion {
    /// Parse "vMAJOR.MINOR.PATCH[-PRERELEASE]".
    pub fn parse(s: &str) -> Result<Self, Error> {
        let rest = s.strip_prefix('v').ok_or_else(|| {
            Error::InvalidVersion(format!("dependency version must start with 'v': {s}"))
        })?;
        Ok(ExactVersion(SemVer::parse(rest)?))
    }

    pub fn semver(&self) -> &SemVer {
        &self.0
    }
}

impl fmt::Display for ExactVersion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "v{}", self.0)
    }
}

// ============================================================
// ToolchainVersion — toolchain version without `v` prefix
// ============================================================

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ToolchainVersion(pub SemVer);

impl ToolchainVersion {
    /// Parse "MAJOR.MINOR.PATCH[-PRERELEASE]" (no v prefix).
    pub fn parse(s: &str) -> Result<Self, Error> {
        if s.starts_with('v') {
            return Err(Error::InvalidVersion(
                format!("toolchain version must not start with 'v': {s}"),
            ));
        }
        Ok(ToolchainVersion(SemVer::parse(s)?))
    }

    pub fn semver(&self) -> &SemVer {
        &self.0
    }
}

impl fmt::Display for ToolchainVersion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

// ============================================================
// Constraint operators and types
// ============================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConstraintOp {
    Exact,
    Compatible,
    PatchCompat,
}

/// Dependency constraint: `v1.2.3` (exact), `^1.2.3` (compatible), `~1.2.3` (patch-compatible).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DepConstraint {
    pub op: ConstraintOp,
    pub version: SemVer,
}

impl DepConstraint {
    /// Parse a dependency constraint string.
    /// Accepted forms: `v1.2.3`, `^1.2.3`, `~1.2.3`.
    pub fn parse(s: &str) -> Result<Self, Error> {
        if let Some(rest) = s.strip_prefix('^') {
            let v = SemVer::parse(rest)?;
            Ok(DepConstraint { op: ConstraintOp::Compatible, version: v })
        } else if let Some(rest) = s.strip_prefix('~') {
            let v = SemVer::parse(rest)?;
            Ok(DepConstraint { op: ConstraintOp::PatchCompat, version: v })
        } else if s.starts_with('v') {
            let ev = ExactVersion::parse(s)?;
            Ok(DepConstraint { op: ConstraintOp::Exact, version: ev.0 })
        } else {
            Err(Error::InvalidConstraint(
                format!("dependency constraint must start with '^', '~', or 'v': {s}"),
            ))
        }
    }

    /// Check if an exact version satisfies this constraint.
    pub fn satisfies(&self, v: &ExactVersion) -> bool {
        let sv = &v.0;
        match self.op {
            ConstraintOp::Exact => *sv == self.version,
            ConstraintOp::Compatible => satisfies_compatible(sv, &self.version),
            ConstraintOp::PatchCompat => satisfies_patch_compat(sv, &self.version),
        }
    }
}

impl fmt::Display for DepConstraint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.op {
            ConstraintOp::Exact => write!(f, "v{}", self.version),
            ConstraintOp::Compatible => write!(f, "^{}", self.version),
            ConstraintOp::PatchCompat => write!(f, "~{}", self.version),
        }
    }
}

/// Toolchain constraint: `1.2.3` (exact), `^1.2.3` (compatible), `~1.2.3` (patch-compatible).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToolchainConstraint {
    pub op: ConstraintOp,
    pub version: SemVer,
}

impl ToolchainConstraint {
    /// Parse a toolchain constraint string.
    /// Accepted forms: `1.2.3`, `^1.2.3`, `~1.2.3`.
    pub fn parse(s: &str) -> Result<Self, Error> {
        if let Some(rest) = s.strip_prefix('^') {
            let v = SemVer::parse(rest)?;
            Ok(ToolchainConstraint { op: ConstraintOp::Compatible, version: v })
        } else if let Some(rest) = s.strip_prefix('~') {
            let v = SemVer::parse(rest)?;
            Ok(ToolchainConstraint { op: ConstraintOp::PatchCompat, version: v })
        } else if s.starts_with('v') {
            Err(Error::InvalidConstraint(
                format!("toolchain constraint must not start with 'v': {s}"),
            ))
        } else {
            let v = SemVer::parse(s)?;
            Ok(ToolchainConstraint { op: ConstraintOp::Exact, version: v })
        }
    }

    /// Check if a toolchain version satisfies this constraint.
    pub fn satisfies(&self, v: &ToolchainVersion) -> bool {
        let sv = &v.0;
        match self.op {
            ConstraintOp::Exact => *sv == self.version,
            ConstraintOp::Compatible => satisfies_compatible(sv, &self.version),
            ConstraintOp::PatchCompat => satisfies_patch_compat(sv, &self.version),
        }
    }

    /// Returns `true` if every version accepted by `self` is also accepted by `other`.
    ///
    /// Used to verify that a dependency's toolchain constraint is at least as
    /// narrow as the root project's constraint.
    pub fn is_subset_of(&self, other: &ToolchainConstraint) -> bool {
        // For Exact, self accepts only one version — just check if other accepts it.
        if self.op == ConstraintOp::Exact {
            let tv = ToolchainVersion(self.version.clone());
            return other.satisfies(&tv);
        }

        // For range constraints, compute [lower, upper) for both and check containment.
        let (self_lower, self_upper) = constraint_range(&self.op, &self.version);
        let (other_lower, other_upper) = constraint_range(&other.op, &other.version);

        // self ⊆ other  ⟺  other_lower ≤ self_lower  ∧  self_upper ≤ other_upper
        // Also handle pre-release: if other excludes pre-release but self includes it, not subset.
        if !self.version.pre.is_empty() && other.version.pre.is_empty() {
            return false;
        }

        other_lower <= self_lower && self_upper <= other_upper
    }
}

/// Compute the `[lower, upper)` half-open range for a range constraint.
/// For `Exact`, this is not meaningful — callers should handle that case separately.
fn constraint_range(op: &ConstraintOp, version: &SemVer) -> (SemVer, SemVer) {
    let lower = version.clone();
    let upper = match op {
        ConstraintOp::Compatible => {
            if version.major != 0 {
                SemVer { major: version.major + 1, minor: 0, patch: 0, pre: vec![] }
            } else if version.minor != 0 {
                SemVer { major: 0, minor: version.minor + 1, patch: 0, pre: vec![] }
            } else {
                SemVer { major: 0, minor: 0, patch: version.patch + 1, pre: vec![] }
            }
        }
        ConstraintOp::PatchCompat => {
            SemVer { major: version.major, minor: version.minor + 1, patch: 0, pre: vec![] }
        }
        ConstraintOp::Exact => {
            // Not meaningful for exact — return a degenerate range.
            SemVer { major: version.major, minor: version.minor, patch: version.patch + 1, pre: vec![] }
        }
    };
    (lower, upper)
}

impl fmt::Display for ToolchainConstraint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.op {
            ConstraintOp::Exact => write!(f, "{}", self.version),
            ConstraintOp::Compatible => write!(f, "^{}", self.version),
            ConstraintOp::PatchCompat => write!(f, "~{}", self.version),
        }
    }
}

// ============================================================
// Shared constraint satisfaction logic
// ============================================================

/// Compatible constraint (^): same non-zero leftmost component, >= lower bound.
/// Pre-release versions only match if lower bound is also pre-release on the same patch.
fn satisfies_compatible(v: &SemVer, lower: &SemVer) -> bool {
    if !check_prerelease(v, lower) {
        return false;
    }
    if v < lower {
        return false;
    }
    if lower.major != 0 {
        v.major == lower.major
    } else if lower.minor != 0 {
        v.major == 0 && v.minor == lower.minor
    } else {
        // ^0.0.X — pin to exact patch
        v.major == 0 && v.minor == 0 && v.patch == lower.patch
    }
}

/// Patch-compatible constraint (~): same major.minor, >= lower bound.
fn satisfies_patch_compat(v: &SemVer, lower: &SemVer) -> bool {
    if !check_prerelease(v, lower) {
        return false;
    }
    if v < lower {
        return false;
    }
    v.major == lower.major && v.minor == lower.minor
}

/// Pre-release filter: if the lower bound has pre-release identifiers,
/// allow pre-release versions on the same major.minor.patch.
/// If the lower bound has no pre-release, reject all pre-release versions.
fn check_prerelease(v: &SemVer, lower: &SemVer) -> bool {
    if v.pre.is_empty() {
        return true;
    }
    if lower.pre.is_empty() {
        return false;
    }
    // Both have pre-release: only match if same major.minor.patch
    v.major == lower.major && v.minor == lower.minor && v.patch == lower.patch
}

// ============================================================
// Serde for constraint/version types
// ============================================================

impl Serialize for DepConstraint {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl<'de> Deserialize<'de> for DepConstraint {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let raw = String::deserialize(deserializer)?;
        DepConstraint::parse(&raw).map_err(serde::de::Error::custom)
    }
}

impl Serialize for ToolchainConstraint {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl<'de> Deserialize<'de> for ToolchainConstraint {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let raw = String::deserialize(deserializer)?;
        ToolchainConstraint::parse(&raw).map_err(serde::de::Error::custom)
    }
}

impl Serialize for ExactVersion {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl<'de> Deserialize<'de> for ExactVersion {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let raw = String::deserialize(deserializer)?;
        ExactVersion::parse(&raw).map_err(serde::de::Error::custom)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // --- SemVer ---

    #[test]
    fn test_semver_parse_basic() {
        let v = SemVer::parse("1.2.3").unwrap();
        assert_eq!(v.major, 1);
        assert_eq!(v.minor, 2);
        assert_eq!(v.patch, 3);
        assert!(v.pre.is_empty());
    }

    #[test]
    fn test_semver_parse_prerelease() {
        let v = SemVer::parse("1.0.0-beta.1").unwrap();
        assert_eq!(v.pre, vec![PreRelease::Alpha("beta".into()), PreRelease::Num(1)]);
    }

    #[test]
    fn test_semver_reject_build_metadata() {
        assert!(SemVer::parse("1.0.0+build").is_err());
    }

    #[test]
    fn test_semver_reject_leading_zero() {
        assert!(SemVer::parse("01.0.0").is_err());
        assert!(SemVer::parse("1.00.0").is_err());
        assert!(SemVer::parse("1.0.00").is_err());
    }

    #[test]
    fn test_semver_ordering() {
        let v1 = SemVer::parse("1.0.0").unwrap();
        let v2 = SemVer::parse("1.0.1").unwrap();
        let v3 = SemVer::parse("1.1.0").unwrap();
        let v4 = SemVer::parse("2.0.0").unwrap();
        assert!(v1 < v2);
        assert!(v2 < v3);
        assert!(v3 < v4);
    }

    #[test]
    fn test_semver_prerelease_ordering() {
        let release = SemVer::parse("1.0.0").unwrap();
        let pre = SemVer::parse("1.0.0-alpha").unwrap();
        assert!(pre < release);
    }

    #[test]
    fn test_semver_display() {
        assert_eq!(SemVer::parse("1.2.3").unwrap().to_string(), "1.2.3");
        assert_eq!(SemVer::parse("1.0.0-beta.1").unwrap().to_string(), "1.0.0-beta.1");
    }

    // --- ExactVersion ---

    #[test]
    fn test_exact_version_parse() {
        let v = ExactVersion::parse("v1.2.3").unwrap();
        assert_eq!(v.to_string(), "v1.2.3");
    }

    #[test]
    fn test_exact_version_reject_no_prefix() {
        assert!(ExactVersion::parse("1.2.3").is_err());
    }

    // --- ToolchainVersion ---

    #[test]
    fn test_toolchain_version_parse() {
        let v = ToolchainVersion::parse("1.0.0").unwrap();
        assert_eq!(v.to_string(), "1.0.0");
    }

    #[test]
    fn test_toolchain_version_reject_v_prefix() {
        assert!(ToolchainVersion::parse("v1.0.0").is_err());
    }

    // --- DepConstraint ---

    #[test]
    fn test_dep_constraint_exact() {
        let c = DepConstraint::parse("v1.2.3").unwrap();
        assert!(c.satisfies(&ExactVersion::parse("v1.2.3").unwrap()));
        assert!(!c.satisfies(&ExactVersion::parse("v1.2.4").unwrap()));
    }

    #[test]
    fn test_dep_constraint_compatible_major() {
        let c = DepConstraint::parse("^1.2.3").unwrap();
        assert!(c.satisfies(&ExactVersion::parse("v1.2.3").unwrap()));
        assert!(c.satisfies(&ExactVersion::parse("v1.9.0").unwrap()));
        assert!(!c.satisfies(&ExactVersion::parse("v2.0.0").unwrap()));
        assert!(!c.satisfies(&ExactVersion::parse("v1.2.2").unwrap()));
    }

    #[test]
    fn test_dep_constraint_compatible_minor() {
        let c = DepConstraint::parse("^0.4.0").unwrap();
        assert!(c.satisfies(&ExactVersion::parse("v0.4.0").unwrap()));
        assert!(c.satisfies(&ExactVersion::parse("v0.4.9").unwrap()));
        assert!(!c.satisfies(&ExactVersion::parse("v0.5.0").unwrap()));
        assert!(!c.satisfies(&ExactVersion::parse("v0.3.9").unwrap()));
    }

    #[test]
    fn test_dep_constraint_compatible_patch() {
        let c = DepConstraint::parse("^0.0.3").unwrap();
        assert!(c.satisfies(&ExactVersion::parse("v0.0.3").unwrap()));
        assert!(!c.satisfies(&ExactVersion::parse("v0.0.4").unwrap()));
        assert!(!c.satisfies(&ExactVersion::parse("v0.0.2").unwrap()));
    }

    #[test]
    fn test_dep_constraint_patch_compat() {
        let c = DepConstraint::parse("~1.2.3").unwrap();
        assert!(c.satisfies(&ExactVersion::parse("v1.2.3").unwrap()));
        assert!(c.satisfies(&ExactVersion::parse("v1.2.9").unwrap()));
        assert!(!c.satisfies(&ExactVersion::parse("v1.3.0").unwrap()));
    }

    #[test]
    fn test_prerelease_excluded_by_default() {
        let c = DepConstraint::parse("^1.0.0").unwrap();
        assert!(!c.satisfies(&ExactVersion::parse("v1.0.1-beta.1").unwrap()));
    }

    #[test]
    fn test_prerelease_allowed_when_lower_bound_is_prerelease() {
        let c = DepConstraint::parse("^1.0.0-beta.1").unwrap();
        assert!(c.satisfies(&ExactVersion::parse("v1.0.0-beta.2").unwrap()));
        assert!(c.satisfies(&ExactVersion::parse("v1.0.0").unwrap()));
    }

    // --- ToolchainConstraint ---

    #[test]
    fn test_toolchain_constraint_compatible() {
        let c = ToolchainConstraint::parse("^1.0.0").unwrap();
        assert!(c.satisfies(&ToolchainVersion::parse("1.0.0").unwrap()));
        assert!(c.satisfies(&ToolchainVersion::parse("1.5.0").unwrap()));
        assert!(!c.satisfies(&ToolchainVersion::parse("2.0.0").unwrap()));
    }

    #[test]
    fn test_toolchain_constraint_exact() {
        let c = ToolchainConstraint::parse("1.2.3").unwrap();
        assert!(c.satisfies(&ToolchainVersion::parse("1.2.3").unwrap()));
        assert!(!c.satisfies(&ToolchainVersion::parse("1.2.4").unwrap()));
    }

    #[test]
    fn test_toolchain_constraint_is_subset() {
        // ^1.0.0 ⊆ ^1.0.0  (identical)
        let a = ToolchainConstraint::parse("^1.0.0").unwrap();
        assert!(a.is_subset_of(&a));

        // ~1.2.3 ⊆ ^1.2.0  (tilde is narrower than compatible)
        let tilde = ToolchainConstraint::parse("~1.2.3").unwrap();
        let caret = ToolchainConstraint::parse("^1.2.0").unwrap();
        assert!(tilde.is_subset_of(&caret));

        // ^1.0.0 ⊄ ~1.0.0  (compatible is wider than tilde)
        let wide = ToolchainConstraint::parse("^1.0.0").unwrap();
        let narrow = ToolchainConstraint::parse("~1.0.0").unwrap();
        assert!(!wide.is_subset_of(&narrow));

        // exact 1.5.0 ⊆ ^1.0.0
        let exact = ToolchainConstraint::parse("1.5.0").unwrap();
        assert!(exact.is_subset_of(&ToolchainConstraint::parse("^1.0.0").unwrap()));

        // exact 2.0.0 ⊄ ^1.0.0
        let exact2 = ToolchainConstraint::parse("2.0.0").unwrap();
        assert!(!exact2.is_subset_of(&ToolchainConstraint::parse("^1.0.0").unwrap()));

        // ^0.2.0 ⊄ ^0.1.0  (different minor zero-major)
        let a02 = ToolchainConstraint::parse("^0.2.0").unwrap();
        let a01 = ToolchainConstraint::parse("^0.1.0").unwrap();
        assert!(!a02.is_subset_of(&a01));
        assert!(!a01.is_subset_of(&a02));

        // pre-release: ^1.0.0-rc.1 ⊄ ^1.0.0  (includes pre-release that superset excludes)
        let pre = ToolchainConstraint::parse("^1.0.0-rc.1").unwrap();
        let stable = ToolchainConstraint::parse("^1.0.0").unwrap();
        assert!(!pre.is_subset_of(&stable));

        // ^1.0.0 ⊆ ^1.0.0-rc.1  (stable subset accepts strictly fewer pre-release versions)
        assert!(stable.is_subset_of(&pre));
    }
}
