use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::cmp::Ordering;
use std::fmt;
use std::hash::{Hash, Hasher};

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
    major: u64,
    minor: u64,
    patch: u64,
    pre: Vec<PreRelease>,
}

impl SemVer {
    pub fn new(major: u64, minor: u64, patch: u64) -> Self {
        Self {
            major,
            minor,
            patch,
            pre: Vec::new(),
        }
    }

    pub fn is_prerelease(&self) -> bool {
        !self.pre.is_empty()
    }

    pub fn major(&self) -> u64 {
        self.major
    }

    pub fn minor(&self) -> u64 {
        self.minor
    }

    pub fn patch(&self) -> u64 {
        self.patch
    }

    pub fn prerelease(&self) -> &[PreRelease] {
        &self.pre
    }

    /// Parse "MAJOR.MINOR.PATCH[-PRERELEASE]" (no prefix).
    pub fn parse(s: &str) -> Result<Self, Error> {
        if s.len() >= crate::schema::MAX_PORTABLE_PATH_COMPONENT_BYTES {
            return Err(Error::InvalidVersion(format!(
                "version exceeds the {}-byte canonical limit: {s}",
                crate::schema::MAX_PORTABLE_PATH_COMPONENT_BYTES - 1
            )));
        }
        if s.get(s.len().saturating_sub(".lock".len())..)
            .is_some_and(|suffix| suffix.eq_ignore_ascii_case(".lock"))
        {
            return Err(Error::InvalidVersion(format!(
                "version must not end with the Git-reserved .lock suffix: {s}"
            )));
        }
        if s.contains('+') {
            return Err(Error::InvalidVersion(format!(
                "build metadata (+) is not allowed: {s}"
            )));
        }
        let (version_part, pre_part) = match s.find('-') {
            Some(idx) => (&s[..idx], Some(&s[idx + 1..])),
            None => (s, None),
        };
        let parts: Vec<&str> = version_part.split('.').collect();
        if parts.len() != 3 {
            return Err(Error::InvalidVersion(format!(
                "expected MAJOR.MINOR.PATCH, got: {s}"
            )));
        }
        let major = parse_numeric_no_leading_zero(parts[0], s)?;
        let minor = parse_numeric_no_leading_zero(parts[1], s)?;
        let patch = parse_numeric_no_leading_zero(parts[2], s)?;
        let pre = match pre_part {
            Some("") => {
                return Err(Error::InvalidVersion(format!("empty prerelease: {s}")));
            }
            Some(p) => parse_prerelease(p, s)?,
            None => Vec::new(),
        };
        Ok(SemVer {
            major,
            minor,
            patch,
            pre,
        })
    }
}

fn parse_numeric_no_leading_zero(s: &str, full: &str) -> Result<u64, Error> {
    if s.is_empty() {
        return Err(Error::InvalidVersion(format!(
            "empty numeric component in: {full}"
        )));
    }
    if s.len() > 1 && s.starts_with('0') {
        return Err(Error::InvalidVersion(format!(
            "leading zero in numeric component '{s}' in: {full}"
        )));
    }
    s.parse::<u64>()
        .map_err(|_| Error::InvalidVersion(format!("invalid numeric '{s}' in: {full}")))
}

fn parse_prerelease(s: &str, full: &str) -> Result<Vec<PreRelease>, Error> {
    let mut result = Vec::new();
    for ident in s.split('.') {
        if ident.is_empty() {
            return Err(Error::InvalidVersion(format!(
                "empty prerelease identifier in: {full}"
            )));
        }
        if ident.chars().all(|c| c.is_ascii_digit()) {
            let n = parse_numeric_no_leading_zero(ident, full)?;
            result.push(PreRelease::Num(n));
        } else {
            if !ident
                .chars()
                .all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == '-')
            {
                return Err(Error::InvalidVersion(format!(
                    "prerelease identifier must use only ASCII lowercase letters, digits, and '-': '{ident}' in: {full}"
                )));
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
// ExactVersion — canonical dependency version
// ============================================================

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExactVersion(SemVer);

impl ExactVersion {
    /// Parse "MAJOR.MINOR.PATCH[-PRERELEASE]".
    ///
    /// Version values in Volang protocol files are plain SemVer. Registry
    /// adapters remain responsible for adding transport-specific prefixes,
    /// such as GitHub's conventional `v` tag prefix.
    pub fn parse(s: &str) -> Result<Self, Error> {
        if s.starts_with('v') {
            return Err(Error::InvalidVersion(format!(
                "dependency version must not start with 'v': {s}"
            )));
        }
        let version = ExactVersion(SemVer::parse(s)?);
        version.validate().map_err(Error::InvalidVersion)?;
        Ok(version)
    }

    pub fn from_semver(version: SemVer) -> Self {
        Self(version)
    }

    pub fn semver(&self) -> &SemVer {
        &self.0
    }

    pub fn validate(&self) -> Result<(), String> {
        let rendered = self.to_string();
        crate::schema::validate_portable_path_component(&rendered)
            .map_err(|error| format!("dependency version {error}"))?;
        let reparsed = Self::parse_unchecked(&rendered).map_err(|error| error.to_string())?;
        if reparsed != *self {
            return Err("dependency version is not canonical".to_string());
        }
        Ok(())
    }

    fn parse_unchecked(s: &str) -> Result<Self, Error> {
        if s.starts_with('v') {
            return Err(Error::InvalidVersion(format!(
                "dependency version must not start with 'v': {s}"
            )));
        }
        Ok(Self(SemVer::parse(s)?))
    }
}

impl fmt::Display for ExactVersion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

// ============================================================
// ToolchainVersion — toolchain version without `v` prefix
// ============================================================

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ToolchainVersion(SemVer);

impl ToolchainVersion {
    /// Parse "MAJOR.MINOR.PATCH[-PRERELEASE]" (no v prefix).
    pub fn parse(s: &str) -> Result<Self, Error> {
        if s.starts_with('v') {
            return Err(Error::InvalidVersion(format!(
                "toolchain version must not start with 'v': {s}"
            )));
        }
        Ok(ToolchainVersion(SemVer::parse(s)?))
    }

    pub fn semver(&self) -> &SemVer {
        &self.0
    }

    pub fn from_semver(version: SemVer) -> Self {
        Self(version)
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

/// Dependency constraint: `1.2.3` (exact), `^1.2.3` (compatible), `~1.2.3` (patch-compatible).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DepConstraint {
    pub op: ConstraintOp,
    pub version: SemVer,
}

impl DepConstraint {
    /// Parse a dependency constraint string.
    /// Accepted forms: `1.2.3`, `^1.2.3`, `~1.2.3`.
    pub fn parse(s: &str) -> Result<Self, Error> {
        if let Some(rest) = s.strip_prefix('^') {
            let v = SemVer::parse(rest)?;
            Ok(DepConstraint {
                op: ConstraintOp::Compatible,
                version: v,
            })
        } else if let Some(rest) = s.strip_prefix('~') {
            let v = SemVer::parse(rest)?;
            Ok(DepConstraint {
                op: ConstraintOp::PatchCompat,
                version: v,
            })
        } else {
            let ev = ExactVersion::parse(s)?;
            Ok(DepConstraint {
                op: ConstraintOp::Exact,
                version: ev.0,
            })
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

/// Return whether all dependency constraints admit at least one common exact
/// version. Dependency constraints are lower-inclusive intervals; checking the
/// greatest lower bound is sufficient, with one additional stable candidate
/// for a prerelease lower bound because stable releases remain admissible.
#[cfg(test)]
pub(crate) fn dependency_constraints_have_common_version<'a>(
    constraints: impl IntoIterator<Item = &'a DepConstraint>,
) -> bool {
    let constraints = constraints.into_iter().collect::<Vec<_>>();
    let Some(greatest_lower_bound) = constraints
        .iter()
        .map(|constraint| &constraint.version)
        .max()
    else {
        return true;
    };

    let lower_candidate = ExactVersion::from_semver((*greatest_lower_bound).clone());
    if constraints
        .iter()
        .all(|constraint| constraint.satisfies(&lower_candidate))
    {
        return true;
    }
    if !greatest_lower_bound.is_prerelease() {
        return false;
    }

    let stable_candidate = ExactVersion::from_semver(SemVer::new(
        greatest_lower_bound.major(),
        greatest_lower_bound.minor(),
        greatest_lower_bound.patch(),
    ));
    constraints
        .iter()
        .all(|constraint| constraint.satisfies(&stable_candidate))
}

impl fmt::Display for DepConstraint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.op {
            ConstraintOp::Exact => write!(f, "{}", self.version),
            ConstraintOp::Compatible => write!(f, "^{}", self.version),
            ConstraintOp::PatchCompat => write!(f, "~{}", self.version),
        }
    }
}

/// Minimum compatible toolchain version.
///
/// The wire value is one bare version. Before 1.0 the compatibility epoch is
/// `major.minor`; from 1.0 onward it is `major`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToolchainConstraint {
    pub version: SemVer,
}

impl ToolchainConstraint {
    /// Parse a bare minimum toolchain version.
    pub fn parse(s: &str) -> Result<Self, Error> {
        if s.starts_with(['^', '~', 'v']) {
            Err(Error::InvalidConstraint(format!(
                "toolchain minimum must be a bare semantic version: {s}"
            )))
        } else {
            let v = SemVer::parse(s)?;
            Ok(ToolchainConstraint { version: v })
        }
    }

    /// Check if a toolchain version satisfies this constraint.
    pub fn satisfies(&self, v: &ToolchainVersion) -> bool {
        same_toolchain_epoch(&v.0, &self.version) && v.0 >= self.version
    }

    /// Returns `true` if every version accepted by `self` is also accepted by `other`.
    ///
    /// Used to verify that a dependency's toolchain constraint covers every
    /// toolchain version permitted by the root project's constraint.
    pub fn is_subset_of(&self, other: &ToolchainConstraint) -> bool {
        same_toolchain_epoch(&self.version, &other.version) && self.version >= other.version
    }
}

fn same_toolchain_epoch(left: &SemVer, right: &SemVer) -> bool {
    left.major == right.major && (left.major != 0 || left.minor == right.minor)
}

impl fmt::Display for ToolchainConstraint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.version)
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
        assert_eq!(
            v.pre,
            vec![PreRelease::Alpha("beta".into()), PreRelease::Num(1)]
        );
    }

    #[test]
    fn test_semver_reject_build_metadata() {
        assert!(SemVer::parse("1.0.0+build").is_err());
    }

    #[test]
    fn every_protocol_version_form_rejects_git_lock_suffixes() {
        for version in ["1.2.3-alpha.lock", "1.2.3-alpha.LOCK", "1.2.3-alpha.LoCk"] {
            assert!(SemVer::parse(version).is_err(), "{version}");
            assert!(ExactVersion::parse(version).is_err(), "{version}");
            assert!(ToolchainVersion::parse(version).is_err(), "{version}");
            assert!(DepConstraint::parse(version).is_err(), "{version}");
            assert!(DepConstraint::parse(&format!("^{version}")).is_err());
            assert!(ToolchainConstraint::parse(&format!("~{version}")).is_err());
        }
        assert!(ExactVersion::parse("1.2.3-alpha-lock").is_ok());
        assert!(ExactVersion::parse("1.2.3-lock.alpha").is_ok());
    }

    #[test]
    fn every_protocol_version_form_rejects_uppercase_prerelease_identifiers() {
        for version in ["1.2.3-RC", "1.2.3-rc.Test", "1.2.3-Alpha-1"] {
            assert!(SemVer::parse(version).is_err(), "{version}");
            assert!(ExactVersion::parse(version).is_err(), "{version}");
            assert!(ToolchainVersion::parse(version).is_err(), "{version}");
            assert!(DepConstraint::parse(version).is_err(), "{version}");
            assert!(DepConstraint::parse(&format!("^{version}")).is_err());
            assert!(ToolchainConstraint::parse(&format!("~{version}")).is_err());
        }
        assert!(ExactVersion::parse("1.2.3-rc.1").is_ok());
    }

    #[test]
    fn test_semver_reject_leading_zero() {
        assert!(SemVer::parse("01.0.0").is_err());
        assert!(SemVer::parse("1.00.0").is_err());
        assert!(SemVer::parse("1.0.00").is_err());
    }

    #[test]
    fn exact_version_fits_one_portable_cache_component() {
        let prefix = "1.0.0-";
        let max_semver = format!(
            "{prefix}{}",
            "a".repeat(crate::schema::MAX_PORTABLE_PATH_COMPONENT_BYTES - 1 - prefix.len())
        );
        assert_eq!(
            max_semver.len(),
            crate::schema::MAX_PORTABLE_PATH_COMPONENT_BYTES - 1
        );
        assert!(ExactVersion::parse(&max_semver).is_ok());

        let too_long = format!("{max_semver}a");
        assert!(ExactVersion::parse(&too_long).is_err());
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
        assert_eq!(
            SemVer::parse("1.0.0-beta.1").unwrap().to_string(),
            "1.0.0-beta.1"
        );
    }

    // --- ExactVersion ---

    #[test]
    fn test_exact_version_parse() {
        let v = ExactVersion::parse("1.2.3").unwrap();
        assert_eq!(v.to_string(), "1.2.3");
    }

    #[test]
    fn test_exact_version_reject_v_prefix() {
        assert!(ExactVersion::parse("v1.2.3").is_err());
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
        let c = DepConstraint::parse("1.2.3").unwrap();
        assert!(c.satisfies(&ExactVersion::parse("1.2.3").unwrap()));
        assert!(!c.satisfies(&ExactVersion::parse("1.2.4").unwrap()));
    }

    #[test]
    fn test_dep_constraint_compatible_major() {
        let c = DepConstraint::parse("^1.2.3").unwrap();
        assert!(c.satisfies(&ExactVersion::parse("1.2.3").unwrap()));
        assert!(c.satisfies(&ExactVersion::parse("1.9.0").unwrap()));
        assert!(!c.satisfies(&ExactVersion::parse("2.0.0").unwrap()));
        assert!(!c.satisfies(&ExactVersion::parse("1.2.2").unwrap()));
    }

    #[test]
    fn test_dep_constraint_compatible_minor() {
        let c = DepConstraint::parse("^0.4.0").unwrap();
        assert!(c.satisfies(&ExactVersion::parse("0.4.0").unwrap()));
        assert!(c.satisfies(&ExactVersion::parse("0.4.9").unwrap()));
        assert!(!c.satisfies(&ExactVersion::parse("0.5.0").unwrap()));
        assert!(!c.satisfies(&ExactVersion::parse("0.3.9").unwrap()));
    }

    #[test]
    fn test_dep_constraint_compatible_patch() {
        let c = DepConstraint::parse("^0.0.3").unwrap();
        assert!(c.satisfies(&ExactVersion::parse("0.0.3").unwrap()));
        assert!(!c.satisfies(&ExactVersion::parse("0.0.4").unwrap()));
        assert!(!c.satisfies(&ExactVersion::parse("0.0.2").unwrap()));
    }

    #[test]
    fn dependency_constraint_intersection_handles_ranges_exact_and_prerelease_bounds() {
        let overlapping = [
            DepConstraint::parse("^1.0.0").unwrap(),
            DepConstraint::parse("~1.4.0").unwrap(),
            DepConstraint::parse("1.4.2").unwrap(),
        ];
        assert!(dependency_constraints_have_common_version(
            overlapping.iter()
        ));

        let disjoint = [
            DepConstraint::parse("^1.0.0").unwrap(),
            DepConstraint::parse("^2.0.0").unwrap(),
        ];
        assert!(!dependency_constraints_have_common_version(disjoint.iter()));

        let prerelease_to_stable = [
            DepConstraint::parse("^1.0.0-alpha.1").unwrap(),
            DepConstraint::parse("^1.1.0-beta.1").unwrap(),
        ];
        assert!(dependency_constraints_have_common_version(
            prerelease_to_stable.iter()
        ));
    }

    #[test]
    fn test_dep_constraint_patch_compat() {
        let c = DepConstraint::parse("~1.2.3").unwrap();
        assert!(c.satisfies(&ExactVersion::parse("1.2.3").unwrap()));
        assert!(c.satisfies(&ExactVersion::parse("1.2.9").unwrap()));
        assert!(!c.satisfies(&ExactVersion::parse("1.3.0").unwrap()));
    }

    #[test]
    fn test_prerelease_excluded_by_default() {
        let c = DepConstraint::parse("^1.0.0").unwrap();
        assert!(!c.satisfies(&ExactVersion::parse("1.0.1-beta.1").unwrap()));
    }

    #[test]
    fn test_prerelease_allowed_when_lower_bound_is_prerelease() {
        let c = DepConstraint::parse("^1.0.0-beta.1").unwrap();
        assert!(c.satisfies(&ExactVersion::parse("1.0.0-beta.2").unwrap()));
        assert!(c.satisfies(&ExactVersion::parse("1.0.0").unwrap()));
    }

    // --- ToolchainConstraint ---

    #[test]
    fn test_toolchain_constraint_compatible() {
        let c = ToolchainConstraint::parse("1.0.0").unwrap();
        assert!(c.satisfies(&ToolchainVersion::parse("1.0.0").unwrap()));
        assert!(c.satisfies(&ToolchainVersion::parse("1.5.0").unwrap()));
        assert!(!c.satisfies(&ToolchainVersion::parse("2.0.0").unwrap()));
    }

    #[test]
    fn test_toolchain_constraint_is_a_compatible_minimum() {
        let c = ToolchainConstraint::parse("1.2.3").unwrap();
        assert!(c.satisfies(&ToolchainVersion::parse("1.2.3").unwrap()));
        assert!(c.satisfies(&ToolchainVersion::parse("1.2.4").unwrap()));
        assert!(!c.satisfies(&ToolchainVersion::parse("2.0.0").unwrap()));
    }

    #[test]
    fn test_toolchain_constraint_is_subset() {
        // ^1.0.0 ⊆ ^1.0.0  (identical)
        let a = ToolchainConstraint::parse("1.0.0").unwrap();
        assert!(a.is_subset_of(&a));

        // A later minimum within the same compatibility epoch is narrower.
        let later = ToolchainConstraint::parse("1.2.3").unwrap();
        let earlier = ToolchainConstraint::parse("1.2.0").unwrap();
        assert!(later.is_subset_of(&earlier));
        assert!(!earlier.is_subset_of(&later));

        // exact 1.5.0 ⊆ ^1.0.0
        let exact = ToolchainConstraint::parse("1.5.0").unwrap();
        assert!(exact.is_subset_of(&ToolchainConstraint::parse("1.0.0").unwrap()));

        // exact 2.0.0 ⊄ ^1.0.0
        let exact2 = ToolchainConstraint::parse("2.0.0").unwrap();
        assert!(!exact2.is_subset_of(&ToolchainConstraint::parse("1.0.0").unwrap()));

        // ^0.2.0 ⊄ ^0.1.0  (different minor zero-major)
        let a02 = ToolchainConstraint::parse("0.2.0").unwrap();
        let a01 = ToolchainConstraint::parse("0.1.0").unwrap();
        assert!(!a02.is_subset_of(&a01));
        assert!(!a01.is_subset_of(&a02));

        // pre-release: ^1.0.0-rc.1 ⊄ ^1.0.0  (includes pre-release that superset excludes)
        let pre = ToolchainConstraint::parse("1.0.0-rc.1").unwrap();
        let stable = ToolchainConstraint::parse("1.0.0").unwrap();
        assert!(!pre.is_subset_of(&stable));

        // ^1.0.0 ⊆ ^1.0.0-rc.1  (stable subset accepts strictly fewer pre-release versions)
        assert!(stable.is_subset_of(&pre));

        // A later pre-release minimum in the same epoch is narrower.
        let later_core_pre = ToolchainConstraint::parse("1.1.0-alpha.1").unwrap();
        let earlier_core_pre = ToolchainConstraint::parse("1.0.0-alpha.1").unwrap();
        assert!(later_core_pre.is_subset_of(&earlier_core_pre));

        // On the same core, a later pre-release lower bound is a subset when
        // the stable upper range is also contained.
        let alpha = ToolchainConstraint::parse("1.0.0-alpha.1").unwrap();
        let beta = ToolchainConstraint::parse("1.0.0-beta.1").unwrap();
        assert!(beta.is_subset_of(&alpha));
        assert!(!alpha.is_subset_of(&beta));

        // Identical compatible minima contain exactly the same versions.
        assert!(ToolchainConstraint::parse("1.0.0")
            .unwrap()
            .is_subset_of(&ToolchainConstraint::parse("1.0.0").unwrap()));

        // ^0.0.X is a semantic singleton for stable lower bounds.
        assert!(ToolchainConstraint::parse("0.0.5")
            .unwrap()
            .is_subset_of(&ToolchainConstraint::parse("0.0.5").unwrap()));
        assert!(!ToolchainConstraint::parse("0.0.5-alpha.1")
            .unwrap()
            .is_subset_of(&ToolchainConstraint::parse("0.0.5").unwrap()));
    }

    #[test]
    fn constraint_subset_handles_u64_component_boundaries_without_overflow() {
        let max = u64::MAX;
        let caret_max = ToolchainConstraint::parse(&format!("{max}.0.0")).unwrap();
        assert!(caret_max.is_subset_of(&caret_max));

        let tilde_max = ToolchainConstraint::parse(&format!("{max}.{max}.{max}")).unwrap();
        assert!(tilde_max.is_subset_of(&caret_max));

        let zero_major_max_minor = ToolchainConstraint::parse(&format!("0.{max}.0")).unwrap();
        assert!(zero_major_max_minor.is_subset_of(&zero_major_max_minor));

        let zero_major_max_patch = ToolchainConstraint::parse(&format!("0.0.{max}")).unwrap();
        assert!(zero_major_max_patch.is_subset_of(&zero_major_max_patch));
        assert!(zero_major_max_patch
            .is_subset_of(&ToolchainConstraint::parse(&format!("0.0.{max}")).unwrap()));

        let zero_major_nonzero_minor_max_patch =
            ToolchainConstraint::parse(&format!("0.1.{max}")).unwrap();
        assert!(zero_major_nonzero_minor_max_patch
            .is_subset_of(&ToolchainConstraint::parse(&format!("0.1.{max}")).unwrap()));

        let nonzero_major_singleton =
            ToolchainConstraint::parse(&format!("1.{max}.{max}")).unwrap();
        assert!(nonzero_major_singleton
            .is_subset_of(&ToolchainConstraint::parse(&format!("1.{max}.{max}")).unwrap()));

        let tilde_singleton = ToolchainConstraint::parse(&format!("7.8.{max}")).unwrap();
        assert!(tilde_singleton
            .is_subset_of(&ToolchainConstraint::parse(&format!("7.8.{max}")).unwrap()));
    }
}
