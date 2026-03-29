use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::fmt;

use crate::Error;

// ============================================================
// Digest — sha256:<64 hex chars>
// ============================================================

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Digest {
    raw: String,
}

impl Digest {
    pub fn parse(s: &str) -> Result<Self, Error> {
        let hex = s.strip_prefix("sha256:").ok_or_else(|| {
            Error::InvalidDigest(format!("digest must start with 'sha256:': {s}"))
        })?;
        if hex.len() != 64 {
            return Err(Error::InvalidDigest(format!(
                "sha256 digest must be 64 hex chars, got {}: {s}",
                hex.len()
            )));
        }
        if !hex
            .chars()
            .all(|c| c.is_ascii_hexdigit() && !c.is_ascii_uppercase())
        {
            return Err(Error::InvalidDigest(format!(
                "digest must be lowercase hex: {s}"
            )));
        }
        Ok(Digest { raw: s.to_string() })
    }

    pub fn as_str(&self) -> &str {
        &self.raw
    }

    pub fn hex(&self) -> &str {
        &self.raw["sha256:".len()..]
    }

    /// Compute the SHA-256 digest of the given data.
    pub fn from_sha256(data: &[u8]) -> Self {
        use sha2::{Digest as Sha2Digest, Sha256};
        use std::fmt::Write;
        let mut hasher = Sha256::new();
        hasher.update(data);
        let result = hasher.finalize();
        let mut raw = String::with_capacity(7 + 64); // "sha256:" + 64 hex chars
        raw.push_str("sha256:");
        for b in result.iter() {
            write!(raw, "{b:02x}").unwrap();
        }
        Digest { raw }
    }
}

impl fmt::Display for Digest {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.raw)
    }
}

impl Serialize for Digest {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.raw)
    }
}

impl<'de> Deserialize<'de> for Digest {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let raw = String::deserialize(deserializer)?;
        Digest::parse(&raw).map_err(serde::de::Error::custom)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_digest_parse() {
        let d = Digest::parse(
            "sha256:2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d2f7d",
        )
        .unwrap();
        assert_eq!(d.hex().len(), 64);
    }

    #[test]
    fn test_digest_reject_uppercase() {
        assert!(Digest::parse(
            "sha256:2F7D2F7D2F7D2F7D2F7D2F7D2F7D2F7D2F7D2F7D2F7D2F7D2F7D2F7D2F7D2F7D"
        )
        .is_err());
    }

    #[test]
    fn test_digest_reject_short() {
        assert!(Digest::parse("sha256:abcd").is_err());
    }

    #[test]
    fn test_digest_reject_no_prefix() {
        assert!(Digest::parse(
            "abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890ab"
        )
        .is_err());
    }
}
