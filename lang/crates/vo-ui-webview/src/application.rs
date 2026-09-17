#[cfg(any(feature = "window", test))]
use sha2::{Digest, Sha256};

/// Stable application identity, independent from executable paths and titles.
/// Changing it selects a different browser storage profile.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ApplicationId(String);

impl ApplicationId {
    pub fn new(value: &str) -> Result<Self, String> {
        if value.len() > 255
            || !value.contains('.')
            || !value.split('.').all(|part| {
                part.as_bytes().first().is_some_and(u8::is_ascii_alphabetic)
                    && part
                        .bytes()
                        .all(|byte| byte.is_ascii_alphanumeric() || byte == b'-')
            })
        {
            return Err("invalid desktop application identifier".into());
        }
        Ok(Self(value.into()))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }

    #[cfg(any(feature = "window", test))]
    fn digest(&self) -> sha2::digest::Output<Sha256> {
        Sha256::new()
            .chain_update(b"volang.ui.storage.v1\0")
            .chain_update(self.0.as_bytes())
            .finalize()
    }

    #[cfg(any(all(feature = "window", target_os = "macos"), test))]
    pub(crate) fn store_id(&self) -> [u8; 16] {
        self.digest()[..16].try_into().unwrap()
    }

    #[cfg(any(all(feature = "window", not(target_os = "macos")), test))]
    pub(crate) fn directory_in(&self, root: &std::path::Path) -> std::path::PathBuf {
        // Hashing also avoids Windows reserved file names and path case folding.
        root.join("Volang")
            .join("UI")
            .join(format!("{:x}", self.digest()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn storage_identity_is_stable_distinct_and_never_an_authored_path() {
        let app = ApplicationId::new("dev.example.notes").unwrap();
        assert_eq!(app.as_str(), "dev.example.notes");
        let repeated = ApplicationId::new("dev.example.notes").unwrap();
        let other = ApplicationId::new("dev.example.other").unwrap();
        assert_eq!(app.store_id(), repeated.store_id());
        assert_ne!(app.store_id(), other.store_id());
        let root = std::path::Path::new("profile-root");
        let path = app.directory_in(root);
        assert_eq!(path.parent(), Some(root.join("Volang/UI").as_path()));
        assert_ne!(path, other.directory_in(root));
        let name = path.file_name().unwrap().to_str().unwrap();
        assert_eq!(
            name,
            "426e6305b4b2412dfc62f8f9238c0a4d2cfa6383df868b0813a8d8ba02b7dcaa"
        );
        assert_eq!(name.len(), 64);
        assert!(name.bytes().all(|byte| byte.is_ascii_hexdigit()));
        for value in [
            "",
            "single",
            "a/b.c",
            "a..b",
            "dev.中文",
            "dev.\napp",
            "a.1b",
        ] {
            assert!(ApplicationId::new(value).is_err(), "{value}");
        }
        assert!(ApplicationId::new(&format!("a.{}", "b".repeat(255))).is_err());
    }
}
