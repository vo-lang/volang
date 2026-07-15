use std::path::Path;

use sha2::{Digest, Sha256};

pub struct StableHasher {
    hasher: Sha256,
}

impl StableHasher {
    pub fn new(namespace: &str) -> Self {
        let mut hasher = Sha256::new();
        hasher.update(b"vo-stable-hash-v1");
        hasher.update((namespace.len() as u64).to_le_bytes());
        hasher.update(namespace.as_bytes());
        Self { hasher }
    }

    pub fn update_bytes(&mut self, label: &str, value: &[u8]) {
        self.hasher.update((label.len() as u64).to_le_bytes());
        self.hasher.update(label.as_bytes());
        self.hasher.update((value.len() as u64).to_le_bytes());
        self.hasher.update(value);
    }

    pub fn update_str(&mut self, label: &str, value: &str) {
        self.update_bytes(label, value.as_bytes());
    }

    pub fn update_path(&mut self, label: &str, path: &Path) {
        #[cfg(unix)]
        {
            use std::os::unix::ffi::OsStrExt;
            self.update_bytes(label, path.as_os_str().as_bytes());
        }

        #[cfg(windows)]
        {
            use std::os::windows::ffi::OsStrExt;
            let mut bytes = Vec::new();
            for unit in path.as_os_str().encode_wide() {
                bytes.extend_from_slice(&unit.to_le_bytes());
            }
            self.update_bytes(label, &bytes);
        }

        #[cfg(not(any(unix, windows)))]
        self.update_bytes(label, path.as_os_str().as_encoded_bytes());
    }

    pub fn update_bool(&mut self, label: &str, value: bool) {
        self.update_bytes(label, &[u8::from(value)]);
    }

    pub fn finish(self) -> String {
        let bytes = self.hasher.finalize();
        let hex: String = bytes.iter().map(|b| format!("{b:02x}")).collect();
        format!("sha256:{hex}")
    }

    pub fn finish_suffix(self) -> String {
        let digest = self.finish();
        digest
            .strip_prefix("sha256:")
            .unwrap_or(&digest)
            .to_string()
    }
}

#[cfg(all(test, unix))]
mod tests {
    use super::*;
    use std::ffi::OsString;
    use std::os::unix::ffi::OsStringExt;

    #[test]
    fn path_hashing_preserves_arbitrary_unix_bytes() {
        let hash = |path: &Path| {
            let mut hasher = StableHasher::new("path-test");
            hasher.update_path("path", path);
            hasher.finish()
        };
        let raw = OsString::from_vec(b"a\xffz".to_vec());
        assert_ne!(hash(Path::new(&raw)), hash(Path::new("a\u{fffd}z")));
    }
}
