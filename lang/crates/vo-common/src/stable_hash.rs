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
        self.update_str(label, &path.to_string_lossy());
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
        digest.strip_prefix("sha256:").unwrap_or(&digest).to_string()
    }
}
