use std::fmt;
use std::fs;
use std::io::Write;
use std::path::{Component, Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};

use ring::signature::{UnparsedPublicKey, ED25519};
use sha2::{Digest, Sha256};

const MAX_UPDATE_FILES: usize = 100_000;
const MAX_UPDATE_FILE_BYTES: u64 = 2 * 1024 * 1024 * 1024;
const MAX_UPDATE_TOTAL_BYTES: u64 = 8 * 1024 * 1024 * 1024;
static NEXT_TRANSACTION: AtomicU64 = AtomicU64::new(1);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DesktopUpdateFile {
    pub path: String,
    pub size: u64,
    pub sha256: [u8; 32],
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DesktopUpdateManifest {
    pub application_id: String,
    pub version: String,
    pub minimum_version: String,
    pub target: String,
    pub published_unix_millis: u64,
    pub files: Vec<DesktopUpdateFile>,
    pub signature: Vec<u8>,
}

impl DesktopUpdateManifest {
    pub fn canonical_payload(&self) -> Result<Vec<u8>, DesktopUpdateError> {
        self.validate()?;
        let mut output = b"volang.desktop.update.v1\0".to_vec();
        encode_text(&mut output, &self.application_id)?;
        encode_text(&mut output, &self.version)?;
        encode_text(&mut output, &self.minimum_version)?;
        encode_text(&mut output, &self.target)?;
        output.extend_from_slice(&self.published_unix_millis.to_le_bytes());
        output.extend_from_slice(&(self.files.len() as u64).to_le_bytes());
        for file in &self.files {
            encode_text(&mut output, &file.path)?;
            output.extend_from_slice(&file.size.to_le_bytes());
            output.extend_from_slice(&file.sha256);
        }
        Ok(output)
    }

    fn validate(&self) -> Result<(), DesktopUpdateError> {
        if !valid_token(&self.application_id)
            || !valid_token(&self.version)
            || (!self.minimum_version.is_empty() && !valid_token(&self.minimum_version))
            || !valid_token(&self.target)
            || self.files.is_empty()
            || self.files.len() > MAX_UPDATE_FILES
        {
            return Err(DesktopUpdateError::InvalidManifest(
                "invalid identity, target, or file count",
            ));
        }
        let mut total = 0_u64;
        let mut previous: Option<&str> = None;
        for file in &self.files {
            if !valid_relative_path(&file.path)
                || file.size > MAX_UPDATE_FILE_BYTES
                || previous.is_some_and(|path| path >= file.path.as_str())
            {
                return Err(DesktopUpdateError::InvalidManifest(
                    "update files must be sorted unique bounded relative paths",
                ));
            }
            total = total
                .checked_add(file.size)
                .ok_or(DesktopUpdateError::InvalidManifest("update size overflow"))?;
            previous = Some(&file.path);
        }
        if total > MAX_UPDATE_TOTAL_BYTES {
            return Err(DesktopUpdateError::InvalidManifest(
                "update exceeds total size limit",
            ));
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum DesktopUpdateError {
    InvalidManifest(&'static str),
    InvalidSignature,
    TargetMismatch,
    SourceChanged(String),
    AlreadyStaged,
    MissingVersion,
    NoRollbackVersion,
    Io(String),
}

impl fmt::Display for DesktopUpdateError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidManifest(message) => {
                write!(formatter, "invalid desktop update manifest: {message}")
            }
            Self::InvalidSignature => formatter.write_str("desktop update signature is invalid"),
            Self::TargetMismatch => {
                formatter.write_str("desktop update target does not match this package")
            }
            Self::SourceChanged(path) => write!(formatter, "desktop update source changed: {path}"),
            Self::AlreadyStaged => formatter.write_str("desktop update version is already staged"),
            Self::MissingVersion => formatter.write_str("desktop update version is not staged"),
            Self::NoRollbackVersion => {
                formatter.write_str("desktop update has no rollback version")
            }
            Self::Io(message) => write!(formatter, "desktop update I/O failed: {message}"),
        }
    }
}

impl std::error::Error for DesktopUpdateError {}

pub struct DesktopUpdateStore {
    root: PathBuf,
    application_id: String,
    target: String,
    public_key: [u8; 32],
}

impl DesktopUpdateStore {
    pub fn new(
        root: PathBuf,
        application_id: String,
        target: String,
        public_key: [u8; 32],
    ) -> Result<Self, DesktopUpdateError> {
        if !valid_token(&application_id) || !valid_token(&target) {
            return Err(DesktopUpdateError::InvalidManifest(
                "invalid update store identity",
            ));
        }
        Ok(Self {
            root,
            application_id,
            target,
            public_key,
        })
    }

    pub fn stage(
        &self,
        source: &Path,
        manifest: &DesktopUpdateManifest,
    ) -> Result<PathBuf, DesktopUpdateError> {
        if manifest.application_id != self.application_id || manifest.target != self.target {
            return Err(DesktopUpdateError::TargetMismatch);
        }
        let payload = manifest.canonical_payload()?;
        UnparsedPublicKey::new(&ED25519, self.public_key)
            .verify(&payload, &manifest.signature)
            .map_err(|_| DesktopUpdateError::InvalidSignature)?;
        let versions = self.root.join("versions");
        fs::create_dir_all(&versions).map_err(io_error)?;
        let final_path = versions.join(&manifest.version);
        if final_path.exists() {
            return Err(DesktopUpdateError::AlreadyStaged);
        }
        let transaction = NEXT_TRANSACTION.fetch_add(1, Ordering::Relaxed);
        let staging = versions.join(format!(".staging-{}-{transaction}", std::process::id()));
        fs::create_dir(&staging).map_err(io_error)?;
        let mut guard = StagingGuard(Some(staging.clone()));
        for file in &manifest.files {
            let source_path = source.join(&file.path);
            if !source_file_is_regular(source, &file.path, file.size)? {
                return Err(DesktopUpdateError::SourceChanged(file.path.clone()));
            }
            let bytes = fs::read(&source_path).map_err(io_error)?;
            if bytes.len() as u64 != file.size || Sha256::digest(&bytes).as_slice() != file.sha256 {
                return Err(DesktopUpdateError::SourceChanged(file.path.clone()));
            }
            let destination = staging.join(&file.path);
            if let Some(parent) = destination.parent() {
                fs::create_dir_all(parent).map_err(io_error)?;
            }
            let mut output = fs::OpenOptions::new()
                .write(true)
                .create_new(true)
                .open(&destination)
                .map_err(io_error)?;
            output
                .write_all(&bytes)
                .and_then(|()| output.sync_all())
                .map_err(io_error)?;
        }
        fs::rename(&staging, &final_path).map_err(io_error)?;
        guard.0 = None;
        Ok(final_path)
    }

    pub fn activate(&self, version: &str) -> Result<(), DesktopUpdateError> {
        if !valid_token(version) || !self.root.join("versions").join(version).is_dir() {
            return Err(DesktopUpdateError::MissingVersion);
        }
        if let Some(active) = self.active_version()? {
            if active == version {
                return Ok(());
            }
            write_marker(&self.root, "previous", &active)?;
        }
        write_marker(&self.root, "active", version)
    }

    pub fn rollback(&self) -> Result<String, DesktopUpdateError> {
        let previous =
            read_marker(&self.root, "previous")?.ok_or(DesktopUpdateError::NoRollbackVersion)?;
        if !self.root.join("versions").join(&previous).is_dir() {
            return Err(DesktopUpdateError::MissingVersion);
        }
        let active = self.active_version()?;
        write_marker(&self.root, "active", &previous)?;
        if let Some(active) = active {
            write_marker(&self.root, "previous", &active)?;
        }
        Ok(previous)
    }

    pub fn active_version(&self) -> Result<Option<String>, DesktopUpdateError> {
        read_marker(&self.root, "active")
    }
}

struct StagingGuard(Option<PathBuf>);

impl Drop for StagingGuard {
    fn drop(&mut self) {
        if let Some(path) = self.0.take() {
            let _ = fs::remove_dir_all(path);
        }
    }
}

struct TemporaryFileGuard(Option<PathBuf>);

impl Drop for TemporaryFileGuard {
    fn drop(&mut self) {
        if let Some(path) = self.0.take() {
            let _ = fs::remove_file(path);
        }
    }
}

fn source_file_is_regular(
    root: &Path,
    relative: &str,
    expected_size: u64,
) -> Result<bool, DesktopUpdateError> {
    let root_metadata = fs::symlink_metadata(root).map_err(io_error)?;
    if !root_metadata.is_dir() || root_metadata.file_type().is_symlink() {
        return Ok(false);
    }
    let components = Path::new(relative).components().collect::<Vec<_>>();
    let mut current = root.to_path_buf();
    for (index, component) in components.iter().enumerate() {
        let Component::Normal(component) = component else {
            return Ok(false);
        };
        current.push(component);
        let metadata = fs::symlink_metadata(&current).map_err(io_error)?;
        if metadata.file_type().is_symlink() {
            return Ok(false);
        }
        let final_component = index + 1 == components.len();
        if (final_component && (!metadata.is_file() || metadata.len() != expected_size))
            || (!final_component && !metadata.is_dir())
        {
            return Ok(false);
        }
    }
    Ok(!components.is_empty())
}

fn valid_token(value: &str) -> bool {
    !value.is_empty()
        && value.len() <= 255
        && !value.starts_with('.')
        && value
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'.' | b'-' | b'_'))
}

fn valid_relative_path(value: &str) -> bool {
    if value.is_empty() || value.contains('\\') || value.bytes().any(|byte| byte.is_ascii_control())
    {
        return false;
    }
    let path = Path::new(value);
    !path.is_absolute()
        && path
            .components()
            .all(|component| matches!(component, Component::Normal(_)))
}

fn encode_text(output: &mut Vec<u8>, value: &str) -> Result<(), DesktopUpdateError> {
    let length = u32::try_from(value.len())
        .map_err(|_| DesktopUpdateError::InvalidManifest("manifest text is oversized"))?;
    output.extend_from_slice(&length.to_le_bytes());
    output.extend_from_slice(value.as_bytes());
    Ok(())
}

fn io_error(error: std::io::Error) -> DesktopUpdateError {
    DesktopUpdateError::Io(error.to_string())
}

fn write_marker(root: &Path, name: &str, value: &str) -> Result<(), DesktopUpdateError> {
    fs::create_dir_all(root).map_err(io_error)?;
    let transaction = NEXT_TRANSACTION.fetch_add(1, Ordering::Relaxed);
    let temporary = root.join(format!(
        ".{name}.{}.{}.tmp",
        std::process::id(),
        transaction
    ));
    let target = root.join(name);
    let mut guard = TemporaryFileGuard(Some(temporary.clone()));
    let mut file = fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(&temporary)
        .map_err(io_error)?;
    file.write_all(value.as_bytes())
        .and_then(|()| file.write_all(b"\n"))
        .and_then(|()| file.sync_all())
        .map_err(io_error)?;
    fs::rename(&temporary, &target).map_err(io_error)?;
    guard.0 = None;
    Ok(())
}

fn read_marker(root: &Path, name: &str) -> Result<Option<String>, DesktopUpdateError> {
    let path = root.join(name);
    match fs::read_to_string(path) {
        Ok(value) => {
            let value = value.trim_end_matches(['\r', '\n']).to_string();
            if !valid_token(&value) {
                return Err(DesktopUpdateError::InvalidManifest(
                    "update marker is invalid",
                ));
            }
            Ok(Some(value))
        }
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(None),
        Err(error) => Err(io_error(error)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ring::signature::{Ed25519KeyPair, KeyPair};

    fn temporary(name: &str) -> PathBuf {
        std::env::temp_dir().join(format!(
            "volang-update-{name}-{}-{}",
            std::process::id(),
            NEXT_TRANSACTION.fetch_add(1, Ordering::Relaxed)
        ))
    }

    fn signed_manifest(
        pair: &Ed25519KeyPair,
        version: &str,
        bytes: &[u8],
    ) -> DesktopUpdateManifest {
        let mut manifest = DesktopUpdateManifest {
            application_id: "com.volang.studio".to_string(),
            version: version.to_string(),
            minimum_version: "1.0.0".to_string(),
            target: "aarch64-apple-darwin".to_string(),
            published_unix_millis: 1,
            files: vec![DesktopUpdateFile {
                path: "bin/studio".to_string(),
                size: bytes.len() as u64,
                sha256: Sha256::digest(bytes).into(),
            }],
            signature: Vec::new(),
        };
        manifest.signature = pair
            .sign(&manifest.canonical_payload().unwrap())
            .as_ref()
            .to_vec();
        manifest
    }

    #[test]
    fn signed_staging_activation_and_rollback_preserve_versions() {
        let pair = Ed25519KeyPair::from_seed_unchecked(&[7_u8; 32]).unwrap();
        let mut key = [0_u8; 32];
        key.copy_from_slice(pair.public_key().as_ref());
        let root = temporary("store");
        let source = temporary("source");
        fs::create_dir_all(source.join("bin")).unwrap();
        let store = DesktopUpdateStore::new(
            root.clone(),
            "com.volang.studio".to_string(),
            "aarch64-apple-darwin".to_string(),
            key,
        )
        .unwrap();

        fs::write(source.join("bin/studio"), b"version one").unwrap();
        store
            .stage(&source, &signed_manifest(&pair, "1.0.0", b"version one"))
            .unwrap();
        store.activate("1.0.0").unwrap();
        fs::write(source.join("bin/studio"), b"version two").unwrap();
        store
            .stage(&source, &signed_manifest(&pair, "2.0.0", b"version two"))
            .unwrap();
        store.activate("2.0.0").unwrap();
        assert_eq!(store.active_version().unwrap().as_deref(), Some("2.0.0"));
        assert_eq!(store.rollback().unwrap(), "1.0.0");
        assert_eq!(store.active_version().unwrap().as_deref(), Some("1.0.0"));
        assert!(root.join("versions/2.0.0/bin/studio").is_file());

        fs::remove_dir_all(root).unwrap();
        fs::remove_dir_all(source).unwrap();
    }

    #[test]
    fn traversal_and_changed_sources_fail_closed() {
        let pair = Ed25519KeyPair::from_seed_unchecked(&[9_u8; 32]).unwrap();
        let mut manifest = signed_manifest(&pair, "1.0.0", b"safe");
        manifest.files[0].path = "../escape".to_string();
        assert!(matches!(
            manifest.canonical_payload(),
            Err(DesktopUpdateError::InvalidManifest(_))
        ));

        let mut key = [0_u8; 32];
        key.copy_from_slice(pair.public_key().as_ref());
        let root = temporary("fail-closed-store");
        let source = temporary("fail-closed-source");
        fs::create_dir_all(source.join("bin")).unwrap();
        fs::write(source.join("bin/studio"), b"changed").unwrap();
        let store = DesktopUpdateStore::new(
            root.clone(),
            "com.volang.studio".to_string(),
            "aarch64-apple-darwin".to_string(),
            key,
        )
        .unwrap();
        assert!(matches!(
            store.stage(&source, &signed_manifest(&pair, "1.0.0", b"safe")),
            Err(DesktopUpdateError::SourceChanged(path)) if path == "bin/studio"
        ));
        let versions = root.join("versions");
        assert!(!versions.join("1.0.0").exists());
        assert!(fs::read_dir(&versions).unwrap().next().is_none());

        fs::write(source.join("bin/studio"), b"safe").unwrap();
        let mut forged = signed_manifest(&pair, "1.0.0", b"safe");
        forged.signature[0] ^= 0xff;
        assert_eq!(
            store.stage(&source, &forged),
            Err(DesktopUpdateError::InvalidSignature)
        );
        assert!(fs::read_dir(&versions).unwrap().next().is_none());

        let wrong_target = DesktopUpdateStore::new(
            temporary("wrong-target-store"),
            "com.volang.studio".to_string(),
            "x86_64-unknown-linux-gnu".to_string(),
            key,
        )
        .unwrap();
        assert_eq!(
            wrong_target.stage(&source, &signed_manifest(&pair, "1.0.0", b"safe")),
            Err(DesktopUpdateError::TargetMismatch)
        );

        fs::remove_dir_all(root).unwrap();
        fs::remove_dir_all(source).unwrap();
    }

    #[cfg(unix)]
    #[test]
    fn source_directory_symlinks_are_rejected() {
        use std::os::unix::fs::symlink;

        let pair = Ed25519KeyPair::from_seed_unchecked(&[11_u8; 32]).unwrap();
        let mut key = [0_u8; 32];
        key.copy_from_slice(pair.public_key().as_ref());
        let root = temporary("symlink-store");
        let source = temporary("symlink-source");
        let external = temporary("symlink-external");
        fs::create_dir_all(&source).unwrap();
        fs::create_dir_all(&external).unwrap();
        fs::write(external.join("studio"), b"safe").unwrap();
        symlink(&external, source.join("bin")).unwrap();
        let store = DesktopUpdateStore::new(
            root.clone(),
            "com.volang.studio".to_string(),
            "aarch64-apple-darwin".to_string(),
            key,
        )
        .unwrap();
        assert!(matches!(
            store.stage(&source, &signed_manifest(&pair, "1.0.0", b"safe")),
            Err(DesktopUpdateError::SourceChanged(path)) if path == "bin/studio"
        ));

        fs::remove_dir_all(root).unwrap();
        fs::remove_dir_all(source).unwrap();
        fs::remove_dir_all(external).unwrap();
    }
}
