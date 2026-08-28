//! Content-addressed cache for deployable AOT artifacts.

use std::env;
use std::fs::{self, File, OpenOptions};
use std::io::{self, Read, Write};
use std::path::{Path, PathBuf};
use std::process;
use std::sync::atomic::{AtomicU64, Ordering};

use sha2::{Digest, Sha256};
use vo_target::TargetSpec;

const CACHE_LAYOUT_GENERATION: &str = "v1";
const CACHE_MAGIC: &[u8; 8] = b"VOAOTC01";
const CACHE_FORMAT_VERSION: u16 = 1;
const CACHE_HEADER_BYTES: usize = 8 + 2 + 1 + 1 + 8 + 32 + 32;
const MAX_CACHED_ARTIFACT_BYTES: u64 = 512 * 1024 * 1024;
static CACHE_TEMP_COUNTER: AtomicU64 = AtomicU64::new(0);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum AotCacheArtifactKind {
    NativeObject = 1,
    CoreWasm = 2,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AotCacheKey {
    kind: AotCacheArtifactKind,
    digest: [u8; 32],
}

impl AotCacheKey {
    pub fn new(
        module_bytes: &[u8],
        target: &TargetSpec,
        kind: AotCacheArtifactKind,
        debug_ir: bool,
    ) -> Self {
        let mut hasher = Sha256::new();
        hash_field(&mut hasher, b"domain", b"volang-aot-cache-key-v1");
        hash_field(
            &mut hasher,
            b"toolchain-version",
            vo_module::TOOLCHAIN_VERSION.as_bytes(),
        );
        hash_field(
            &mut hasher,
            b"compiler-build-id",
            env!("VO_COMPILER_BUILD_ID").as_bytes(),
        );
        hash_field(&mut hasher, b"target", target.triple().as_bytes());
        hash_field(
            &mut hasher,
            b"wasm-features",
            &target.wasm_features().bits().to_le_bytes(),
        );
        hash_field(&mut hasher, b"artifact-kind", &[kind as u8]);
        hash_field(&mut hasher, b"debug-ir", &[u8::from(debug_ir)]);
        hash_field(
            &mut hasher,
            b"native-aot-abi",
            &vo_jit::NATIVE_AOT_ABI_VERSION.to_le_bytes(),
        );
        hash_field(
            &mut hasher,
            b"wasm-aot-abi",
            &vo_wasm_aot::WASM_AOT_ABI_VERSION.to_le_bytes(),
        );
        hash_field(
            &mut hasher,
            b"extension-abi-version",
            &vo_runtime::ext_loader::ABI_VERSION.to_le_bytes(),
        );
        hash_field(
            &mut hasher,
            b"extension-abi-fingerprint",
            &vo_runtime::ext_loader::ABI_FINGERPRINT.to_le_bytes(),
        );
        hash_field(&mut hasher, b"verified-module", module_bytes);
        Self {
            kind,
            digest: hasher.finalize().into(),
        }
    }

    pub fn hex_digest(&self) -> String {
        hex_digest(&self.digest)
    }
}

fn hash_field(hasher: &mut Sha256, name: &[u8], value: &[u8]) {
    hasher.update((name.len() as u64).to_le_bytes());
    hasher.update(name);
    hasher.update((value.len() as u64).to_le_bytes());
    hasher.update(value);
}

fn hex_digest(digest: &[u8; 32]) -> String {
    const HEX: &[u8; 16] = b"0123456789abcdef";
    let mut output = String::with_capacity(64);
    for byte in digest {
        output.push(HEX[usize::from(byte >> 4)] as char);
        output.push(HEX[usize::from(byte & 0x0f)] as char);
    }
    output
}

#[derive(Debug, Clone)]
pub struct AotArtifactCache {
    root: PathBuf,
}

impl AotArtifactCache {
    pub fn new(root: PathBuf) -> io::Result<Self> {
        if root.as_os_str().is_empty() || !root.is_absolute() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "AOT cache root must be a non-empty absolute path",
            ));
        }
        Ok(Self { root })
    }

    pub fn default_for_user() -> io::Result<Self> {
        if let Some(configured) = env::var_os("VO_AOT_CACHE") {
            return Self::new(PathBuf::from(configured));
        }
        let home = dirs::home_dir().ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::NotFound,
                "cannot determine the user home directory; set VO_AOT_CACHE to an absolute path",
            )
        })?;
        Self::new(home.join(".vo").join("aot").join(CACHE_LAYOUT_GENERATION))
    }

    pub fn root(&self) -> &Path {
        &self.root
    }

    fn entry_path(&self, key: &AotCacheKey) -> PathBuf {
        let digest = key.hex_digest();
        self.root.join(&digest[..2]).join(format!("{digest}.voac"))
    }

    /// Returns `None` for a missing, truncated, mismatched, or corrupt entry.
    pub fn load(&self, key: &AotCacheKey) -> io::Result<Option<Vec<u8>>> {
        let path = self.entry_path(key);
        let metadata = match fs::symlink_metadata(&path) {
            Ok(metadata) => metadata,
            Err(error) if error.kind() == io::ErrorKind::NotFound => return Ok(None),
            Err(error) => return Err(error),
        };
        if !metadata.file_type().is_file()
            || metadata.len() > MAX_CACHED_ARTIFACT_BYTES + CACHE_HEADER_BYTES as u64
        {
            return Ok(None);
        }

        let file = File::open(&path)?;
        let mut bytes = Vec::with_capacity(metadata.len() as usize);
        file.take(MAX_CACHED_ARTIFACT_BYTES + CACHE_HEADER_BYTES as u64 + 1)
            .read_to_end(&mut bytes)?;
        Ok(decode_entry(&bytes, key))
    }

    pub fn store(&self, key: &AotCacheKey, payload: &[u8]) -> io::Result<()> {
        if payload.is_empty() || payload.len() as u64 > MAX_CACHED_ARTIFACT_BYTES {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                format!(
                    "AOT artifact length {} is outside 1..={MAX_CACHED_ARTIFACT_BYTES}",
                    payload.len()
                ),
            ));
        }
        let entry = encode_entry(key, payload);
        let path = self.entry_path(key);
        let parent = path.parent().expect("cache entry always has a parent");
        fs::create_dir_all(parent)?;

        for _ in 0..100u32 {
            let counter = CACHE_TEMP_COUNTER.fetch_add(1, Ordering::Relaxed);
            let temp_path = parent.join(format!(
                ".{}.{}.{}.tmp",
                key.hex_digest(),
                process::id(),
                counter
            ));
            let mut file = match create_private_new_file(&temp_path) {
                Ok(file) => file,
                Err(error) if error.kind() == io::ErrorKind::AlreadyExists => continue,
                Err(error) => return Err(error),
            };
            let result = (|| {
                file.write_all(&entry)?;
                file.sync_all()?;
                drop(file);
                replace_file_atomically(&temp_path, &path)?;
                sync_directory(parent)
            })();
            if result.is_err() {
                let _ = fs::remove_file(&temp_path);
            }
            return result;
        }
        Err(io::Error::new(
            io::ErrorKind::AlreadyExists,
            "could not allocate a temporary AOT cache entry",
        ))
    }
}

fn encode_entry(key: &AotCacheKey, payload: &[u8]) -> Vec<u8> {
    let payload_digest: [u8; 32] = Sha256::digest(payload).into();
    let mut entry = Vec::with_capacity(CACHE_HEADER_BYTES + payload.len());
    entry.extend_from_slice(CACHE_MAGIC);
    entry.extend_from_slice(&CACHE_FORMAT_VERSION.to_le_bytes());
    entry.push(key.kind as u8);
    entry.push(0);
    entry.extend_from_slice(&(payload.len() as u64).to_le_bytes());
    entry.extend_from_slice(&key.digest);
    entry.extend_from_slice(&payload_digest);
    entry.extend_from_slice(payload);
    entry
}

fn decode_entry(bytes: &[u8], key: &AotCacheKey) -> Option<Vec<u8>> {
    if bytes.len() < CACHE_HEADER_BYTES || bytes.get(..8)? != CACHE_MAGIC {
        return None;
    }
    if u16::from_le_bytes(bytes.get(8..10)?.try_into().ok()?) != CACHE_FORMAT_VERSION
        || *bytes.get(10)? != key.kind as u8
        || *bytes.get(11)? != 0
    {
        return None;
    }
    let payload_len = u64::from_le_bytes(bytes.get(12..20)?.try_into().ok()?);
    if payload_len == 0 || payload_len > MAX_CACHED_ARTIFACT_BYTES {
        return None;
    }
    if bytes.get(20..52)? != key.digest {
        return None;
    }
    let expected_len = CACHE_HEADER_BYTES.checked_add(usize::try_from(payload_len).ok()?)?;
    if bytes.len() != expected_len {
        return None;
    }
    let payload = bytes.get(CACHE_HEADER_BYTES..)?;
    let digest: [u8; 32] = Sha256::digest(payload).into();
    if bytes.get(52..84)? != digest {
        return None;
    }
    Some(payload.to_vec())
}

#[cfg(unix)]
fn create_private_new_file(path: &Path) -> io::Result<File> {
    use std::os::unix::fs::OpenOptionsExt;
    OpenOptions::new()
        .write(true)
        .create_new(true)
        .mode(0o600)
        .open(path)
}

#[cfg(not(unix))]
fn create_private_new_file(path: &Path) -> io::Result<File> {
    OpenOptions::new().write(true).create_new(true).open(path)
}

#[cfg(not(windows))]
fn replace_file_atomically(from: &Path, to: &Path) -> io::Result<()> {
    fs::rename(from, to)
}

#[cfg(windows)]
fn replace_file_atomically(from: &Path, to: &Path) -> io::Result<()> {
    use std::iter::once;
    use std::os::windows::ffi::OsStrExt;

    #[link(name = "kernel32")]
    extern "system" {
        fn MoveFileExW(existing: *const u16, replacement: *const u16, flags: u32) -> i32;
    }

    const MOVEFILE_REPLACE_EXISTING: u32 = 0x1;
    const MOVEFILE_WRITE_THROUGH: u32 = 0x8;
    let from = from
        .as_os_str()
        .encode_wide()
        .chain(once(0))
        .collect::<Vec<_>>();
    let to = to
        .as_os_str()
        .encode_wide()
        .chain(once(0))
        .collect::<Vec<_>>();
    let result = unsafe {
        MoveFileExW(
            from.as_ptr(),
            to.as_ptr(),
            MOVEFILE_REPLACE_EXISTING | MOVEFILE_WRITE_THROUGH,
        )
    };
    if result == 0 {
        Err(io::Error::last_os_error())
    } else {
        Ok(())
    }
}

fn sync_directory(path: &Path) -> io::Result<()> {
    #[cfg(unix)]
    File::open(path)?.sync_all()?;
    #[cfg(not(unix))]
    let _ = path;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::{SystemTime, UNIX_EPOCH};
    use vo_target::WASM32_UNKNOWN_UNKNOWN;

    fn temp_cache(label: &str) -> (PathBuf, AotArtifactCache) {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let root = env::temp_dir().join(format!("vo-aot-cache-{label}-{}-{unique}", process::id()));
        let cache = AotArtifactCache::new(root.clone()).unwrap();
        (root, cache)
    }

    #[test]
    fn key_covers_target_kind_options_and_module() {
        let target = TargetSpec::parse(WASM32_UNKNOWN_UNKNOWN).unwrap();
        let base = AotCacheKey::new(b"module", &target, AotCacheArtifactKind::CoreWasm, false);
        assert_ne!(
            base,
            AotCacheKey::new(b"other", &target, AotCacheArtifactKind::CoreWasm, false)
        );
        assert_ne!(
            base,
            AotCacheKey::new(b"module", &target, AotCacheArtifactKind::CoreWasm, true)
        );
    }

    #[test]
    fn round_trip_and_corruption_is_a_miss() {
        let (root, cache) = temp_cache("round-trip");
        let target = TargetSpec::parse(WASM32_UNKNOWN_UNKNOWN).unwrap();
        let key = AotCacheKey::new(b"module", &target, AotCacheArtifactKind::CoreWasm, false);
        assert_eq!(cache.load(&key).unwrap(), None);
        cache.store(&key, b"artifact").unwrap();
        assert_eq!(cache.load(&key).unwrap(), Some(b"artifact".to_vec()));

        let path = cache.entry_path(&key);
        let mut bytes = fs::read(&path).unwrap();
        *bytes.last_mut().unwrap() ^= 0xff;
        fs::write(path, bytes).unwrap();
        assert_eq!(cache.load(&key).unwrap(), None);
        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn rejects_relative_roots_and_symlink_entries() {
        assert!(AotArtifactCache::new(PathBuf::from("relative")).is_err());
        #[cfg(unix)]
        {
            use std::os::unix::fs::symlink;
            let (root, cache) = temp_cache("symlink");
            let target = TargetSpec::parse(WASM32_UNKNOWN_UNKNOWN).unwrap();
            let key = AotCacheKey::new(b"module", &target, AotCacheArtifactKind::CoreWasm, false);
            let path = cache.entry_path(&key);
            fs::create_dir_all(path.parent().unwrap()).unwrap();
            let target_file = root.join("target");
            fs::write(&target_file, b"untrusted").unwrap();
            symlink(target_file, path).unwrap();
            assert_eq!(cache.load(&key).unwrap(), None);
            fs::remove_dir_all(root).unwrap();
        }
    }
}
