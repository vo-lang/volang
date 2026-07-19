use std::ffi::{OsStr, OsString};
#[cfg(any(target_os = "linux", target_os = "macos"))]
use std::fs::File;
use std::io;
#[cfg(any(target_os = "linux", target_os = "macos"))]
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
#[cfg(any(target_os = "linux", target_os = "macos"))]
use std::sync::atomic::{AtomicU64, Ordering};
#[cfg(any(target_os = "linux", target_os = "macos"))]
use std::time::{SystemTime, UNIX_EPOCH};

#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
use std::collections::{BTreeMap, BTreeSet};
#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
use std::sync::{mpsc, Arc, Mutex, OnceLock};
#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
use std::time::Duration;

#[cfg(any(target_os = "linux", target_os = "macos"))]
use std::ffi::{CStr, CString};
#[cfg(any(target_os = "linux", target_os = "macos"))]
use std::fs::OpenOptions;
#[cfg(any(target_os = "linux", target_os = "macos"))]
use std::os::fd::{AsRawFd, FromRawFd};
#[cfg(any(target_os = "linux", target_os = "macos"))]
use std::os::unix::ffi::{OsStrExt, OsStringExt};
#[cfg(any(target_os = "linux", target_os = "macos"))]
use std::os::unix::fs::OpenOptionsExt;

use vo_common::vfs::{FileSystemEntryKind, MAX_DIRECTORY_ENTRIES};

#[cfg(any(test, target_os = "linux", target_os = "macos"))]
use crate::cache::layout::{STAGING_DIR, STAGING_LOCK_FILE};
#[cfg(any(target_os = "linux", target_os = "macos"))]
use crate::digest::Digest;
use crate::Error;

#[cfg(any(target_os = "linux", target_os = "macos"))]
static TRANSACTION_COUNTER: AtomicU64 = AtomicU64::new(0);
const SECURE_CACHE_MUTATION_SUPPORTED: bool = cfg!(any(target_os = "linux", target_os = "macos"));
pub(crate) const CACHE_OWNER_MARKER: &str = ".vo-cache-owner";
#[cfg(any(target_os = "linux", target_os = "macos"))]
const CACHE_OWNER_MARKER_CONTENT: &[u8] = b"volang-module-cache-v1\n";
#[cfg(any(target_os = "linux", target_os = "macos"))]
const OWNER_MARKER_ACQUISITION_ATTEMPTS: usize = 64;

/// An operating-system lock shared by cache installers and held exclusively by
/// cache cleanup. File locks are released by the OS after process failure, so
/// abandoned transaction trees can be collected without racing live writers.
#[derive(Debug)]
pub(crate) struct CacheMutationLock {
    #[cfg(any(target_os = "linux", target_os = "macos"))]
    file: File,
    /// Identity-only guards retain the parent shared cache lease until their
    /// identity lock has been released during field destruction.
    #[cfg(any(target_os = "linux", target_os = "macos"))]
    _retained_cache_lease: Option<File>,
    #[cfg(any(target_os = "linux", target_os = "macos"))]
    cache_root: Option<AnchoredDirectory>,
    #[cfg(any(target_os = "linux", target_os = "macos"))]
    owner_marker: Option<File>,
    #[cfg(any(target_os = "linux", target_os = "macos"))]
    staging: Option<AnchoredDirectory>,
}

impl CacheMutationLock {
    pub(crate) fn shared(cache_root: &Path) -> Result<Self, Error> {
        Self::acquire(cache_root, false, true)
    }

    /// Acquire a shared capability without creating the cache root, owner
    /// marker, staging directory, or lock file. Read-only commands use this
    /// path so verification cannot initialize cache state.
    pub(crate) fn shared_existing(cache_root: &Path) -> Result<Self, Error> {
        Self::acquire(cache_root, false, false)
    }

    pub(crate) fn exclusive(cache_root: &Path) -> Result<Self, Error> {
        Self::acquire(cache_root, true, false)
    }

    /// Serialize publication through a bounded lock pool. The returned guard
    /// retains this guard's shared cache lease, so cleanup remains excluded
    /// even when the originating guard is dropped first. Digest collisions
    /// safely serialize unrelated identities without exposing untrusted names
    /// or growing lock files.
    pub(crate) fn identity_lock(&self, identity: &str) -> Result<Self, Error> {
        #[cfg(any(target_os = "linux", target_os = "macos"))]
        {
            self.cache_root.as_ref().ok_or_else(|| {
                invalid_cache_state(
                    "identity-only cache lock cannot create another identity lock".to_string(),
                )
            })?;
            self.validate_cache_ownership()?;
            let retained_cache_lease = self.file.try_clone()?;
            let staging = self.open_locked_staging_directory()?;
            let locks = staging
                .ensure_relative_directory(Path::new("locks"), "module cache identity locks")?;
            let digest = Digest::from_sha256(identity.as_bytes());
            let digest_hex = &digest.as_str()["sha256:".len()..];
            let name = OsString::from(format!("slot-{}.lock", &digest_hex[..2]));
            let file = open_lock_file_at(&locks, &name, "module cache identity lock", true)?;
            file.lock()?;
            staging.require_child_identity(
                OsStr::new("locks"),
                &locks,
                "module cache identity-lock directory",
            )?;
            validate_open_file_at(&file, &locks, &name, "module cache identity lock")?;
            self.validate_cache_ownership()?;
            Ok(Self {
                file,
                _retained_cache_lease: Some(retained_cache_lease),
                cache_root: None,
                owner_marker: None,
                staging: None,
            })
        }

        #[cfg(not(any(target_os = "linux", target_os = "macos")))]
        {
            let _ = identity;
            Err(unsupported_secure_cache_operation())
        }
    }

    fn acquire(cache_root: &Path, exclusive: bool, create: bool) -> Result<Self, Error> {
        require_secure_cache_mutation_support()?;
        #[cfg(any(target_os = "linux", target_os = "macos"))]
        {
            let cache_root = normalize_cache_root_path(cache_root)?;
            let anchored_root = open_cache_root(&cache_root, create)?;
            let owner_marker = open_cache_owner_marker(&anchored_root, create)?;
            let staging = if create {
                anchored_root.ensure_relative_directory(
                    Path::new(STAGING_DIR),
                    "module cache staging directory",
                )?
            } else {
                anchored_root
                    .open_child(OsStr::new(STAGING_DIR), "module cache staging directory")?
            };
            let file = open_lock_file_at(
                &staging,
                OsStr::new(STAGING_LOCK_FILE),
                "module cache staging lock",
                create,
            )?;
            if exclusive {
                file.lock()?;
            } else {
                file.lock_shared()?;
            }
            anchored_root.require_path_identity(&cache_root, "module cache root")?;
            anchored_root.require_child_identity(
                OsStr::new(STAGING_DIR),
                &staging,
                "module cache staging directory",
            )?;
            validate_open_file_at(
                &file,
                &staging,
                OsStr::new(STAGING_LOCK_FILE),
                "module cache staging lock",
            )?;
            validate_cache_owner_marker_file(&owner_marker, &anchored_root)?;
            let acquired = Self {
                file,
                _retained_cache_lease: None,
                cache_root: Some(anchored_root),
                owner_marker: Some(owner_marker),
                staging: Some(staging),
            };
            acquired.open_locked_staging_directory()?;
            Ok(acquired)
        }

        #[cfg(not(any(target_os = "linux", target_os = "macos")))]
        {
            let _ = (cache_root, exclusive, create);
            unreachable!("secure cache mutation capability gate returned success")
        }
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    pub(crate) fn cache_root_directory(&self) -> Result<AnchoredDirectory, Error> {
        let root = self.cache_root.as_ref().ok_or_else(|| {
            invalid_cache_state("identity-only cache lock has no cache-root capability".to_string())
        })?;
        self.validate_cache_ownership()?;
        root.duplicate()
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    pub(crate) fn validate_cache_ownership(&self) -> Result<(), Error> {
        let root = self.cache_root.as_ref().ok_or_else(|| {
            invalid_cache_state("identity-only cache lock has no cache-root capability".to_string())
        })?;
        let marker = self.owner_marker.as_ref().ok_or_else(|| {
            invalid_cache_state("cache-root capability has no owner marker".to_string())
        })?;
        let staging = self.staging.as_ref().ok_or_else(|| {
            invalid_cache_state("cache-root capability has no staging directory".to_string())
        })?;
        validate_cache_authority(root, marker, staging, &self.file)
    }

    #[cfg(not(any(target_os = "linux", target_os = "macos")))]
    pub(crate) fn cache_root_directory(&self) -> Result<AnchoredDirectory, Error> {
        Err(unsupported_secure_cache_operation())
    }

    #[cfg(not(any(target_os = "linux", target_os = "macos")))]
    pub(crate) fn validate_cache_ownership(&self) -> Result<(), Error> {
        Err(unsupported_secure_cache_operation())
    }

    pub(crate) fn publish_noreplace(&self, from: &Path, to: &Path) -> Result<(), Error> {
        #[cfg(any(target_os = "linux", target_os = "macos"))]
        {
            let cache_root = self.cache_root.as_ref().ok_or_else(|| {
                invalid_cache_state(
                    "identity-only cache lock cannot authorize publication".to_string(),
                )
            })?;
            let owner_marker = self.owner_marker.as_ref().ok_or_else(|| {
                invalid_cache_state(
                    "cache-root publication capability has no owner marker".to_string(),
                )
            })?;
            let staging = self.staging.as_ref().ok_or_else(|| {
                invalid_cache_state(
                    "cache-root publication capability has no staging directory".to_string(),
                )
            })?;
            self.validate_cache_ownership()?;
            publish_from_anchor(cache_root, owner_marker, staging, &self.file, from, to)
        }

        #[cfg(not(any(target_os = "linux", target_os = "macos")))]
        {
            let _ = (from, to);
            Err(unsupported_secure_cache_operation())
        }
    }

    pub(crate) fn begin_transaction(&self, identity: &str) -> Result<CacheTransaction, Error> {
        #[cfg(any(target_os = "linux", target_os = "macos"))]
        {
            let root = self.cache_root.as_ref().ok_or_else(|| {
                invalid_cache_state(
                    "identity-only cache lock cannot create a transaction".to_string(),
                )
            })?;
            self.validate_cache_ownership()?;
            let staging = self.open_locked_staging_directory()?;
            // Duplicate every capability before creating a transaction entry.
            // A descriptor-allocation failure must not leave an abandoned
            // directory behind in an otherwise healthy cache.
            let transaction_root = root.duplicate()?;
            let transaction_lease = self.file.try_clone()?;
            let transaction_owner_marker = self
                .owner_marker
                .as_ref()
                .ok_or_else(|| {
                    invalid_cache_state(
                        "cache-root transaction capability has no owner marker".to_string(),
                    )
                })?
                .try_clone()?;
            for _ in 0..64 {
                let name = next_transaction_name(identity);
                staging.reject_portable_child_aliases(&name, "module cache transaction")?;
                match staging.create_child_directory(&name, "module cache transaction") {
                    Ok(directory) => {
                        if let Err(error) = self.validate_cache_ownership() {
                            let _ = staging.remove_open_directory(
                                &name,
                                &directory,
                                "module cache transaction",
                            );
                            return Err(error);
                        }
                        return Ok(CacheTransaction {
                            relative_path: PathBuf::from(STAGING_DIR).join(&name),
                            state: CacheTransactionState::Active,
                            root: transaction_root,
                            lease_file: transaction_lease,
                            owner_marker: transaction_owner_marker,
                            staging,
                            directory,
                            name,
                        });
                    }
                    Err(Error::Io(error)) if error.kind() == io::ErrorKind::AlreadyExists => {
                        continue;
                    }
                    Err(error) => return Err(error),
                }
            }
            Err(invalid_cache_state(
                "failed to allocate a unique module cache transaction directory".to_string(),
            ))
        }

        #[cfg(not(any(target_os = "linux", target_os = "macos")))]
        {
            let _ = identity;
            Err(unsupported_secure_cache_operation())
        }
    }

    pub(crate) fn ensure_directory(&self, relative: &Path) -> Result<(), Error> {
        #[cfg(any(target_os = "linux", target_os = "macos"))]
        {
            let root = self.cache_root.as_ref().ok_or_else(|| {
                invalid_cache_state(
                    "identity-only cache lock cannot create cache directories".to_string(),
                )
            })?;
            self.validate_cache_ownership()?;
            root.ensure_relative_directory(relative, "module cache directory")?;
            self.validate_cache_ownership()?;
            Ok(())
        }

        #[cfg(not(any(target_os = "linux", target_os = "macos")))]
        {
            let _ = relative;
            Err(unsupported_secure_cache_operation())
        }
    }

    pub(crate) fn entry_kind(&self, relative: &Path) -> Result<FileSystemEntryKind, Error> {
        #[cfg(any(target_os = "linux", target_os = "macos"))]
        {
            let root = self.cache_root.as_ref().ok_or_else(|| {
                invalid_cache_state(
                    "identity-only cache lock cannot inspect cache entries".to_string(),
                )
            })?;
            self.validate_cache_ownership()?;
            let kind = root.relative_entry_kind(relative)?;
            self.validate_cache_ownership()?;
            Ok(kind)
        }

        #[cfg(not(any(target_os = "linux", target_os = "macos")))]
        {
            let _ = relative;
            Err(unsupported_secure_cache_operation())
        }
    }

    pub(crate) fn open_directory(
        &self,
        relative: &Path,
        context: &str,
    ) -> Result<AnchoredDirectory, Error> {
        #[cfg(any(target_os = "linux", target_os = "macos"))]
        {
            let root = self.cache_root.as_ref().ok_or_else(|| {
                invalid_cache_state(
                    "identity-only cache lock cannot inspect cache directories".to_string(),
                )
            })?;
            self.validate_cache_ownership()?;
            let directory = root.open_relative_directory(relative, context)?;
            self.validate_cache_ownership()?;
            Ok(directory)
        }

        #[cfg(not(any(target_os = "linux", target_os = "macos")))]
        {
            let _ = (relative, context);
            Err(unsupported_secure_cache_operation())
        }
    }

    pub(crate) fn file_system(&self) -> AnchoredCacheFs<'_> {
        AnchoredCacheFs { cache_lock: self }
    }

    fn read_file(&self, relative: &Path, max_bytes: usize) -> Result<Vec<u8>, Error> {
        #[cfg(any(target_os = "linux", target_os = "macos"))]
        {
            let root = self.cache_root.as_ref().ok_or_else(|| {
                invalid_cache_state(
                    "identity-only cache lock cannot read cache entries".to_string(),
                )
            })?;
            self.validate_cache_ownership()?;
            let bytes = root.read_file(relative, max_bytes)?;
            self.validate_cache_ownership()?;
            Ok(bytes)
        }

        #[cfg(not(any(target_os = "linux", target_os = "macos")))]
        {
            let _ = (relative, max_bytes);
            Err(unsupported_secure_cache_operation())
        }
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    pub(crate) fn open_locked_staging_directory(&self) -> Result<AnchoredDirectory, Error> {
        self.validate_cache_ownership()?;
        let staging = self
            .staging
            .as_ref()
            .ok_or_else(|| {
                invalid_cache_state("cache-root capability has no staging directory".to_string())
            })?
            .duplicate()?;
        self.validate_cache_ownership()?;
        Ok(staging)
    }

    #[cfg(not(any(target_os = "linux", target_os = "macos")))]
    pub(crate) fn open_locked_staging_directory(&self) -> Result<AnchoredDirectory, Error> {
        Err(unsupported_secure_cache_operation())
    }
}

pub(crate) struct AnchoredCacheFs<'a> {
    cache_lock: &'a CacheMutationLock,
}

impl vo_common::vfs::FileSystem for AnchoredCacheFs<'_> {
    fn read_file(&self, path: &Path) -> io::Result<String> {
        let bytes = self.read_bytes_limited(path, vo_common::vfs::MAX_TEXT_FILE_BYTES)?;
        String::from_utf8(bytes)
            .map_err(|error| io::Error::new(io::ErrorKind::InvalidData, error.utf8_error()))
    }

    fn read_bytes(&self, path: &Path) -> io::Result<Vec<u8>> {
        self.read_bytes_limited(path, vo_common::vfs::MAX_PACKAGE_SOURCE_BYTES)
    }

    fn read_bytes_limited(&self, path: &Path, max_bytes: usize) -> io::Result<Vec<u8>> {
        self.cache_lock
            .read_file(path, max_bytes)
            .map_err(cache_error_to_io)
    }

    fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
        let directory = self
            .cache_lock
            .open_directory(path, "module cache directory")
            .map_err(cache_error_to_io)?;
        let entries = directory
            .entries()
            .map(|entries| entries.into_iter().map(|name| path.join(name)).collect())
            .map_err(cache_error_to_io)?;
        self.cache_lock
            .validate_cache_ownership()
            .map_err(cache_error_to_io)?;
        Ok(entries)
    }

    fn exists(&self, path: &Path) -> bool {
        !matches!(
            self.cache_lock.entry_kind(path),
            Ok(FileSystemEntryKind::Missing) | Err(_)
        )
    }

    fn is_dir(&self, path: &Path) -> bool {
        matches!(
            self.cache_lock.entry_kind(path),
            Ok(FileSystemEntryKind::Directory)
        )
    }

    fn entry_kind(&self, path: &Path) -> io::Result<FileSystemEntryKind> {
        self.cache_lock.entry_kind(path).map_err(cache_error_to_io)
    }

    fn executable_mode(&self, path: &Path) -> io::Result<Option<bool>> {
        #[cfg(any(target_os = "linux", target_os = "macos"))]
        {
            let root = self.cache_lock.cache_root.as_ref().ok_or_else(|| {
                io::Error::other("identity-only cache lock cannot inspect source mode")
            })?;
            self.cache_lock
                .validate_cache_ownership()
                .map_err(cache_error_to_io)?;
            let executable = root.file_executable(path).map_err(cache_error_to_io)?;
            self.cache_lock
                .validate_cache_ownership()
                .map_err(cache_error_to_io)?;
            Ok(Some(executable))
        }
        #[cfg(not(any(target_os = "linux", target_os = "macos")))]
        {
            let _ = path;
            Ok(None)
        }
    }
}

/// One cache transaction rooted in the same directory capability as its
/// shared mutation lock. Drop performs descriptor-relative best-effort cleanup.
#[derive(Debug)]
pub(crate) struct CacheTransaction {
    relative_path: PathBuf,
    #[cfg(any(target_os = "linux", target_os = "macos"))]
    state: CacheTransactionState,
    #[cfg(any(target_os = "linux", target_os = "macos"))]
    root: AnchoredDirectory,
    #[cfg(any(target_os = "linux", target_os = "macos"))]
    lease_file: File,
    #[cfg(any(target_os = "linux", target_os = "macos"))]
    owner_marker: File,
    #[cfg(any(target_os = "linux", target_os = "macos"))]
    staging: AnchoredDirectory,
    #[cfg(any(target_os = "linux", target_os = "macos"))]
    directory: AnchoredDirectory,
    #[cfg(any(target_os = "linux", target_os = "macos"))]
    name: OsString,
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CacheTransactionState {
    Active,
    PublishedTree,
    PublishedEntry,
    Cleaned,
}

impl CacheTransaction {
    pub(crate) fn relative_path(&self) -> &Path {
        &self.relative_path
    }

    pub(crate) fn create_dir_all(&self, relative: &Path) -> Result<(), Error> {
        #[cfg(any(target_os = "linux", target_os = "macos"))]
        {
            self.require_root_invariants()?;
            self.directory
                .ensure_relative_directory(relative, "module cache transaction directory")?;
            self.require_root_invariants()?;
            Ok(())
        }
        #[cfg(not(any(target_os = "linux", target_os = "macos")))]
        {
            let _ = relative;
            Err(unsupported_secure_cache_operation())
        }
    }

    pub(crate) fn write_file(&self, relative: &Path, bytes: &[u8]) -> Result<(), Error> {
        #[cfg(any(target_os = "linux", target_os = "macos"))]
        {
            self.require_root_invariants()?;
            self.directory
                .create_file(relative, bytes, 0o600, "module cache transaction file")?;
            self.require_root_invariants()?;
            Ok(())
        }
        #[cfg(not(any(target_os = "linux", target_os = "macos")))]
        {
            let _ = (relative, bytes);
            Err(unsupported_secure_cache_operation())
        }
    }

    pub(crate) fn write_source_file(
        &self,
        relative: &Path,
        bytes: &[u8],
        mode: crate::schema::SourceFileMode,
    ) -> Result<(), Error> {
        #[cfg(any(target_os = "linux", target_os = "macos"))]
        {
            let permissions: libc::mode_t = if mode.is_executable() { 0o700 } else { 0o600 };
            self.require_root_invariants()?;
            self.directory.create_file(
                relative,
                bytes,
                permissions,
                "module cache transaction source file",
            )?;
            self.require_root_invariants()?;
            Ok(())
        }
        #[cfg(not(any(target_os = "linux", target_os = "macos")))]
        {
            let _ = (relative, bytes, mode);
            Err(unsupported_secure_cache_operation())
        }
    }

    pub(crate) fn read_file(&self, relative: &Path, max_bytes: usize) -> Result<Vec<u8>, Error> {
        #[cfg(any(target_os = "linux", target_os = "macos"))]
        {
            self.require_root_invariants()?;
            let bytes = self.directory.read_file(relative, max_bytes)?;
            self.require_root_invariants()?;
            Ok(bytes)
        }
        #[cfg(not(any(target_os = "linux", target_os = "macos")))]
        {
            let _ = (relative, max_bytes);
            Err(unsupported_secure_cache_operation())
        }
    }

    pub(crate) fn entry_kind(&self, relative: &Path) -> Result<FileSystemEntryKind, Error> {
        #[cfg(any(target_os = "linux", target_os = "macos"))]
        {
            self.require_root_invariants()?;
            let kind = self.directory.relative_entry_kind(relative)?;
            self.require_root_invariants()?;
            Ok(kind)
        }
        #[cfg(not(any(target_os = "linux", target_os = "macos")))]
        {
            let _ = relative;
            Err(unsupported_secure_cache_operation())
        }
    }

    pub(crate) fn publish_tree(&mut self, destination: &Path) -> Result<(), Error> {
        #[cfg(any(target_os = "linux", target_os = "macos"))]
        {
            self.require_active()?;
            self.require_root_invariants()?;
            validate_relative_path(destination)?;
            if relative_starts_with(destination, STAGING_DIR) {
                return Err(invalid_cache_state(
                    "transaction destination must be outside the staging directory".to_string(),
                ));
            }
            let (parent, name) = split_relative_entry(destination, "transaction destination")?;
            let destination_parent = self
                .root
                .ensure_relative_directory(parent, "module cache destination directory")?;
            destination_parent.reject_portable_child_aliases(name, "module cache destination")?;
            self.staging.require_child_identity(
                &self.name,
                &self.directory,
                "module cache transaction",
            )?;
            self.directory.file.sync_all()?;
            self.require_root_invariants()?;
            self.staging.require_child_identity(
                &self.name,
                &self.directory,
                "module cache transaction",
            )?;
            self.root.require_relative_directory_identity(
                parent,
                &destination_parent,
                "module cache destination parent",
            )?;
            rename_between_directories(&self.staging, &self.name, &destination_parent, name)?;
            self.state = CacheTransactionState::PublishedTree;
            complete_publication(
                PublicationConfirmation {
                    root: &self.root,
                    owner_marker: &self.owner_marker,
                    staging: &self.staging,
                    lease_file: &self.lease_file,
                    source_parent: &self.staging,
                    destination_parent: &destination_parent,
                    destination_parent_path: parent,
                    destination,
                },
                || {
                    destination_parent.require_child_identity(
                        name,
                        &self.directory,
                        "published module cache transaction",
                    )
                },
            )
        }
        #[cfg(not(any(target_os = "linux", target_os = "macos")))]
        {
            let _ = destination;
            Err(unsupported_secure_cache_operation())
        }
    }

    pub(crate) fn publish_file(&mut self, source: &Path, destination: &Path) -> Result<(), Error> {
        #[cfg(any(target_os = "linux", target_os = "macos"))]
        {
            self.require_active()?;
            self.require_root_invariants()?;
            let (source_parent, source_name) = split_relative_entry(source, "transaction source")?;
            let source_parent = self
                .directory
                .open_relative_directory(source_parent, "transaction source parent")?;
            let source_file =
                source_parent.open_regular_file(source_name, "transaction source file")?;
            validate_relative_path(destination)?;
            if relative_starts_with(destination, STAGING_DIR) {
                return Err(invalid_cache_state(
                    "transaction destination must be outside the staging directory".to_string(),
                ));
            }
            let (destination_parent_path, destination_name) =
                split_relative_entry(destination, "transaction destination")?;
            let destination_parent = self.root.ensure_relative_directory(
                destination_parent_path,
                "module cache destination directory",
            )?;
            destination_parent
                .reject_portable_child_aliases(destination_name, "module cache destination")?;
            self.require_root_invariants()?;
            validate_open_file_at(
                &source_file,
                &source_parent,
                source_name,
                "transaction source file",
            )?;
            self.root.require_relative_directory_identity(
                destination_parent_path,
                &destination_parent,
                "module cache destination parent",
            )?;
            rename_between_directories(
                &source_parent,
                source_name,
                &destination_parent,
                destination_name,
            )?;
            self.state = CacheTransactionState::PublishedEntry;
            complete_publication(
                PublicationConfirmation {
                    root: &self.root,
                    owner_marker: &self.owner_marker,
                    staging: &self.staging,
                    lease_file: &self.lease_file,
                    source_parent: &source_parent,
                    destination_parent: &destination_parent,
                    destination_parent_path,
                    destination,
                },
                || {
                    validate_open_file_at(
                        &source_file,
                        &destination_parent,
                        destination_name,
                        "published module cache file",
                    )
                },
            )
        }
        #[cfg(not(any(target_os = "linux", target_os = "macos")))]
        {
            let _ = (source, destination);
            Err(unsupported_secure_cache_operation())
        }
    }

    pub(crate) fn publish_directory(
        &mut self,
        source: &Path,
        destination: &Path,
    ) -> Result<(), Error> {
        #[cfg(any(target_os = "linux", target_os = "macos"))]
        {
            self.require_active()?;
            self.require_root_invariants()?;
            let (source_parent, source_name) = split_relative_entry(source, "transaction source")?;
            let source_parent = self
                .directory
                .open_relative_directory(source_parent, "transaction source parent")?;
            let source_directory =
                source_parent.open_child(source_name, "transaction source directory")?;
            source_parent.require_child_identity(
                source_name,
                &source_directory,
                "transaction source directory",
            )?;
            source_directory.file.sync_all()?;
            validate_relative_path(destination)?;
            if relative_starts_with(destination, STAGING_DIR) {
                return Err(invalid_cache_state(
                    "transaction destination must be outside the staging directory".to_string(),
                ));
            }
            let (destination_parent_path, destination_name) =
                split_relative_entry(destination, "transaction destination")?;
            let destination_parent = self.root.ensure_relative_directory(
                destination_parent_path,
                "module cache destination directory",
            )?;
            destination_parent
                .reject_portable_child_aliases(destination_name, "module cache destination")?;
            self.require_root_invariants()?;
            source_parent.require_child_identity(
                source_name,
                &source_directory,
                "transaction source directory",
            )?;
            self.root.require_relative_directory_identity(
                destination_parent_path,
                &destination_parent,
                "module cache destination parent",
            )?;
            rename_between_directories(
                &source_parent,
                source_name,
                &destination_parent,
                destination_name,
            )?;
            self.state = CacheTransactionState::PublishedEntry;
            complete_publication(
                PublicationConfirmation {
                    root: &self.root,
                    owner_marker: &self.owner_marker,
                    staging: &self.staging,
                    lease_file: &self.lease_file,
                    source_parent: &source_parent,
                    destination_parent: &destination_parent,
                    destination_parent_path,
                    destination,
                },
                || {
                    destination_parent.require_child_identity(
                        destination_name,
                        &source_directory,
                        "published module cache directory",
                    )
                },
            )
        }
        #[cfg(not(any(target_os = "linux", target_os = "macos")))]
        {
            let _ = (source, destination);
            Err(unsupported_secure_cache_operation())
        }
    }

    pub(crate) fn cleanup(&mut self) -> Result<(), Error> {
        #[cfg(any(target_os = "linux", target_os = "macos"))]
        {
            match self.state {
                CacheTransactionState::Cleaned => return Ok(()),
                CacheTransactionState::PublishedTree => {
                    self.state = CacheTransactionState::Cleaned;
                    return Ok(());
                }
                CacheTransactionState::Active | CacheTransactionState::PublishedEntry => {}
            }
            let root_identity_error = require_cache_root_path_identity(&self.root).err();
            self.require_anchored_cache_invariants()?;
            self.staging.require_child_identity(
                &self.name,
                &self.directory,
                "module cache transaction",
            )?;
            self.directory.remove_all_entries()?;
            self.require_anchored_cache_invariants()?;
            self.staging.remove_open_directory(
                &self.name,
                &self.directory,
                "module cache transaction",
            )?;
            self.state = CacheTransactionState::Cleaned;
            match root_identity_error {
                Some(error) => Err(error),
                None => Ok(()),
            }
        }
        #[cfg(not(any(target_os = "linux", target_os = "macos")))]
        Err(unsupported_secure_cache_operation())
    }

    pub(crate) fn file_system(&self) -> AnchoredTransactionFs<'_> {
        AnchoredTransactionFs { transaction: self }
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    fn require_active(&self) -> Result<(), Error> {
        if self.state == CacheTransactionState::Active {
            return Ok(());
        }
        Err(invalid_cache_state(format!(
            "module cache transaction is no longer publishable: {:?}",
            self.state,
        )))
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    fn require_root_invariants(&self) -> Result<(), Error> {
        require_cache_root_path_identity(&self.root)?;
        self.require_anchored_cache_invariants()?;
        require_cache_root_path_identity(&self.root)
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    fn require_anchored_cache_invariants(&self) -> Result<(), Error> {
        validate_cache_owner_marker_file(&self.owner_marker, &self.root)?;
        self.root.require_child_identity(
            OsStr::new(STAGING_DIR),
            &self.staging,
            "module cache staging directory",
        )?;
        validate_open_file_at(
            &self.lease_file,
            &self.staging,
            OsStr::new(STAGING_LOCK_FILE),
            "module cache staging lock",
        )
    }
}

impl Drop for CacheTransaction {
    fn drop(&mut self) {
        let _ = self.cleanup();
    }
}

pub(crate) struct AnchoredTransactionFs<'a> {
    transaction: &'a CacheTransaction,
}

impl vo_common::vfs::FileSystem for AnchoredTransactionFs<'_> {
    fn read_file(&self, path: &Path) -> io::Result<String> {
        let bytes = self.read_bytes_limited(path, vo_common::vfs::MAX_TEXT_FILE_BYTES)?;
        String::from_utf8(bytes)
            .map_err(|error| io::Error::new(io::ErrorKind::InvalidData, error.utf8_error()))
    }

    fn read_bytes(&self, path: &Path) -> io::Result<Vec<u8>> {
        self.read_bytes_limited(path, vo_common::vfs::MAX_PACKAGE_SOURCE_BYTES)
    }

    fn read_bytes_limited(&self, path: &Path, max_bytes: usize) -> io::Result<Vec<u8>> {
        self.transaction
            .read_file(path, max_bytes)
            .map_err(cache_error_to_io)
    }

    fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
        #[cfg(any(target_os = "linux", target_os = "macos"))]
        {
            self.transaction
                .require_root_invariants()
                .map_err(cache_error_to_io)?;
            let directory = self
                .transaction
                .directory
                .open_relative_directory(path, "transaction directory")
                .map_err(cache_error_to_io)?;
            let entries = directory
                .entries()
                .map(|entries| entries.into_iter().map(|name| path.join(name)).collect())
                .map_err(cache_error_to_io)?;
            self.transaction
                .require_root_invariants()
                .map_err(cache_error_to_io)?;
            Ok(entries)
        }
        #[cfg(not(any(target_os = "linux", target_os = "macos")))]
        {
            let _ = path;
            Err(cache_error_to_io(unsupported_secure_cache_operation()))
        }
    }

    fn exists(&self, path: &Path) -> bool {
        !matches!(
            self.transaction.entry_kind(path),
            Ok(FileSystemEntryKind::Missing) | Err(_)
        )
    }

    fn is_dir(&self, path: &Path) -> bool {
        matches!(
            self.transaction.entry_kind(path),
            Ok(FileSystemEntryKind::Directory)
        )
    }

    fn entry_kind(&self, path: &Path) -> io::Result<FileSystemEntryKind> {
        self.transaction.entry_kind(path).map_err(cache_error_to_io)
    }

    fn executable_mode(&self, path: &Path) -> io::Result<Option<bool>> {
        #[cfg(any(target_os = "linux", target_os = "macos"))]
        {
            self.transaction
                .require_root_invariants()
                .map_err(cache_error_to_io)?;
            let executable = self
                .transaction
                .directory
                .file_executable(path)
                .map_err(cache_error_to_io)?;
            self.transaction
                .require_root_invariants()
                .map_err(cache_error_to_io)?;
            Ok(Some(executable))
        }
        #[cfg(not(any(target_os = "linux", target_os = "macos")))]
        {
            let _ = path;
            Ok(None)
        }
    }
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
fn next_transaction_name(identity: &str) -> OsString {
    let counter = TRANSACTION_COUNTER.fetch_add(1, Ordering::Relaxed);
    let timestamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    let digest = Digest::from_sha256(identity.as_bytes());
    let digest = &digest.as_str()["sha256:".len().."sha256:".len() + 24];
    OsString::from(format!(
        ".vo-txn-{digest}-{:08x}-{:032x}",
        std::process::id(),
        timestamp.wrapping_add(counter as u128),
    ))
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
fn validate_relative_or_empty_path(path: &Path) -> Result<(), Error> {
    if path.as_os_str().is_empty() || path == Path::new(".") {
        return Ok(());
    }
    validate_relative_path(path)
}

fn cache_error_to_io(error: Error) -> io::Error {
    match error {
        Error::Io(error) => error,
        other => io::Error::new(io::ErrorKind::InvalidData, other.to_string()),
    }
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
fn require_cache_root_path_identity(root: &AnchoredDirectory) -> Result<(), Error> {
    root.require_path_identity(&root.display_path, "module cache root")
}

fn require_secure_cache_mutation_capability(supported: bool) -> Result<(), Error> {
    if supported {
        Ok(())
    } else {
        Err(unsupported_secure_cache_operation())
    }
}

pub(crate) fn require_secure_cache_mutation_support() -> Result<(), Error> {
    require_secure_cache_mutation_capability(SECURE_CACHE_MUTATION_SUPPORTED)
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
fn normalize_cache_root_path(cache_root: &Path) -> Result<PathBuf, Error> {
    let normalized = normalize_absolute_cache_path(cache_root)?;
    if normalized == Path::new("/") {
        return Err(invalid_cache_state(
            "the filesystem root cannot be used as the module cache root".to_string(),
        ));
    }
    let current_directory = normalize_absolute_cache_path(&std::env::current_dir()?)?;
    if normalized == current_directory {
        return Err(invalid_cache_state(format!(
            "the current working directory cannot be used as the module cache root: {}",
            normalized.display(),
        )));
    }
    Ok(normalized)
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
fn normalize_absolute_cache_path(path: &Path) -> Result<PathBuf, Error> {
    let absolute = if path.is_absolute() {
        path.to_path_buf()
    } else {
        std::env::current_dir()?.join(path)
    };
    let mut normalized = PathBuf::from("/");
    for component in absolute.components() {
        match component {
            std::path::Component::RootDir | std::path::Component::CurDir => {}
            std::path::Component::Normal(component) => normalized.push(component),
            std::path::Component::ParentDir => {
                if normalized == Path::new("/") || !normalized.pop() {
                    return Err(invalid_cache_state(format!(
                        "module cache root escapes the filesystem root: {}",
                        path.display(),
                    )));
                }
            }
            std::path::Component::Prefix(_) => {
                return Err(invalid_cache_state(format!(
                    "module cache root uses an unsupported path prefix: {}",
                    path.display(),
                )));
            }
        }
    }

    // macOS exposes these stable system aliases as symbolic links into
    // /private. Normalize only the operating-system-owned aliases; arbitrary
    // user path symlinks remain visible to the componentwise no-follow walk.
    #[cfg(target_os = "macos")]
    for (alias, target) in [
        (Path::new("/var"), Path::new("/private/var")),
        (Path::new("/tmp"), Path::new("/private/tmp")),
        (Path::new("/etc"), Path::new("/private/etc")),
    ] {
        if let Ok(suffix) = normalized.strip_prefix(alias) {
            normalized = target.join(suffix);
            break;
        }
    }
    Ok(normalized)
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
fn open_cache_root(cache_root: &Path, create: bool) -> Result<AnchoredDirectory, Error> {
    let filesystem_root = AnchoredDirectory::open(Path::new("/"), "filesystem root")?;
    let relative = cache_root.strip_prefix(Path::new("/")).map_err(|_| {
        invalid_cache_state(format!(
            "normalized module cache root must be absolute: {}",
            cache_root.display(),
        ))
    })?;
    let root = if create {
        filesystem_root.ensure_relative_directory(relative, "module cache root")?
    } else {
        filesystem_root.open_relative_directory(relative, "module cache root")?
    };
    root.require_path_identity(cache_root, "module cache root")?;
    Ok(root)
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
fn open_cache_owner_marker(root: &AnchoredDirectory, create: bool) -> Result<File, Error> {
    let name = OsStr::new(CACHE_OWNER_MARKER);
    let name_c = c_string(name, "module cache owner marker name")?;
    let mut observed_incomplete_initialization = false;
    for _ in 0..OWNER_MARKER_ACQUISITION_ATTEMPTS {
        let (flags, created) = match root.entry_kind(name)? {
            FileSystemEntryKind::Missing => {
                if !create {
                    return Err(invalid_cache_state(format!(
                        "module cache root {} is missing required owner marker {CACHE_OWNER_MARKER}",
                        root.display_path.display(),
                    )));
                }
                let entries = root.entries()?;
                if entries.iter().any(|entry| entry == name) {
                    // Another first acquirer created the exact marker after
                    // `entry_kind` observed it missing. Retry so we open and
                    // lock that marker, then validate its completed content.
                    continue;
                }
                if !entries.is_empty() {
                    return Err(invalid_cache_state(format!(
                        "module cache root {} is non-empty but missing required owner marker {CACHE_OWNER_MARKER}; automatic adoption is unsupported. Move or remove this directory and retry, or select a different cache root. `vo cache clean` can clean only a cache carrying the current owner marker",
                        root.display_path.display(),
                    )));
                }
                (
                    libc::O_RDWR
                        | libc::O_CLOEXEC
                        | libc::O_CREAT
                        | libc::O_EXCL
                        | libc::O_NOFOLLOW,
                    true,
                )
            }
            FileSystemEntryKind::RegularFile => {
                (libc::O_RDWR | libc::O_CLOEXEC | libc::O_NOFOLLOW, false)
            }
            other => {
                return Err(invalid_cache_state(format!(
                    "module cache owner marker {} must be a non-linked regular file; found {other:?}",
                    root.display_path.join(name).display(),
                )));
            }
        };
        let descriptor =
            unsafe { libc::openat(root.file.as_raw_fd(), name_c.as_ptr(), flags, 0o600) };
        if descriptor < 0 {
            let error = io::Error::last_os_error();
            if error.kind() == io::ErrorKind::AlreadyExists {
                continue;
            }
            return Err(contextual_io_error(
                "module cache owner marker",
                &root.display_path.join(name),
                error,
            ));
        }
        let mut file = unsafe { File::from_raw_fd(descriptor) };
        if created {
            let initialization = (|| -> Result<(), Error> {
                #[cfg(test)]
                perform_owner_marker_creator_prelock_for_test(root);
                file.lock()?;
                #[cfg(test)]
                perform_owner_marker_publication_race_for_test(root);
                let mut entries = root.entries()?;
                let Some(marker_index) = entries.iter().position(|entry| entry == name) else {
                    return Err(invalid_cache_state(format!(
                        "module cache root {} lost owner marker {CACHE_OWNER_MARKER} while it was being created",
                        root.display_path.display(),
                    )));
                };
                entries.remove(marker_index);
                if !entries.is_empty() {
                    return Err(invalid_cache_state(format!(
                        "module cache root {} gained entries while owner marker {CACHE_OWNER_MARKER} was being created",
                        root.display_path.display(),
                    )));
                }
                file.write_all(CACHE_OWNER_MARKER_CONTENT)?;
                file.sync_all()?;
                root.file.sync_all()?;
                Ok(())
            })();
            if let Err(error) = initialization {
                return Err(rollback_created_owner_marker(root, name, &file, error));
            }
            file.unlock()?;
        }
        file.lock_shared()?;
        #[cfg(test)]
        if !created {
            perform_owner_marker_observer_lock_for_test(root);
        }
        match validate_cache_owner_marker_file(&file, root) {
            Ok(()) => return Ok(file),
            Err(error) => {
                if !create || file.metadata()?.len() != 0 {
                    return Err(error);
                }
                // An observer can acquire a shared lock in the narrow window
                // between O_EXCL publishing the empty marker and its creator
                // acquiring the initialization lock. Only retry while this
                // descriptor is still the exact, non-linked marker; every
                // other malformed-marker result remains a hard failure.
                require_cache_root_path_identity(root)?;
                validate_open_file_at(&file, root, name, "module cache owner marker")?;
                file.unlock()?;
                drop(file);
                observed_incomplete_initialization = true;
                std::thread::yield_now();
            }
        }
    }
    if observed_incomplete_initialization {
        return Err(invalid_cache_state(format!(
            "module cache owner marker {} remained incomplete after {OWNER_MARKER_ACQUISITION_ATTEMPTS} acquisition attempts",
            root.display_path.join(CACHE_OWNER_MARKER).display(),
        )));
    }
    Err(invalid_cache_state(format!(
        "module cache owner marker {} changed repeatedly during acquisition",
        root.display_path.join(CACHE_OWNER_MARKER).display(),
    )))
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
fn rollback_created_owner_marker(
    root: &AnchoredDirectory,
    name: &OsStr,
    file: &File,
    original: Error,
) -> Error {
    match root.remove_open_regular_file(name, file, "incomplete module cache owner marker") {
        Ok(()) => original,
        Err(cleanup_error) => invalid_cache_state(format!(
            "{original}; failed to remove the verified incomplete owner marker: {cleanup_error}",
        )),
    }
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
fn validate_cache_owner_marker_file(file: &File, root: &AnchoredDirectory) -> Result<(), Error> {
    use std::os::unix::fs::FileExt;

    let name = OsStr::new(CACHE_OWNER_MARKER);
    validate_open_file_at(file, root, name, "module cache owner marker")?;
    let metadata = file.metadata()?;
    if metadata.len() != CACHE_OWNER_MARKER_CONTENT.len() as u64 {
        return Err(invalid_cache_state(format!(
            "module cache owner marker {} has invalid length {}",
            root.display_path.join(name).display(),
            metadata.len(),
        )));
    }
    let mut content = vec![0; CACHE_OWNER_MARKER_CONTENT.len()];
    let mut offset = 0usize;
    while offset < content.len() {
        let read = file.read_at(&mut content[offset..], offset as u64)?;
        if read == 0 {
            return Err(invalid_cache_state(format!(
                "module cache owner marker {} ended before its declared length",
                root.display_path.join(name).display(),
            )));
        }
        offset += read;
    }
    if content != CACHE_OWNER_MARKER_CONTENT {
        return Err(invalid_cache_state(format!(
            "module cache owner marker {} has invalid content",
            root.display_path.join(name).display(),
        )));
    }
    Ok(())
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
fn open_lock_file_at(
    directory: &AnchoredDirectory,
    name: &OsStr,
    context: &str,
    create: bool,
) -> Result<File, Error> {
    validate_child_name(name)?;
    let name_c = c_string(name, "module cache lock name")?;
    for _ in 0..8 {
        let flags = match directory.entry_kind(name)? {
            FileSystemEntryKind::Missing => {
                if !create {
                    return Err(invalid_cache_state(format!(
                        "{context} {} is missing",
                        directory.display_path.join(name).display(),
                    )));
                }
                match directory.reject_portable_child_aliases(name, context) {
                    Ok(()) => {}
                    Err(Error::Io(error)) if error.kind() == io::ErrorKind::AlreadyExists => {
                        continue;
                    }
                    Err(error) => return Err(error),
                }
                libc::O_RDWR | libc::O_CLOEXEC | libc::O_CREAT | libc::O_EXCL | libc::O_NOFOLLOW
            }
            FileSystemEntryKind::RegularFile => libc::O_RDWR | libc::O_CLOEXEC | libc::O_NOFOLLOW,
            other => {
                return Err(invalid_cache_state(format!(
                    "{context} {} must be a regular file without symbolic links; found {other:?}",
                    directory.display_path.join(name).display(),
                )));
            }
        };
        let descriptor =
            unsafe { libc::openat(directory.file.as_raw_fd(), name_c.as_ptr(), flags, 0o600) };
        if descriptor < 0 {
            let error = io::Error::last_os_error();
            if error.kind() == io::ErrorKind::AlreadyExists {
                continue;
            }
            return Err(contextual_io_error(
                context,
                &directory.display_path.join(name),
                error,
            ));
        }
        let file = unsafe { File::from_raw_fd(descriptor) };
        validate_open_file_at(&file, directory, name, context)?;
        directory.file.sync_all()?;
        return Ok(file);
    }
    Err(invalid_cache_state(format!(
        "{context} {} changed repeatedly during acquisition",
        directory.display_path.join(name).display(),
    )))
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
fn validate_open_file_at(
    file: &File,
    directory: &AnchoredDirectory,
    name: &OsStr,
    context: &str,
) -> Result<(), Error> {
    use std::os::unix::fs::MetadataExt;

    let current = directory.stat_child(name)?;
    let opened = file.metadata()?;
    if entry_kind_from_mode(current.st_mode) != FileSystemEntryKind::RegularFile
        || stat_device_id(&current) != opened.dev()
        || current.st_ino != opened.ino()
    {
        return Err(invalid_cache_state(format!(
            "{context} {} changed identity while it was opened",
            directory.display_path.join(name).display(),
        )));
    }
    if current.st_nlink != 1 || opened.nlink() != 1 {
        return Err(invalid_cache_state(format!(
            "{context} {} must not be a hard link",
            directory.display_path.join(name).display(),
        )));
    }
    Ok(())
}

#[cfg(target_os = "linux")]
fn stat_device_id(metadata: &libc::stat) -> u64 {
    metadata.st_dev
}

#[cfg(target_os = "macos")]
fn stat_device_id(metadata: &libc::stat) -> u64 {
    metadata.st_dev as u64
}

pub(crate) fn ensure_real_directory(path: &Path, context: &str) -> Result<(), Error> {
    match exact_entry_kind(path)? {
        FileSystemEntryKind::Missing => {
            reject_portable_leaf_aliases(path, context)?;
            match std::fs::create_dir(path) {
                Ok(()) => {
                    require_real_directory(path, context)?;
                    sync_parent_directory(path)
                }
                Err(error) if error.kind() == io::ErrorKind::AlreadyExists => {
                    require_real_directory(path, context)
                }
                Err(error) => Err(Error::Io(error)),
            }
        }
        FileSystemEntryKind::Directory => require_exact_leaf_spelling(path, context),
        other => Err(invalid_cache_state(format!(
            "{context} {} must be a directory without symbolic links; found {other:?}",
            path.display(),
        ))),
    }
}

#[cfg(unix)]
fn sync_parent_directory(path: &Path) -> Result<(), Error> {
    let parent = path
        .parent()
        .filter(|parent| !parent.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."));
    std::fs::File::open(parent)?.sync_all()?;
    Ok(())
}

#[cfg(not(unix))]
fn sync_parent_directory(_path: &Path) -> Result<(), Error> {
    Ok(())
}

/// A directory capability used for cache publication and cleanup.
///
/// Linux and macOS descendants are opened one component at a time with
/// `openat(O_DIRECTORY | O_NOFOLLOW)`. All destructive operations are then
/// issued relative to those descriptors, so replacing a pathname cannot
/// redirect an operation through a symbolic link.
#[cfg(any(target_os = "linux", target_os = "macos"))]
#[derive(Debug)]
pub(crate) struct AnchoredDirectory {
    file: File,
    display_path: PathBuf,
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
struct CleanupDirectoryFrame {
    directory: AnchoredDirectory,
    name_in_parent: Option<OsString>,
    entries: Vec<OsString>,
    depth: usize,
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
impl AnchoredDirectory {
    pub(crate) fn open(path: &Path, context: &str) -> Result<Self, Error> {
        let file = OpenOptions::new()
            .read(true)
            .custom_flags(libc::O_CLOEXEC | libc::O_DIRECTORY | libc::O_NOFOLLOW)
            .open(path)
            .map_err(|error| contextual_io_error(context, path, error))?;
        if !file.metadata()?.is_dir() {
            return Err(invalid_cache_state(format!(
                "{context} {} must be a directory",
                path.display(),
            )));
        }
        Ok(Self {
            file,
            display_path: path.to_path_buf(),
        })
    }

    pub(crate) fn require_path_identity(&self, path: &Path, context: &str) -> Result<(), Error> {
        use std::os::unix::fs::MetadataExt;

        let opened = self.file.metadata()?;
        let current = std::fs::symlink_metadata(path)?;
        if !current.is_dir()
            || current.file_type().is_symlink()
            || opened.dev() != current.dev()
            || opened.ino() != current.ino()
        {
            return Err(invalid_cache_state(format!(
                "{context} {} changed identity after it was opened",
                path.display(),
            )));
        }
        Ok(())
    }

    pub(crate) fn entries(&self) -> Result<Vec<OsString>, Error> {
        let mut entry_count = 0usize;
        self.entries_with_budget(
            &mut entry_count,
            MAX_DIRECTORY_ENTRIES,
            "module cache directory",
            &self.display_path,
        )
    }

    fn entries_with_budget(
        &self,
        entry_count: &mut usize,
        max_entries: usize,
        limit_context: &str,
        limit_path: &Path,
    ) -> Result<Vec<OsString>, Error> {
        let dot = c".";
        let duplicate = unsafe {
            libc::openat(
                self.file.as_raw_fd(),
                dot.as_ptr(),
                libc::O_RDONLY | libc::O_CLOEXEC | libc::O_DIRECTORY | libc::O_NOFOLLOW,
            )
        };
        if duplicate < 0 {
            return Err(Error::Io(io::Error::last_os_error()));
        }
        let stream = unsafe { libc::fdopendir(duplicate) };
        if stream.is_null() {
            let error = io::Error::last_os_error();
            unsafe {
                libc::close(duplicate);
            }
            return Err(Error::Io(error));
        }
        let stream = DirectoryStream(stream);
        let mut entries = Vec::new();
        loop {
            clear_errno();
            let entry = unsafe { libc::readdir(stream.0) };
            if entry.is_null() {
                let errno = current_errno();
                if errno != 0 {
                    return Err(Error::Io(io::Error::from_raw_os_error(errno)));
                }
                break;
            }
            let name = unsafe { CStr::from_ptr((*entry).d_name.as_ptr()) }.to_bytes();
            if name == b"." || name == b".." {
                continue;
            }
            *entry_count = entry_count.checked_add(1).ok_or_else(|| {
                invalid_cache_state(format!("{limit_context} entry count overflows usize"))
            })?;
            if *entry_count > max_entries {
                return Err(invalid_cache_state(format!(
                    "{limit_context} {} contains more than {max_entries} entries",
                    limit_path.display(),
                )));
            }
            entries.push(PathBuf::from(OsString::from_vec(name.to_vec())));
        }
        vo_common::vfs::sort_fs_paths(&mut entries);
        Ok(entries
            .into_iter()
            .map(|path| path.into_os_string())
            .collect())
    }

    pub(crate) fn entry_kind(&self, name: &OsStr) -> Result<FileSystemEntryKind, Error> {
        validate_child_name(name)?;
        if !self.inspect_child_spelling(name, "module cache entry")? {
            return Ok(FileSystemEntryKind::Missing);
        }
        let name = c_string(name, "cache entry name")?;
        let mut metadata = std::mem::MaybeUninit::<libc::stat>::uninit();
        let result = unsafe {
            libc::fstatat(
                self.file.as_raw_fd(),
                name.as_ptr(),
                metadata.as_mut_ptr(),
                libc::AT_SYMLINK_NOFOLLOW,
            )
        };
        if result != 0 {
            let error = io::Error::last_os_error();
            if error.kind() == io::ErrorKind::NotFound {
                return Ok(FileSystemEntryKind::Missing);
            }
            return Err(Error::Io(error));
        }
        let metadata = unsafe { metadata.assume_init() };
        Ok(entry_kind_from_mode(metadata.st_mode))
    }

    pub(crate) fn open_child(&self, name: &OsStr, context: &str) -> Result<Self, Error> {
        validate_child_name(name)?;
        self.require_exact_child_spelling(name, context)?;
        let name_c = c_string(name, "cache directory name")?;
        let descriptor = unsafe {
            libc::openat(
                self.file.as_raw_fd(),
                name_c.as_ptr(),
                libc::O_RDONLY | libc::O_CLOEXEC | libc::O_DIRECTORY | libc::O_NOFOLLOW,
            )
        };
        if descriptor < 0 {
            return Err(contextual_io_error(
                context,
                &self.display_path.join(name),
                io::Error::last_os_error(),
            ));
        }
        let file = unsafe { File::from_raw_fd(descriptor) };
        if !file.metadata()?.is_dir() {
            return Err(invalid_cache_state(format!(
                "{context} {} must be a directory",
                self.display_path.join(name).display(),
            )));
        }
        Ok(Self {
            file,
            display_path: self.display_path.join(name),
        })
    }

    fn open_regular_file(&self, name: &OsStr, context: &str) -> Result<File, Error> {
        validate_child_name(name)?;
        self.require_exact_child_spelling(name, context)?;
        let name_c = c_string(name, "cache file name")?;
        let descriptor = unsafe {
            libc::openat(
                self.file.as_raw_fd(),
                name_c.as_ptr(),
                libc::O_RDONLY | libc::O_CLOEXEC | libc::O_NOFOLLOW,
            )
        };
        if descriptor < 0 {
            return Err(contextual_io_error(
                context,
                &self.display_path.join(name),
                io::Error::last_os_error(),
            ));
        }
        let file = unsafe { File::from_raw_fd(descriptor) };
        validate_open_file_at(&file, self, name, context)?;
        Ok(file)
    }

    fn create_child_directory(&self, name: &OsStr, context: &str) -> Result<Self, Error> {
        self.create_child_directory_with_post_create(name, context, |_| Ok(()))
    }

    fn create_child_directory_with_post_create(
        &self,
        name: &OsStr,
        context: &str,
        post_create: impl FnOnce(&Self) -> Result<(), Error>,
    ) -> Result<Self, Error> {
        validate_child_name(name)?;
        self.reject_portable_child_aliases(name, context)?;
        let name_c = c_string(name, "cache directory name")?;
        let result = unsafe { libc::mkdirat(self.file.as_raw_fd(), name_c.as_ptr(), 0o700) };
        if result != 0 {
            return Err(contextual_io_error(
                context,
                &self.display_path.join(name),
                io::Error::last_os_error(),
            ));
        }
        let opened = match self.open_child_after_creation(name, context) {
            Ok(opened) => opened,
            Err(error) => {
                return Err(invalid_cache_state(format!(
                    "{context} {} was created but could not be opened for anchored validation: {error}; the exact path was left in place",
                    self.display_path.join(name).display(),
                )));
            }
        };
        let validation = (|| -> Result<(), Error> {
            post_create(&opened)?;
            self.require_child_identity_without_alias_scan(name, &opened, context)?;
            self.require_exact_child_spelling(name, context)?;
            self.require_child_identity_without_alias_scan(name, &opened, context)?;
            self.file.sync_all()?;
            Ok(())
        })();
        if let Err(error) = validation {
            // POSIX has no portable operation that removes a directory by its
            // opened descriptor. A validate-then-unlink sequence could delete
            // a same-name replacement, so failed creations remain visible for
            // explicit inspection instead of risking destructive rollback.
            return Err(invalid_cache_state(format!(
                "{context} {} failed validation after creation: {error}; the exact path was left in place because POSIX cannot roll back this directory without a path race",
                self.display_path.join(name).display(),
            )));
        }
        Ok(opened)
    }

    /// Open the exact name after a successful `mkdirat` without first requiring
    /// portable alias uniqueness. Alias and path identity validation happen
    /// after an anchored descriptor exists; failures preserve the path.
    fn open_child_after_creation(&self, name: &OsStr, context: &str) -> Result<Self, Error> {
        validate_child_name(name)?;
        let name_c = c_string(name, "cache directory name")?;
        let descriptor = unsafe {
            libc::openat(
                self.file.as_raw_fd(),
                name_c.as_ptr(),
                libc::O_RDONLY | libc::O_CLOEXEC | libc::O_DIRECTORY | libc::O_NOFOLLOW,
            )
        };
        if descriptor < 0 {
            return Err(contextual_io_error(
                context,
                &self.display_path.join(name),
                io::Error::last_os_error(),
            ));
        }
        Ok(Self {
            file: unsafe { File::from_raw_fd(descriptor) },
            display_path: self.display_path.join(name),
        })
    }

    fn reject_portable_child_aliases(&self, expected: &OsStr, context: &str) -> Result<(), Error> {
        if self.inspect_child_spelling(expected, context)? {
            return Err(Error::Io(io::Error::new(
                io::ErrorKind::AlreadyExists,
                format!(
                    "{context} {} already exists",
                    self.display_path.join(expected).display(),
                ),
            )));
        }
        Ok(())
    }

    fn require_exact_child_spelling(&self, expected: &OsStr, context: &str) -> Result<(), Error> {
        if self.inspect_child_spelling(expected, context)? {
            return Ok(());
        }
        Err(invalid_cache_state(format!(
            "{context} {} does not exist with its exact spelling",
            self.display_path.join(expected).display(),
        )))
    }

    fn inspect_child_spelling(&self, expected: &OsStr, context: &str) -> Result<bool, Error> {
        validate_child_name(expected)?;
        let expected_text = expected.to_str();
        let expected_key = expected_text.map(crate::schema::portable_case_key);
        let mut exact = false;
        for actual in self.entries()? {
            if actual == expected {
                exact = true;
                continue;
            }
            if let (Some(expected_key), Some(actual_text)) = (&expected_key, actual.to_str()) {
                if crate::schema::portable_case_key(actual_text) == *expected_key {
                    return Err(invalid_cache_state(format!(
                        "{context} {} conflicts with portable spelling {:?}; exact spelling is required",
                        self.display_path.join(expected).display(),
                        actual_text,
                    )));
                }
            }
        }
        Ok(exact)
    }

    fn ensure_relative_directory(&self, relative: &Path, context: &str) -> Result<Self, Error> {
        validate_relative_or_empty_path(relative)?;
        let mut current = self.duplicate()?;
        for component in relative.components() {
            let std::path::Component::Normal(component) = component else {
                return Err(invalid_cache_state(format!(
                    "{context} must be a canonical relative path: {}",
                    relative.display(),
                )));
            };
            current = match current.entry_kind(component)? {
                FileSystemEntryKind::Missing => {
                    match current.create_child_directory(component, context) {
                        Ok(directory) => directory,
                        Err(Error::Io(error)) if error.kind() == io::ErrorKind::AlreadyExists => {
                            match current.entry_kind(component)? {
                                FileSystemEntryKind::Directory => {
                                    current.open_child(component, context)?
                                }
                                other => {
                                    return Err(invalid_cache_state(format!(
                                        "{context} {} changed to {other:?} during concurrent creation",
                                        current.display_path.join(component).display(),
                                    )));
                                }
                            }
                        }
                        Err(error) => return Err(error),
                    }
                }
                FileSystemEntryKind::Directory => current.open_child(component, context)?,
                other => {
                    return Err(invalid_cache_state(format!(
                        "{context} {} must be a real directory; found {other:?}",
                        current.display_path.join(component).display(),
                    )));
                }
            };
        }
        Ok(current)
    }

    fn relative_entry_kind(&self, relative: &Path) -> Result<FileSystemEntryKind, Error> {
        validate_relative_or_empty_path(relative)?;
        if relative.as_os_str().is_empty() || relative == Path::new(".") {
            return Ok(FileSystemEntryKind::Directory);
        }
        let (parent, name) = split_relative_entry(relative, "module cache entry")?;
        let mut current = self.duplicate()?;
        for component in parent.components() {
            let std::path::Component::Normal(component) = component else {
                return Err(invalid_cache_state(format!(
                    "module cache entry parent must be a canonical relative path: {}",
                    parent.display(),
                )));
            };
            match current.entry_kind(component)? {
                FileSystemEntryKind::Missing => return Ok(FileSystemEntryKind::Missing),
                FileSystemEntryKind::Directory => {
                    current = current.open_child(component, "module cache entry parent")?;
                }
                other => {
                    return Err(invalid_cache_state(format!(
                        "module cache entry parent {} must be a real directory; found {other:?}",
                        current.display_path.join(component).display(),
                    )));
                }
            }
        }
        current.entry_kind(name)
    }

    fn create_file(
        &self,
        relative: &Path,
        bytes: &[u8],
        permissions: libc::mode_t,
        context: &str,
    ) -> Result<(), Error> {
        let (parent, name) = split_relative_entry(relative, context)?;
        let parent = self.ensure_relative_directory(parent, context)?;
        parent.reject_portable_child_aliases(name, context)?;
        let name_c = c_string(name, "cache file name")?;
        let descriptor = unsafe {
            libc::openat(
                parent.file.as_raw_fd(),
                name_c.as_ptr(),
                libc::O_WRONLY | libc::O_CLOEXEC | libc::O_CREAT | libc::O_EXCL | libc::O_NOFOLLOW,
                permissions as libc::c_uint,
            )
        };
        if descriptor < 0 {
            return Err(contextual_io_error(
                context,
                &parent.display_path.join(name),
                io::Error::last_os_error(),
            ));
        }
        let mut file = unsafe { File::from_raw_fd(descriptor) };
        let chmod_result = unsafe { libc::fchmod(file.as_raw_fd(), permissions) };
        if chmod_result != 0 {
            return Err(contextual_io_error(
                context,
                &parent.display_path.join(name),
                io::Error::last_os_error(),
            ));
        }
        file.write_all(bytes)?;
        file.sync_all()?;
        parent.file.sync_all()?;
        Ok(())
    }

    pub(crate) fn read_file(&self, relative: &Path, max_bytes: usize) -> Result<Vec<u8>, Error> {
        let (parent, name) = split_relative_entry(relative, "module cache file")?;
        let parent = self.open_relative_directory(parent, "module cache file parent")?;
        parent.require_exact_child_spelling(name, "module cache file")?;
        let name_c = c_string(name, "cache file name")?;
        let descriptor = unsafe {
            libc::openat(
                parent.file.as_raw_fd(),
                name_c.as_ptr(),
                libc::O_RDONLY | libc::O_CLOEXEC | libc::O_NOFOLLOW,
            )
        };
        if descriptor < 0 {
            return Err(Error::Io(io::Error::last_os_error()));
        }
        let mut file = unsafe { File::from_raw_fd(descriptor) };
        validate_open_file_at(&file, &parent, name, "module cache file")?;
        let metadata = file.metadata()?;
        if !metadata.is_file() {
            return Err(invalid_cache_state(format!(
                "module cache file {} must be a regular file",
                parent.display_path.join(name).display(),
            )));
        }
        let max_len = u64::try_from(max_bytes).unwrap_or(u64::MAX);
        if metadata.len() > max_len {
            return Err(invalid_cache_state(format!(
                "module cache file {} exceeds the {max_bytes}-byte limit",
                parent.display_path.join(name).display(),
            )));
        }
        let mut bytes = Vec::new();
        let mut limited = (&mut file).take(max_len.saturating_add(1));
        limited.read_to_end(&mut bytes)?;
        if bytes.len() > max_bytes {
            return Err(invalid_cache_state(format!(
                "module cache file {} exceeds the {max_bytes}-byte limit",
                parent.display_path.join(name).display(),
            )));
        }
        Ok(bytes)
    }

    fn file_executable(&self, relative: &Path) -> Result<bool, Error> {
        use std::os::unix::fs::MetadataExt as _;

        let (parent, name) = split_relative_entry(relative, "module cache source file")?;
        let parent = self.open_relative_directory(parent, "module cache source parent")?;
        let file = parent.open_regular_file(name, "module cache source file")?;
        validate_open_file_at(&file, &parent, name, "module cache source file")?;
        Ok(file.metadata()?.mode() & 0o100 != 0)
    }

    pub(crate) fn remove_tree(&self, name: &OsStr, context: &str) -> Result<(), Error> {
        if self.entry_kind(name)? != FileSystemEntryKind::Directory {
            return Err(invalid_cache_state(format!(
                "{context} {} must remain a directory until cleanup",
                self.display_path.join(name).display(),
            )));
        }
        let child = self.open_child(name, context)?;
        child.remove_all_entries()?;
        self.remove_open_directory(name, &child, context)
    }

    pub(crate) fn remove_regular_file(&self, name: &OsStr, context: &str) -> Result<(), Error> {
        if self.entry_kind(name)? != FileSystemEntryKind::RegularFile {
            return Err(invalid_cache_state(format!(
                "{context} {} must remain a regular file until cleanup",
                self.display_path.join(name).display(),
            )));
        }
        self.unlink(name, 0, context)
    }

    fn remove_open_regular_file(
        &self,
        name: &OsStr,
        opened: &File,
        context: &str,
    ) -> Result<(), Error> {
        validate_open_file_at(opened, self, name, context)?;
        self.unlink(name, 0, context)
    }

    pub(crate) fn remove_empty_directory(
        &self,
        name: &OsStr,
        opened: &Self,
        context: &str,
    ) -> Result<(), Error> {
        if !opened.entries()?.is_empty() {
            return Err(invalid_cache_state(format!(
                "{context} {} is not empty",
                self.display_path.join(name).display(),
            )));
        }
        self.remove_open_directory(name, opened, context)
    }

    pub(crate) fn require_child_identity(
        &self,
        name: &OsStr,
        opened: &Self,
        context: &str,
    ) -> Result<(), Error> {
        self.require_exact_child_spelling(name, context)?;
        self.require_child_identity_without_alias_scan(name, opened, context)
    }

    fn require_child_identity_without_alias_scan(
        &self,
        name: &OsStr,
        opened: &Self,
        context: &str,
    ) -> Result<(), Error> {
        use std::os::unix::fs::MetadataExt;

        let current = self.stat_child_without_alias_scan(name)?;
        let opened_metadata = opened.file.metadata()?;
        if stat_device_id(&current) != opened_metadata.dev()
            || current.st_ino != opened_metadata.ino()
        {
            return Err(invalid_cache_state(format!(
                "{context} {} changed identity after it was opened",
                self.display_path.join(name).display(),
            )));
        }
        Ok(())
    }

    fn require_relative_directory_identity(
        &self,
        relative: &Path,
        opened: &Self,
        context: &str,
    ) -> Result<(), Error> {
        use std::os::unix::fs::MetadataExt;

        let current = self.open_relative_directory(relative, context)?;
        let current_metadata = current.file.metadata()?;
        let opened_metadata = opened.file.metadata()?;
        if current_metadata.dev() != opened_metadata.dev()
            || current_metadata.ino() != opened_metadata.ino()
        {
            return Err(invalid_cache_state(format!(
                "{context} {} changed identity after it was opened",
                self.display_path.join(relative).display(),
            )));
        }
        Ok(())
    }

    fn duplicate(&self) -> Result<Self, Error> {
        Ok(Self {
            file: self.file.try_clone()?,
            display_path: self.display_path.clone(),
        })
    }

    fn open_relative_directory(&self, relative: &Path, context: &str) -> Result<Self, Error> {
        validate_relative_or_empty_path(relative)?;
        let mut current = self.duplicate()?;
        if relative.as_os_str().is_empty() || relative == Path::new(".") {
            return Ok(current);
        }
        for component in relative.components() {
            let std::path::Component::Normal(component) = component else {
                return Err(invalid_cache_state(format!(
                    "{context} must be a canonical relative path: {}",
                    relative.display(),
                )));
            };
            current = current.open_child(component, context)?;
        }
        Ok(current)
    }

    fn remove_all_entries(&self) -> Result<(), Error> {
        self.remove_all_entries_with_limits(
            crate::schema::MAX_PORTABLE_PATH_COMPONENTS,
            MAX_DIRECTORY_ENTRIES,
        )
    }

    fn remove_all_entries_with_limits(
        &self,
        max_depth: usize,
        max_entries: usize,
    ) -> Result<(), Error> {
        let cleanup_root = self.display_path.clone();
        let mut discovered_entries = 0usize;
        let root = self.duplicate()?;
        let mut pending = vec![cleanup_directory_frame(
            root,
            None,
            0,
            &mut discovered_entries,
            max_entries,
            &cleanup_root,
        )?];

        while let Some(frame) = pending.last_mut() {
            let Some(name) = frame.entries.pop() else {
                let completed = pending.pop().ok_or_else(|| {
                    invalid_cache_state("module cache cleanup stack underflow".to_string())
                })?;
                if let Some(name) = completed.name_in_parent {
                    let parent = pending.last().ok_or_else(|| {
                        invalid_cache_state(
                            "module cache cleanup lost a directory parent".to_string(),
                        )
                    })?;
                    parent.directory.remove_open_directory(
                        &name,
                        &completed.directory,
                        "module cache cleanup directory",
                    )?;
                }
                continue;
            };

            match frame.directory.entry_kind(&name)? {
                FileSystemEntryKind::Directory => {
                    let child_depth = frame.depth.checked_add(1).ok_or_else(|| {
                        invalid_cache_state(
                            "module cache cleanup directory depth overflows usize".to_string(),
                        )
                    })?;
                    if child_depth > max_depth {
                        return Err(invalid_cache_state(format!(
                            "module cache cleanup exceeds the {max_depth}-directory depth limit at {}",
                            frame.directory.display_path.join(&name).display(),
                        )));
                    }
                    let child = frame
                        .directory
                        .open_child(&name, "module cache cleanup directory")?;
                    pending.push(cleanup_directory_frame(
                        child,
                        Some(name),
                        child_depth,
                        &mut discovered_entries,
                        max_entries,
                        &cleanup_root,
                    )?);
                }
                FileSystemEntryKind::Missing => {
                    return Err(invalid_cache_state(format!(
                        "module cache entry {} disappeared during cleanup",
                        frame.directory.display_path.join(&name).display(),
                    )));
                }
                FileSystemEntryKind::RegularFile
                | FileSystemEntryKind::Symlink
                | FileSystemEntryKind::Special => {
                    frame
                        .directory
                        .unlink(&name, 0, "module cache cleanup entry")?;
                }
                FileSystemEntryKind::Unknown => {
                    return Err(invalid_cache_state(format!(
                        "module cache entry {} has an unknown file type during cleanup",
                        frame.directory.display_path.join(&name).display(),
                    )));
                }
            }
        }
        Ok(())
    }

    fn remove_open_directory(
        &self,
        name: &OsStr,
        opened: &Self,
        context: &str,
    ) -> Result<(), Error> {
        let current = self.stat_child(name)?;
        let opened_metadata = opened.file.metadata()?;
        #[cfg(unix)]
        {
            use std::os::unix::fs::MetadataExt;
            if stat_device_id(&current) != opened_metadata.dev()
                || current.st_ino != opened_metadata.ino()
            {
                return Err(invalid_cache_state(format!(
                    "{context} {} changed identity during cleanup",
                    self.display_path.join(name).display(),
                )));
            }
        }
        self.unlink(name, libc::AT_REMOVEDIR, context)
    }

    fn stat_child(&self, name: &OsStr) -> Result<libc::stat, Error> {
        validate_child_name(name)?;
        self.require_exact_child_spelling(name, "module cache entry")?;
        self.stat_child_without_alias_scan(name)
    }

    fn stat_child_without_alias_scan(&self, name: &OsStr) -> Result<libc::stat, Error> {
        validate_child_name(name)?;
        let name = c_string(name, "cache entry name")?;
        let mut metadata = std::mem::MaybeUninit::<libc::stat>::uninit();
        let result = unsafe {
            libc::fstatat(
                self.file.as_raw_fd(),
                name.as_ptr(),
                metadata.as_mut_ptr(),
                libc::AT_SYMLINK_NOFOLLOW,
            )
        };
        if result != 0 {
            return Err(Error::Io(io::Error::last_os_error()));
        }
        Ok(unsafe { metadata.assume_init() })
    }

    fn unlink(&self, name: &OsStr, flags: i32, context: &str) -> Result<(), Error> {
        validate_child_name(name)?;
        self.require_exact_child_spelling(name, context)?;
        self.unlink_without_alias_scan(name, flags, context)
    }

    fn unlink_without_alias_scan(
        &self,
        name: &OsStr,
        flags: i32,
        context: &str,
    ) -> Result<(), Error> {
        validate_child_name(name)?;
        let name_c = c_string(name, "cache entry name")?;
        let result = unsafe { libc::unlinkat(self.file.as_raw_fd(), name_c.as_ptr(), flags) };
        if result != 0 {
            return Err(contextual_io_error(
                context,
                &self.display_path.join(name),
                io::Error::last_os_error(),
            ));
        }
        self.file.sync_all()?;
        Ok(())
    }
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
fn cleanup_directory_frame(
    directory: AnchoredDirectory,
    name_in_parent: Option<OsString>,
    depth: usize,
    discovered_entries: &mut usize,
    max_entries: usize,
    cleanup_root: &Path,
) -> Result<CleanupDirectoryFrame, Error> {
    let entries = directory.entries_with_budget(
        discovered_entries,
        max_entries,
        "module cache cleanup tree",
        cleanup_root,
    )?;
    Ok(CleanupDirectoryFrame {
        directory,
        name_in_parent,
        entries,
        depth,
    })
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
struct DirectoryStream(*mut libc::DIR);

#[cfg(any(target_os = "linux", target_os = "macos"))]
impl Drop for DirectoryStream {
    fn drop(&mut self) {
        unsafe {
            libc::closedir(self.0);
        }
    }
}

#[cfg(target_os = "linux")]
fn clear_errno() {
    unsafe {
        *libc::__errno_location() = 0;
    }
}

#[cfg(target_os = "linux")]
fn current_errno() -> i32 {
    unsafe { *libc::__errno_location() }
}

#[cfg(target_os = "macos")]
fn clear_errno() {
    unsafe {
        *libc::__error() = 0;
    }
}

#[cfg(target_os = "macos")]
fn current_errno() -> i32 {
    unsafe { *libc::__error() }
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
fn entry_kind_from_mode(mode: libc::mode_t) -> FileSystemEntryKind {
    match mode & libc::S_IFMT {
        libc::S_IFDIR => FileSystemEntryKind::Directory,
        libc::S_IFREG => FileSystemEntryKind::RegularFile,
        libc::S_IFLNK => FileSystemEntryKind::Symlink,
        _ => FileSystemEntryKind::Special,
    }
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
fn validate_child_name(name: &OsStr) -> Result<(), Error> {
    let path = Path::new(name);
    let mut components = path.components();
    if matches!(components.next(), Some(std::path::Component::Normal(_)))
        && components.next().is_none()
    {
        return Ok(());
    }
    Err(invalid_cache_state(format!(
        "module cache entry name must contain exactly one normal component: {:?}",
        name,
    )))
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
fn c_string(value: &OsStr, context: &str) -> Result<CString, Error> {
    CString::new(value.as_bytes()).map_err(|_| {
        Error::Io(io::Error::new(
            io::ErrorKind::InvalidInput,
            format!("{context} contains a zero byte"),
        ))
    })
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
fn contextual_io_error(context: &str, path: &Path, error: io::Error) -> Error {
    Error::Io(io::Error::new(
        error.kind(),
        format!("{context} {}: {error}", path.display()),
    ))
}

#[cfg(not(any(target_os = "linux", target_os = "macos")))]
#[derive(Debug)]
pub(crate) struct AnchoredDirectory;

#[cfg(not(any(target_os = "linux", target_os = "macos")))]
impl AnchoredDirectory {
    pub(crate) fn require_path_identity(&self, _path: &Path, _context: &str) -> Result<(), Error> {
        Err(unsupported_secure_cache_operation())
    }

    pub(crate) fn entries(&self) -> Result<Vec<OsString>, Error> {
        Err(unsupported_secure_cache_operation())
    }

    pub(crate) fn entry_kind(&self, _name: &OsStr) -> Result<FileSystemEntryKind, Error> {
        Err(unsupported_secure_cache_operation())
    }

    pub(crate) fn open_child(&self, _name: &OsStr, _context: &str) -> Result<Self, Error> {
        Err(unsupported_secure_cache_operation())
    }

    pub(crate) fn remove_tree(&self, _name: &OsStr, _context: &str) -> Result<(), Error> {
        Err(unsupported_secure_cache_operation())
    }

    pub(crate) fn remove_regular_file(&self, _name: &OsStr, _context: &str) -> Result<(), Error> {
        Err(unsupported_secure_cache_operation())
    }

    pub(crate) fn remove_empty_directory(
        &self,
        _name: &OsStr,
        _opened: &Self,
        _context: &str,
    ) -> Result<(), Error> {
        Err(unsupported_secure_cache_operation())
    }

    pub(crate) fn require_child_identity(
        &self,
        _name: &OsStr,
        _opened: &Self,
        _context: &str,
    ) -> Result<(), Error> {
        Err(unsupported_secure_cache_operation())
    }
}

fn unsupported_secure_cache_operation() -> Error {
    Error::Io(io::Error::new(
        io::ErrorKind::Unsupported,
        "descriptor-relative module cache mutation is unavailable on this platform",
    ))
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
enum OpenPublicationSource {
    Directory(AnchoredDirectory),
    File(File),
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
impl OpenPublicationSource {
    fn validate_at(
        &self,
        parent: &AnchoredDirectory,
        name: &OsStr,
        context: &str,
    ) -> Result<(), Error> {
        match self {
            Self::Directory(directory) => parent.require_child_identity(name, directory, context),
            Self::File(file) => validate_open_file_at(file, parent, name, context),
        }
    }
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
fn publish_from_anchor(
    root: &AnchoredDirectory,
    owner_marker: &File,
    staging: &AnchoredDirectory,
    lease_file: &File,
    from: &Path,
    to: &Path,
) -> Result<(), Error> {
    validate_cache_authority(root, owner_marker, staging, lease_file)?;
    let from_relative = publication_relative_path(from, &root.display_path).ok_or_else(|| {
        invalid_cache_state(format!(
            "publication source {} escapes cache root {}",
            from.display(),
            root.display_path.display(),
        ))
    })?;
    let to_relative = publication_relative_path(to, &root.display_path).ok_or_else(|| {
        invalid_cache_state(format!(
            "publication destination {} escapes cache root {}",
            to.display(),
            root.display_path.display(),
        ))
    })?;
    if !relative_starts_with(&from_relative, STAGING_DIR)
        || relative_starts_with(&to_relative, STAGING_DIR)
    {
        return Err(invalid_cache_state(format!(
            "module cache publication must move from {STAGING_DIR} into the installed cache",
        )));
    }
    let from_in_staging = from_relative
        .strip_prefix(Path::new(STAGING_DIR))
        .map_err(|_| {
            invalid_cache_state(format!(
                "publication source {} is outside the held staging directory",
                from.display(),
            ))
        })?;
    let (from_parent_path, from_name) =
        split_relative_entry(from_in_staging, "publication source")?;
    let (to_parent_path, to_name) = split_relative_entry(&to_relative, "publication destination")?;
    let from_parent =
        staging.open_relative_directory(from_parent_path, "module cache staging parent")?;
    let to_parent =
        root.open_relative_directory(to_parent_path, "module cache destination parent")?;
    to_parent.reject_portable_child_aliases(to_name, "module cache destination")?;
    let source = match from_parent.entry_kind(from_name)? {
        FileSystemEntryKind::Directory => OpenPublicationSource::Directory(
            from_parent.open_child(from_name, "module cache publication source directory")?,
        ),
        FileSystemEntryKind::RegularFile => OpenPublicationSource::File(
            from_parent.open_regular_file(from_name, "module cache publication source file")?,
        ),
        source_kind => {
            return Err(invalid_cache_state(format!(
                "module cache publication source {} must be a real directory or regular file; found {source_kind:?}",
                from.display(),
            )));
        }
    };
    validate_cache_authority(root, owner_marker, staging, lease_file)?;
    root.require_relative_directory_identity(
        to_parent_path,
        &to_parent,
        "module cache destination parent",
    )?;
    source.validate_at(&from_parent, from_name, "module cache publication source")?;
    rename_between_directories(&from_parent, from_name, &to_parent, to_name)?;
    complete_publication(
        PublicationConfirmation {
            root,
            owner_marker,
            staging,
            lease_file,
            source_parent: &from_parent,
            destination_parent: &to_parent,
            destination_parent_path: to_parent_path,
            destination: &to_relative,
        },
        || source.validate_at(&to_parent, to_name, "published module cache source"),
    )
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
fn publication_relative_path(path: &Path, root: &Path) -> Option<PathBuf> {
    if root == Path::new(".") && path.is_relative() {
        return Some(path.strip_prefix(".").unwrap_or(path).to_path_buf());
    }
    let normalized = normalize_absolute_cache_path(path).ok()?;
    normalized.strip_prefix(root).ok().map(Path::to_path_buf)
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
fn relative_starts_with(path: &Path, expected: &str) -> bool {
    matches!(
        path.components().next(),
        Some(std::path::Component::Normal(component)) if component == OsStr::new(expected)
    )
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
fn validate_relative_path(path: &Path) -> Result<(), Error> {
    if path.as_os_str().is_empty() {
        return Err(invalid_cache_state(format!(
            "module cache path must be non-empty, canonical, and relative: {}",
            path.display(),
        )));
    }
    let mut component_count = 0usize;
    for component in path.components() {
        if !matches!(component, std::path::Component::Normal(_)) {
            return Err(invalid_cache_state(format!(
                "module cache path must be canonical and relative: {}",
                path.display(),
            )));
        }
        component_count = component_count.checked_add(1).ok_or_else(|| {
            invalid_cache_state("module cache path component count overflows usize".to_string())
        })?;
        if component_count > crate::schema::MAX_PORTABLE_PATH_COMPONENTS {
            return Err(invalid_cache_state(format!(
                "module cache path exceeds the {}-component limit: {}",
                crate::schema::MAX_PORTABLE_PATH_COMPONENTS,
                path.display(),
            )));
        }
    }
    Ok(())
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
fn split_relative_entry<'a>(path: &'a Path, context: &str) -> Result<(&'a Path, &'a OsStr), Error> {
    validate_relative_path(path)?;
    let name = path
        .file_name()
        .ok_or_else(|| invalid_cache_state(format!("{context} has no final path component")))?;
    let parent = path.parent().unwrap_or_else(|| Path::new(""));
    Ok((parent, name))
}

#[cfg(target_os = "linux")]
fn rename_between_directories(
    from_parent: &AnchoredDirectory,
    from_name: &OsStr,
    to_parent: &AnchoredDirectory,
    to_name: &OsStr,
) -> Result<(), Error> {
    validate_child_name(from_name)?;
    validate_child_name(to_name)?;
    let from_name = c_string(from_name, "publication source name")?;
    let to_name = c_string(to_name, "publication destination name")?;
    #[cfg(test)]
    perform_source_rebind_before_publication_for_test(from_parent, from_name.as_c_str());
    let result = unsafe {
        libc::syscall(
            libc::SYS_renameat2,
            from_parent.file.as_raw_fd(),
            from_name.as_ptr(),
            to_parent.file.as_raw_fd(),
            to_name.as_ptr(),
            libc::RENAME_NOREPLACE,
        )
    };
    if result == 0 {
        Ok(())
    } else {
        Err(Error::Io(io::Error::last_os_error()))
    }
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
struct PublicationConfirmation<'a> {
    root: &'a AnchoredDirectory,
    owner_marker: &'a File,
    staging: &'a AnchoredDirectory,
    lease_file: &'a File,
    source_parent: &'a AnchoredDirectory,
    destination_parent: &'a AnchoredDirectory,
    destination_parent_path: &'a Path,
    destination: &'a Path,
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
fn complete_publication<F>(
    confirmation: PublicationConfirmation<'_>,
    mut validate_committed_identity: F,
) -> Result<(), Error>
where
    F: FnMut() -> Result<(), Error>,
{
    #[cfg(test)]
    perform_staging_lock_replacement_after_publication_for_test(
        confirmation.staging,
        confirmation.destination_parent,
        confirmation.destination,
    );
    #[cfg(test)]
    perform_root_rebind_after_publication_for_test(
        confirmation.destination_parent,
        confirmation.destination,
    );
    #[cfg(test)]
    perform_destination_parent_rebind_after_publication_for_test(
        confirmation.destination_parent,
        confirmation.destination,
    );

    let location_before_sync =
        validate_publication_location(&confirmation, &mut validate_committed_identity).err();
    let durability = sync_published_parents(
        confirmation.source_parent,
        confirmation.destination_parent,
        confirmation.destination,
    );
    let location_after_sync =
        validate_publication_location(&confirmation, &mut validate_committed_identity).err();
    let mut location_errors = [location_before_sync, location_after_sync]
        .into_iter()
        .flatten();
    if let Some(location_error) = location_errors.next() {
        let mut message = location_error.to_string();
        for additional in location_errors {
            message.push_str("; ");
            message.push_str(&additional.to_string());
        }
        if let Err(durability_error) = durability {
            message.push_str("; directory durability confirmation also failed: ");
            message.push_str(&durability_error.to_string());
        }
        return Err(Error::CachePublicationLocationUnconfirmed {
            path: confirmation.destination.display().to_string(),
            message,
        });
    }
    durability
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
fn validate_publication_location<F>(
    confirmation: &PublicationConfirmation<'_>,
    validate_committed_identity: &mut F,
) -> Result<(), Error>
where
    F: FnMut() -> Result<(), Error>,
{
    validate_cache_authority(
        confirmation.root,
        confirmation.owner_marker,
        confirmation.staging,
        confirmation.lease_file,
    )?;
    confirmation.root.require_relative_directory_identity(
        confirmation.destination_parent_path,
        confirmation.destination_parent,
        "module cache destination parent",
    )?;
    validate_committed_identity()
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
fn validate_cache_authority(
    root: &AnchoredDirectory,
    owner_marker: &File,
    staging: &AnchoredDirectory,
    lease_file: &File,
) -> Result<(), Error> {
    require_cache_root_path_identity(root)?;
    validate_cache_owner_marker_file(owner_marker, root)?;
    root.require_child_identity(
        OsStr::new(STAGING_DIR),
        staging,
        "module cache staging directory",
    )?;
    validate_open_file_at(
        lease_file,
        staging,
        OsStr::new(STAGING_LOCK_FILE),
        "module cache staging lock",
    )?;
    require_cache_root_path_identity(root)
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
fn sync_published_parents(
    source_parent: &AnchoredDirectory,
    destination_parent: &AnchoredDirectory,
    destination: &Path,
) -> Result<(), Error> {
    #[cfg(test)]
    let inject_source_failure =
        should_fail_publication_sync_for_test(destination_parent, destination);
    #[cfg(not(test))]
    let inject_source_failure = false;

    let source_result = if inject_source_failure {
        Err(io::Error::other("injected post-rename sync failure"))
    } else {
        source_parent.file.sync_all()
    };
    // The destination parent still needs its own durability attempt when the
    // source-parent flush fails. The rename has already committed, so an early
    // return here would leave the destination namespace unnecessarily exposed.
    let destination_result = destination_parent.file.sync_all();
    #[cfg(test)]
    if inject_source_failure {
        record_destination_sync_after_injected_source_failure_for_test(
            destination_parent,
            destination,
        );
    }
    match (source_result.err(), destination_result.err()) {
        (None, None) => Ok(()),
        (Some(source_error), None) => Err(publication_committed_error(
            destination,
            io::Error::new(
                source_error.kind(),
                format!(
                    "source parent {}: {source_error}",
                    source_parent.display_path.display(),
                ),
            ),
        )),
        (None, Some(destination_error)) => Err(publication_committed_error(
            destination,
            io::Error::new(
                destination_error.kind(),
                format!(
                    "destination parent {}: {destination_error}",
                    destination_parent.display_path.display(),
                ),
            ),
        )),
        (Some(source_error), Some(destination_error)) => Err(publication_committed_error(
            destination,
            io::Error::new(
                source_error.kind(),
                format!(
                    "source parent {}: {source_error}; destination parent {}: {destination_error}",
                    source_parent.display_path.display(),
                    destination_parent.display_path.display(),
                ),
            ),
        )),
    }
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
fn publication_committed_error(destination: &Path, error: io::Error) -> Error {
    Error::CachePublicationDurabilityUnconfirmed {
        path: destination.display().to_string(),
        message: error.to_string(),
    }
}

#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
static FAIL_PUBLICATION_SYNCS: OnceLock<Mutex<BTreeSet<PathBuf>>> = OnceLock::new();

#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
static DESTINATION_SYNCS_AFTER_INJECTED_SOURCE_FAILURE: OnceLock<Mutex<BTreeSet<PathBuf>>> =
    OnceLock::new();

#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
static REPLACE_STAGING_LOCKS_AFTER_PUBLICATION: OnceLock<Mutex<BTreeSet<PathBuf>>> =
    OnceLock::new();

#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
#[derive(Debug)]
struct TestRootRebind {
    root: PathBuf,
    moved_root: PathBuf,
    replacement_root: PathBuf,
}

#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
static ROOT_REBINDS_AFTER_PUBLICATION: OnceLock<Mutex<BTreeMap<PathBuf, TestRootRebind>>> =
    OnceLock::new();

#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
#[derive(Debug)]
struct TestDestinationParentRebind {
    parent: PathBuf,
    moved_parent: PathBuf,
    replacement_parent: PathBuf,
}

#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
static DESTINATION_PARENT_REBINDS_AFTER_PUBLICATION: OnceLock<
    Mutex<BTreeMap<PathBuf, TestDestinationParentRebind>>,
> = OnceLock::new();

#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
#[derive(Debug)]
struct TestSourceRebind {
    source: PathBuf,
    moved_source: PathBuf,
    replacement_source: PathBuf,
}

#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
static SOURCE_REBINDS_BEFORE_PUBLICATION: OnceLock<Mutex<BTreeMap<PathBuf, TestSourceRebind>>> =
    OnceLock::new();

#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
static OWNER_MARKER_PUBLICATION_RACES: OnceLock<Mutex<BTreeSet<PathBuf>>> = OnceLock::new();

#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
#[derive(Debug)]
struct TestOwnerMarkerInitializationRace {
    marker_published: mpsc::SyncSender<()>,
    observer_locked: mpsc::SyncSender<()>,
    wait_for_observer: Mutex<mpsc::Receiver<()>>,
}

#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
static OWNER_MARKER_INITIALIZATION_RACES: OnceLock<
    Mutex<BTreeMap<PathBuf, Arc<TestOwnerMarkerInitializationRace>>>,
> = OnceLock::new();

#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
pub(crate) fn fail_publication_sync_for_test(destination: &Path) {
    let destination = normalize_absolute_cache_path(destination)
        .expect("test publication destination must normalize");
    FAIL_PUBLICATION_SYNCS
        .get_or_init(|| Mutex::new(BTreeSet::new()))
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .insert(destination);
}

#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
fn destination_sync_followed_injected_source_failure_for_test(destination: &Path) -> bool {
    let destination = normalize_absolute_cache_path(destination)
        .expect("test publication destination must normalize");
    DESTINATION_SYNCS_AFTER_INJECTED_SOURCE_FAILURE
        .get_or_init(|| Mutex::new(BTreeSet::new()))
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .remove(&destination)
}

#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
fn replace_staging_lock_after_publication_for_test(destination: &Path) {
    let destination = normalize_absolute_cache_path(destination)
        .expect("test publication destination must normalize");
    REPLACE_STAGING_LOCKS_AFTER_PUBLICATION
        .get_or_init(|| Mutex::new(BTreeSet::new()))
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .insert(destination);
}

#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
fn rebind_root_after_publication_for_test(
    destination: &Path,
    root: &Path,
    moved_root: &Path,
    replacement_root: &Path,
) {
    let destination = normalize_absolute_cache_path(destination)
        .expect("test publication destination must normalize");
    ROOT_REBINDS_AFTER_PUBLICATION
        .get_or_init(|| Mutex::new(BTreeMap::new()))
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .insert(
            destination,
            TestRootRebind {
                root: root.to_path_buf(),
                moved_root: moved_root.to_path_buf(),
                replacement_root: replacement_root.to_path_buf(),
            },
        );
}

#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
fn rebind_destination_parent_after_publication_for_test(
    destination: &Path,
    parent: &Path,
    moved_parent: &Path,
    replacement_parent: &Path,
) {
    let destination = normalize_absolute_cache_path(destination)
        .expect("test publication destination must normalize");
    DESTINATION_PARENT_REBINDS_AFTER_PUBLICATION
        .get_or_init(|| Mutex::new(BTreeMap::new()))
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .insert(
            destination,
            TestDestinationParentRebind {
                parent: parent.to_path_buf(),
                moved_parent: moved_parent.to_path_buf(),
                replacement_parent: replacement_parent.to_path_buf(),
            },
        );
}

#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
fn rebind_source_before_publication_for_test(
    source: &Path,
    moved_source: &Path,
    replacement_source: &Path,
) {
    let source =
        normalize_absolute_cache_path(source).expect("test publication source must normalize");
    SOURCE_REBINDS_BEFORE_PUBLICATION
        .get_or_init(|| Mutex::new(BTreeMap::new()))
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .insert(
            source.clone(),
            TestSourceRebind {
                source: source.to_path_buf(),
                moved_source: moved_source.to_path_buf(),
                replacement_source: replacement_source.to_path_buf(),
            },
        );
}

#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
fn race_owner_marker_publication_for_test(root: &Path) {
    let root = normalize_absolute_cache_path(root).expect("test cache root must normalize");
    OWNER_MARKER_PUBLICATION_RACES
        .get_or_init(|| Mutex::new(BTreeSet::new()))
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .insert(root);
}

#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
fn race_owner_marker_observer_before_creator_lock_for_test(root: &Path) -> mpsc::Receiver<()> {
    let root = normalize_absolute_cache_path(root).expect("test cache root must normalize");
    let (marker_published, wait_for_marker) = mpsc::sync_channel(1);
    let (observer_locked, wait_for_observer) = mpsc::sync_channel(1);
    let race = Arc::new(TestOwnerMarkerInitializationRace {
        marker_published,
        observer_locked,
        wait_for_observer: Mutex::new(wait_for_observer),
    });
    let previous = OWNER_MARKER_INITIALIZATION_RACES
        .get_or_init(|| Mutex::new(BTreeMap::new()))
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .insert(root, Arc::clone(&race));
    assert!(previous.is_none(), "test owner-marker race must be unique");
    wait_for_marker
}

#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
fn perform_owner_marker_publication_race_for_test(root: &AnchoredDirectory) {
    let should_race = OWNER_MARKER_PUBLICATION_RACES
        .get_or_init(|| Mutex::new(BTreeSet::new()))
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .remove(&root.display_path);
    if should_race {
        root.create_file(
            Path::new("foreign-entry"),
            b"must remain",
            0o600,
            "test foreign cache-root entry",
        )
        .expect("test foreign entry must be created during marker publication");
    }
}

#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
fn perform_owner_marker_creator_prelock_for_test(root: &AnchoredDirectory) {
    let race = OWNER_MARKER_INITIALIZATION_RACES
        .get_or_init(|| Mutex::new(BTreeMap::new()))
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .get(&root.display_path)
        .cloned();
    if let Some(race) = race {
        race.marker_published
            .send(())
            .expect("test owner-marker observer must receive publication signal");
        race.wait_for_observer
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .recv_timeout(Duration::from_secs(5))
            .expect("test owner-marker observer must acquire its shared lock");
    }
}

#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
fn perform_owner_marker_observer_lock_for_test(root: &AnchoredDirectory) {
    let race = OWNER_MARKER_INITIALIZATION_RACES
        .get_or_init(|| Mutex::new(BTreeMap::new()))
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .remove(&root.display_path);
    if let Some(race) = race {
        race.observer_locked
            .send(())
            .expect("test owner-marker creator must receive observer signal");
    }
}

#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
fn perform_staging_lock_replacement_after_publication_for_test(
    staging: &AnchoredDirectory,
    destination_parent: &AnchoredDirectory,
    destination: &Path,
) {
    let Some(name) = destination.file_name() else {
        return;
    };
    let destination = destination_parent.display_path.join(name);
    let should_replace = REPLACE_STAGING_LOCKS_AFTER_PUBLICATION
        .get_or_init(|| Mutex::new(BTreeSet::new()))
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .remove(&destination);
    if !should_replace {
        return;
    }
    let lock = staging.display_path.join(STAGING_LOCK_FILE);
    std::fs::remove_file(&lock).expect("test staging lock must be removed after publication");
    std::fs::write(&lock, b"replacement lock")
        .expect("test replacement staging lock must be created after publication");
}

#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
fn perform_source_rebind_before_publication_for_test(
    source_parent: &AnchoredDirectory,
    source_name: &CStr,
) {
    let source_name = OsStr::from_bytes(source_name.to_bytes());
    let source = source_parent.display_path.join(source_name);
    let action = SOURCE_REBINDS_BEFORE_PUBLICATION
        .get_or_init(|| Mutex::new(BTreeMap::new()))
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .remove(&source);
    let Some(action) = action else {
        return;
    };
    std::fs::rename(&action.source, &action.moved_source)
        .expect("test publication source must move before publication");
    std::fs::rename(&action.replacement_source, &action.source)
        .expect("test replacement source must be installed before publication");
}

#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
fn perform_root_rebind_after_publication_for_test(
    destination_parent: &AnchoredDirectory,
    destination: &Path,
) {
    let Some(name) = destination.file_name() else {
        return;
    };
    let destination = destination_parent.display_path.join(name);
    let action = ROOT_REBINDS_AFTER_PUBLICATION
        .get_or_init(|| Mutex::new(BTreeMap::new()))
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .remove(&destination);
    let Some(action) = action else {
        return;
    };
    std::fs::rename(&action.root, &action.moved_root)
        .expect("test cache root must move after publication");
    std::fs::rename(&action.replacement_root, &action.root)
        .expect("test replacement cache root must be installed after publication");
}

#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
fn perform_destination_parent_rebind_after_publication_for_test(
    destination_parent: &AnchoredDirectory,
    destination: &Path,
) {
    let Some(name) = destination.file_name() else {
        return;
    };
    let destination = destination_parent.display_path.join(name);
    let action = DESTINATION_PARENT_REBINDS_AFTER_PUBLICATION
        .get_or_init(|| Mutex::new(BTreeMap::new()))
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .remove(&destination);
    let Some(action) = action else {
        return;
    };
    std::fs::rename(&action.parent, &action.moved_parent)
        .expect("test destination parent must move after publication");
    std::fs::rename(&action.replacement_parent, &action.parent)
        .expect("test replacement destination parent must be installed after publication");
}

#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
fn should_fail_publication_sync_for_test(
    destination_parent: &AnchoredDirectory,
    destination: &Path,
) -> bool {
    let Some(name) = destination.file_name() else {
        return false;
    };
    FAIL_PUBLICATION_SYNCS
        .get_or_init(|| Mutex::new(BTreeSet::new()))
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .remove(&destination_parent.display_path.join(name))
}

#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
fn record_destination_sync_after_injected_source_failure_for_test(
    destination_parent: &AnchoredDirectory,
    destination: &Path,
) {
    let Some(name) = destination.file_name() else {
        return;
    };
    DESTINATION_SYNCS_AFTER_INJECTED_SOURCE_FAILURE
        .get_or_init(|| Mutex::new(BTreeSet::new()))
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .insert(destination_parent.display_path.join(name));
}

#[cfg(target_os = "macos")]
fn rename_between_directories(
    from_parent: &AnchoredDirectory,
    from_name: &OsStr,
    to_parent: &AnchoredDirectory,
    to_name: &OsStr,
) -> Result<(), Error> {
    unsafe extern "C" {
        fn renameatx_np(
            from_directory: i32,
            from_name: *const std::ffi::c_char,
            to_directory: i32,
            to_name: *const std::ffi::c_char,
            flags: u32,
        ) -> i32;
    }
    const RENAME_EXCL: u32 = 0x0000_0004;
    validate_child_name(from_name)?;
    validate_child_name(to_name)?;
    let from_name = c_string(from_name, "publication source name")?;
    let to_name = c_string(to_name, "publication destination name")?;
    #[cfg(test)]
    perform_source_rebind_before_publication_for_test(from_parent, from_name.as_c_str());
    let result = unsafe {
        renameatx_np(
            from_parent.file.as_raw_fd(),
            from_name.as_ptr(),
            to_parent.file.as_raw_fd(),
            to_name.as_ptr(),
            RENAME_EXCL,
        )
    };
    if result == 0 {
        Ok(())
    } else {
        Err(Error::Io(io::Error::last_os_error()))
    }
}

fn exact_entry_kind(path: &Path) -> io::Result<FileSystemEntryKind> {
    let metadata = match std::fs::symlink_metadata(path) {
        Ok(metadata) => metadata,
        Err(error) if error.kind() == io::ErrorKind::NotFound => {
            return Ok(FileSystemEntryKind::Missing);
        }
        Err(error) => return Err(error),
    };
    let file_type = metadata.file_type();
    Ok(if file_type.is_symlink() {
        FileSystemEntryKind::Symlink
    } else if file_type.is_dir() {
        FileSystemEntryKind::Directory
    } else if file_type.is_file() {
        FileSystemEntryKind::RegularFile
    } else {
        FileSystemEntryKind::Special
    })
}

pub(crate) fn require_real_directory(path: &Path, context: &str) -> Result<(), Error> {
    match exact_entry_kind(path)? {
        FileSystemEntryKind::Directory => require_exact_leaf_spelling(path, context),
        other => Err(invalid_cache_state(format!(
            "{context} {} must be a directory without symbolic links; found {other:?}",
            path.display(),
        ))),
    }
}

pub(crate) fn reject_portable_leaf_aliases(path: &Path, context: &str) -> Result<(), Error> {
    inspect_leaf_spelling(path, context, false)
}

pub(crate) fn require_exact_leaf_spelling(path: &Path, context: &str) -> Result<(), Error> {
    inspect_leaf_spelling(path, context, true)
}

fn inspect_leaf_spelling(path: &Path, context: &str, require_exact: bool) -> Result<(), Error> {
    let Some(expected) = path.file_name() else {
        return Ok(());
    };
    let parent = path
        .parent()
        .filter(|parent| !parent.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."));
    let expected_text = expected.to_str();
    let expected_key = expected_text.map(crate::schema::portable_case_key);
    let mut exact = false;
    let mut count = 0usize;
    for entry in std::fs::read_dir(parent)? {
        count = count.checked_add(1).ok_or_else(|| {
            invalid_cache_state("module cache directory entry count overflow".to_string())
        })?;
        if count > MAX_DIRECTORY_ENTRIES {
            return Err(invalid_cache_state(format!(
                "module cache directory {} contains more than {MAX_DIRECTORY_ENTRIES} entries",
                parent.display(),
            )));
        }
        let actual = entry?.file_name();
        if actual == expected {
            exact = true;
            continue;
        }
        if let (Some(expected_key), Some(actual_text)) = (&expected_key, actual.to_str()) {
            if crate::schema::portable_case_key(actual_text) == *expected_key {
                return Err(invalid_cache_state(format!(
                    "{context} {} conflicts with portable spelling {:?}",
                    path.display(),
                    actual_text,
                )));
            }
        }
    }
    if require_exact && !exact {
        return Err(invalid_cache_state(format!(
            "{context} {} does not exist with its exact spelling",
            path.display(),
        )));
    }
    Ok(())
}

fn invalid_cache_state(detail: String) -> Error {
    Error::Io(io::Error::new(io::ErrorKind::InvalidData, detail))
}

#[cfg(test)]
pub(crate) fn staging_lock_path(cache_root: &Path) -> std::path::PathBuf {
    cache_root.join(STAGING_DIR).join(STAGING_LOCK_FILE)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[cfg(any(target_os = "linux", target_os = "macos"))]
    use crate::identity::ModulePath;

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn owner_marker_generation_matches_cache_layout_generation() {
        let expected = format!(
            "volang-module-cache-{}\n",
            crate::cache::CACHE_LAYOUT_GENERATION,
        );

        assert_eq!(CACHE_OWNER_MARKER_CONTENT, expected.as_bytes());
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn post_create_portable_alias_failure_preserves_both_directories() {
        let root = tempfile::tempdir().unwrap();
        let parent = AnchoredDirectory::open(root.path(), "test parent").unwrap();
        let requested = OsStr::new("Straße");
        let alias = root.path().join("STRASSE");

        let error = parent
            .create_child_directory_with_post_create(requested, "test directory", |_| {
                std::fs::create_dir(&alias).unwrap();
                Ok(())
            })
            .unwrap_err();

        assert!(error.to_string().contains("portable spelling"), "{error}");
        assert!(error.to_string().contains("left in place"), "{error}");
        assert!(root.path().join(requested).is_dir());
        assert!(alias.is_dir());
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn post_create_path_replacement_fails_closed_and_preserves_both_directories() {
        let root = tempfile::tempdir().unwrap();
        let parent = AnchoredDirectory::open(root.path(), "test parent").unwrap();
        let requested = OsStr::new("created");
        let requested_path = root.path().join(requested);
        let moved = root.path().join("moved-created");

        let error = parent
            .create_child_directory_with_post_create(requested, "test directory", |opened| {
                std::fs::rename(&opened.display_path, &moved).unwrap();
                std::fs::create_dir(&opened.display_path).unwrap();
                Ok(())
            })
            .unwrap_err();

        assert!(error.to_string().contains("changed identity"), "{error}");
        assert!(error.to_string().contains("left in place"), "{error}");
        assert!(requested_path.is_dir());
        assert!(moved.is_dir());
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn post_create_failure_preserves_nonempty_directory() {
        let root = tempfile::tempdir().unwrap();
        let parent = AnchoredDirectory::open(root.path(), "test parent").unwrap();
        let requested = OsStr::new("created");
        let sentinel = root.path().join(requested).join("sentinel");

        let error = parent
            .create_child_directory_with_post_create(requested, "test directory", |opened| {
                std::fs::write(opened.display_path.join("sentinel"), b"preserve").unwrap();
                Err(invalid_cache_state(
                    "injected post-create validation failure".to_string(),
                ))
            })
            .unwrap_err();

        assert!(
            error
                .to_string()
                .contains("injected post-create validation failure"),
            "{error}",
        );
        assert!(error.to_string().contains("left in place"), "{error}");
        assert_eq!(std::fs::read(sentinel).unwrap(), b"preserve");
    }

    #[test]
    fn secure_cache_mutation_capability_gate_is_stable_and_side_effect_free() {
        require_secure_cache_mutation_capability(true).unwrap();
        let error = require_secure_cache_mutation_capability(false).unwrap_err();

        assert_eq!(error_io_kind(&error), Some(io::ErrorKind::Unsupported));
        assert_eq!(
            error.to_string(),
            "io error: descriptor-relative module cache mutation is unavailable on this platform"
        );
    }

    #[cfg(not(any(target_os = "linux", target_os = "macos")))]
    #[test]
    fn unsupported_platform_rejects_before_creating_the_cache_root() {
        let parent = tempfile::tempdir().unwrap();
        let root = parent.path().join("cache/must-not-exist");

        let error = CacheMutationLock::shared(&root).unwrap_err();

        assert_eq!(error_io_kind(&error), Some(io::ErrorKind::Unsupported));
        assert!(!root.exists());
        assert!(!parent.path().join("cache").exists());
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn maximum_module_identity_uses_a_fixed_length_lock_name() {
        let root = tempfile::tempdir().unwrap();
        let prefix = "github.com/a/";
        let module = ModulePath::parse(&format!(
            "{prefix}{}",
            "r".repeat(crate::schema::MAX_PORTABLE_PATH_COMPONENT_BYTES - prefix.len()),
        ))
        .unwrap();
        let cache_lock = CacheMutationLock::shared(root.path()).unwrap();
        let _identity_lock = cache_lock
            .identity_lock(&format!("source:{module}@1.0.0"))
            .unwrap();
        let locks_dir = root.path().join(STAGING_DIR).join("locks");
        let entries = std::fs::read_dir(locks_dir)
            .unwrap()
            .map(|entry| entry.unwrap().file_name())
            .collect::<Vec<_>>();

        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].len(), 12);
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn shared_lease_safely_creates_a_missing_cache_root() {
        let parent = tempfile::tempdir().unwrap();
        let root = parent.path().join("nested/cache/root");

        let _lease = crate::cache::acquire_read_lease(&root).unwrap();

        assert_eq!(
            exact_entry_kind(&root).unwrap(),
            FileSystemEntryKind::Directory
        );
        assert_eq!(
            exact_entry_kind(&staging_lock_path(&root)).unwrap(),
            FileSystemEntryKind::RegularFile,
        );
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn dangerous_cache_roots_are_rejected_before_acquisition() {
        let filesystem_error = CacheMutationLock::shared(Path::new("/")).unwrap_err();
        let current_error = CacheMutationLock::shared(Path::new(".")).unwrap_err();

        assert!(filesystem_error.to_string().contains("filesystem root"));
        assert!(current_error
            .to_string()
            .contains("current working directory"));
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn noncache_unowned_root_is_never_blindly_adopted() {
        let root = tempfile::tempdir().unwrap();
        let sentinel = root.path().join("project-source");
        std::fs::write(&sentinel, b"preserve").unwrap();

        let error = CacheMutationLock::shared(root.path()).unwrap_err();

        assert!(
            error
                .to_string()
                .contains("non-empty but missing required owner marker"),
            "{error}"
        );
        assert!(error
            .to_string()
            .contains("automatic adoption is unsupported"));
        assert!(error.to_string().contains("`vo cache clean`"));
        assert_eq!(std::fs::read(sentinel).unwrap(), b"preserve");
        assert!(!root.path().join(CACHE_OWNER_MARKER).exists());
        assert!(!root.path().join(STAGING_DIR).exists());
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn current_layout_without_owner_marker_is_rejected_without_inspection() {
        for entry in ["github.com@acme@lib", ".cargo-target", ".tmp", "github.com"] {
            let root = tempfile::tempdir().unwrap();
            let path = root.path().join(entry);
            std::fs::create_dir(&path).unwrap();
            std::fs::write(path.join("preserve"), b"unchanged").unwrap();

            let error = CacheMutationLock::shared(root.path()).unwrap_err();

            assert!(
                error
                    .to_string()
                    .contains("non-empty but missing required owner marker"),
                "{entry}: {error}",
            );
            assert_eq!(std::fs::read(path.join("preserve")).unwrap(), b"unchanged");
            assert!(!root.path().join(CACHE_OWNER_MARKER).exists());
            assert!(!root.path().join(STAGING_DIR).exists());
        }

        let root = tempfile::tempdir().unwrap();
        let sums = root.path().join("vo.sum");
        std::fs::write(&sums, b"unchanged").unwrap();
        let error = CacheMutationLock::shared(root.path()).unwrap_err();
        assert!(
            error
                .to_string()
                .contains("non-empty but missing required owner marker"),
            "{error}",
        );
        assert_eq!(std::fs::read(sums).unwrap(), b"unchanged");
        assert!(!root.path().join(CACHE_OWNER_MARKER).exists());
        assert!(!root.path().join(STAGING_DIR).exists());
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn first_acquisition_creates_an_exact_nonlinked_owner_marker() {
        use std::os::unix::fs::MetadataExt;

        let root = tempfile::tempdir().unwrap();
        let lease = CacheMutationLock::shared(root.path()).unwrap();
        let marker = root.path().join(CACHE_OWNER_MARKER);

        assert_eq!(std::fs::read(&marker).unwrap(), CACHE_OWNER_MARKER_CONTENT);
        assert_eq!(std::fs::metadata(&marker).unwrap().nlink(), 1);
        lease.validate_cache_ownership().unwrap();
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn owner_marker_observer_retries_when_it_locks_before_the_creator() {
        let root = tempfile::tempdir().unwrap();
        let marker_published = race_owner_marker_observer_before_creator_lock_for_test(root.path());
        let (completed, completions) = mpsc::channel();
        let creator_root = root.path().to_path_buf();
        let creator_completed = completed.clone();
        let creator = std::thread::spawn(move || {
            creator_completed
                .send(CacheMutationLock::shared(&creator_root))
                .unwrap();
        });

        marker_published
            .recv_timeout(Duration::from_secs(5))
            .expect("owner marker creator must publish the marker");
        let observer_root = root.path().to_path_buf();
        let observer = std::thread::spawn(move || {
            completed
                .send(CacheMutationLock::shared(&observer_root))
                .unwrap();
        });

        let first = completions
            .recv_timeout(Duration::from_secs(5))
            .expect("first owner-marker acquisition must complete")
            .unwrap();
        let second = completions
            .recv_timeout(Duration::from_secs(5))
            .expect("second owner-marker acquisition must complete")
            .unwrap();
        creator.join().unwrap();
        observer.join().unwrap();
        first.validate_cache_ownership().unwrap();
        second.validate_cache_ownership().unwrap();
        assert_eq!(
            std::fs::read(root.path().join(CACHE_OWNER_MARKER)).unwrap(),
            CACHE_OWNER_MARKER_CONTENT,
        );
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn permanently_incomplete_owner_marker_has_a_bounded_explicit_error() {
        let root = tempfile::tempdir().unwrap();
        std::fs::write(root.path().join(CACHE_OWNER_MARKER), b"").unwrap();

        let error = CacheMutationLock::shared(root.path()).unwrap_err();

        assert!(
            error.to_string().contains(&format!(
                "remained incomplete after {OWNER_MARKER_ACQUISITION_ATTEMPTS} acquisition attempts"
            )),
            "{error}"
        );
        assert!(!root.path().join(STAGING_DIR).exists());
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn marker_creation_rolls_back_when_a_root_entry_appears_concurrently() {
        let root = tempfile::tempdir().unwrap();
        race_owner_marker_publication_for_test(root.path());

        let error = CacheMutationLock::shared(root.path()).unwrap_err();

        assert!(error.to_string().contains("gained entries"), "{error}");
        assert_eq!(
            std::fs::read(root.path().join("foreign-entry")).unwrap(),
            b"must remain",
        );
        assert!(!root.path().join(CACHE_OWNER_MARKER).exists());
        assert!(!root.path().join(STAGING_DIR).exists());
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn malformed_or_aliased_owner_markers_are_rejected_without_scaffolding() {
        use std::os::unix::fs::symlink;

        for setup in ["content", "symlink", "hardlink"] {
            let root = tempfile::tempdir().unwrap();
            let marker = root.path().join(CACHE_OWNER_MARKER);
            let external = tempfile::NamedTempFile::new().unwrap();
            std::fs::write(external.path(), CACHE_OWNER_MARKER_CONTENT).unwrap();
            match setup {
                "content" => std::fs::write(&marker, b"wrong-owner\n").unwrap(),
                "symlink" => symlink(external.path(), &marker).unwrap(),
                "hardlink" => std::fs::hard_link(external.path(), &marker).unwrap(),
                _ => unreachable!(),
            }

            let error = CacheMutationLock::shared(root.path()).unwrap_err();

            assert!(matches!(error, Error::Io(_)), "{setup}: {error}");
            assert!(!root.path().join(STAGING_DIR).exists(), "{setup}");
        }
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn exclusive_acquisition_requires_existing_cache_ownership() {
        let root = tempfile::tempdir().unwrap();

        let error = CacheMutationLock::exclusive(root.path()).unwrap_err();

        assert!(error.to_string().contains("missing required owner marker"));
        assert!(std::fs::read_dir(root.path()).unwrap().next().is_none());
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn held_lease_rejects_marker_removal_or_replacement() {
        let root = tempfile::tempdir().unwrap();
        let lease = CacheMutationLock::shared(root.path()).unwrap();
        let marker = root.path().join(CACHE_OWNER_MARKER);
        std::fs::remove_file(&marker).unwrap();
        std::fs::write(&marker, CACHE_OWNER_MARKER_CONTENT).unwrap();

        let error = lease
            .ensure_directory(Path::new("must-not-be-created"))
            .unwrap_err();

        assert!(error.to_string().contains("changed identity"), "{error}");
        assert!(!root.path().join("must-not-be-created").exists());
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn held_lease_rejects_staging_lock_removal_or_replacement() {
        let root = tempfile::tempdir().unwrap();
        let lease = CacheMutationLock::shared(root.path()).unwrap();
        let lock_path = staging_lock_path(root.path());
        std::fs::remove_file(&lock_path).unwrap();
        std::fs::write(&lock_path, b"replacement lock").unwrap();

        let error = lease
            .ensure_directory(Path::new("must-not-be-created"))
            .unwrap_err();

        assert!(error.to_string().contains("changed identity"), "{error}");
        assert!(!root.path().join("must-not-be-created").exists());
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn held_lease_and_transaction_reject_staging_directory_replacement() {
        let root = tempfile::tempdir().unwrap();
        let lease = CacheMutationLock::shared(root.path()).unwrap();
        let transaction = lease.begin_transaction("staging-replacement").unwrap();
        let moved_staging = root.path().join("moved-staging");
        std::fs::rename(root.path().join(STAGING_DIR), &moved_staging).unwrap();
        std::fs::create_dir(root.path().join(STAGING_DIR)).unwrap();
        std::fs::write(staging_lock_path(root.path()), b"replacement lock").unwrap();

        let lease_error = lease
            .ensure_directory(Path::new("must-not-be-created"))
            .unwrap_err();
        let transaction_error = transaction
            .write_file(Path::new("must-not-be-written"), b"payload")
            .unwrap_err();

        assert!(
            lease_error.to_string().contains("changed identity"),
            "{lease_error}"
        );
        assert!(
            transaction_error.to_string().contains("changed identity"),
            "{transaction_error}"
        );
        assert!(!root.path().join("must-not-be-created").exists());
        assert!(!moved_staging
            .join(
                transaction
                    .relative_path()
                    .strip_prefix(STAGING_DIR)
                    .unwrap()
            )
            .join("must-not-be-written")
            .exists());
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn shared_lease_rejects_a_symlinked_cache_root() {
        use std::os::unix::fs::symlink;

        let parent = tempfile::tempdir().unwrap();
        let outside = tempfile::tempdir().unwrap();
        let root = parent.path().join("cache");
        symlink(outside.path(), &root).unwrap();

        let error = crate::cache::acquire_read_lease(&root).unwrap_err();

        assert!(error.to_string().contains("Symlink"), "{error}");
        assert!(!outside.path().join(STAGING_DIR).exists());
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn shared_lease_rejects_an_existing_root_beneath_a_symlink_parent() {
        use std::os::unix::fs::symlink;

        let parent = tempfile::tempdir().unwrap();
        let outside = tempfile::tempdir().unwrap();
        let outside_root = outside.path().join("cache");
        std::fs::create_dir(&outside_root).unwrap();
        let linked_parent = parent.path().join("linked-parent");
        symlink(outside.path(), &linked_parent).unwrap();
        let requested_root = linked_parent.join("cache");

        let error = CacheMutationLock::shared(&requested_root).unwrap_err();

        assert!(error.to_string().contains("Symlink"), "{error}");
        assert!(!outside_root.join(STAGING_DIR).exists());
        assert!(std::fs::read_dir(&outside_root).unwrap().next().is_none());
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn shared_lease_rejects_a_portable_alias_in_the_existing_parent_chain() {
        let parent = tempfile::tempdir().unwrap();
        let actual_parent = parent.path().join("CacheParent");
        std::fs::create_dir(&actual_parent).unwrap();
        let requested_root = parent.path().join("cacheparent/cache");

        let error = CacheMutationLock::shared(&requested_root).unwrap_err();

        assert!(error.to_string().contains("portable spelling"), "{error}");
        assert!(!actual_parent.join("cache").exists());
    }

    #[cfg(target_os = "macos")]
    #[test]
    fn macos_system_cache_aliases_are_normalized_to_private_paths() {
        assert_eq!(
            normalize_cache_root_path(Path::new("/var/folders/cache")).unwrap(),
            Path::new("/private/var/folders/cache"),
        );
        assert_eq!(
            normalize_cache_root_path(Path::new("/tmp/cache")).unwrap(),
            Path::new("/private/tmp/cache"),
        );
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn identity_lock_rejects_hard_links_without_self_deadlocking() {
        let root = tempfile::tempdir().unwrap();
        let cache_lock = CacheMutationLock::shared(root.path()).unwrap();
        let identity = "source:github.com/acme/lib@1.0.0";
        let digest = Digest::from_sha256(identity.as_bytes());
        let locks_dir = root.path().join(STAGING_DIR).join("locks");
        std::fs::create_dir(&locks_dir).unwrap();
        let digest_hex = &digest.as_str()["sha256:".len()..];
        let identity_path = locks_dir.join(format!("slot-{}.lock", &digest_hex[..2]));
        std::fs::hard_link(staging_lock_path(root.path()), &identity_path).unwrap();

        let error = cache_lock.identity_lock(identity).unwrap_err();

        assert!(error.to_string().contains("hard link"), "{error}");
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn atomic_publish_never_replaces_an_existing_destination() {
        let root = tempfile::tempdir().unwrap();
        let cache_lock = CacheMutationLock::shared(root.path()).unwrap();
        let staging = root.path().join(STAGING_DIR);
        let source = staging.join("source");
        let destination = root.path().join("destination");
        std::fs::write(&source, b"new").unwrap();
        std::fs::write(&destination, b"old").unwrap();

        let error = cache_lock
            .publish_noreplace(&source, &destination)
            .unwrap_err();

        assert_eq!(error_io_kind(&error), Some(io::ErrorKind::AlreadyExists));
        assert_eq!(std::fs::read(&source).unwrap(), b"new");
        assert_eq!(std::fs::read(&destination).unwrap(), b"old");
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn descriptor_relative_publish_rejects_a_symlinked_destination_parent() {
        use std::os::unix::fs::symlink;

        let root = tempfile::tempdir().unwrap();
        let cache_lock = CacheMutationLock::shared(root.path()).unwrap();
        let staging = root.path().join(STAGING_DIR);
        let source = staging.join("source");
        std::fs::write(&source, b"new").unwrap();
        let outside = tempfile::tempdir().unwrap();
        let sentinel = outside.path().join("sentinel");
        std::fs::write(&sentinel, b"outside").unwrap();
        symlink(outside.path(), root.path().join("destination-parent")).unwrap();

        let error = cache_lock
            .publish_noreplace(&source, &root.path().join("destination-parent/published"))
            .unwrap_err();

        assert!(source.is_file());
        assert!(!outside.path().join("published").exists());
        assert_eq!(std::fs::read(sentinel).unwrap(), b"outside");
        assert!(matches!(error, Error::Io(_)));
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn descriptor_relative_publish_rejects_a_symlink_source() {
        use std::os::unix::fs::symlink;

        let root = tempfile::tempdir().unwrap();
        let cache_lock = CacheMutationLock::shared(root.path()).unwrap();
        let staging = root.path().join(STAGING_DIR);
        let outside = tempfile::NamedTempFile::new().unwrap();
        symlink(outside.path(), staging.join("source")).unwrap();
        let destination = root.path().join("destination");

        let error = cache_lock
            .publish_noreplace(&staging.join("source"), &destination)
            .unwrap_err();

        assert!(error.to_string().contains("Symlink"), "{error}");
        assert!(!destination.exists());
        assert!(staging.join("source").is_symlink());
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn publication_root_supports_a_current_directory_cache() {
        let relative =
            publication_relative_path(Path::new(".vo-staging/source"), Path::new(".")).unwrap();

        assert_eq!(relative, Path::new(".vo-staging/source"));
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn cache_paths_share_the_portable_component_depth_limit() {
        let path =
            std::iter::repeat_n("component", crate::schema::MAX_PORTABLE_PATH_COMPONENTS + 1)
                .collect::<PathBuf>();

        let error = validate_relative_path(&path).unwrap_err();

        assert!(error.to_string().contains("component limit"), "{error}");
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn anchored_cleanup_does_not_follow_a_replaced_root_path() {
        use std::os::unix::fs::symlink;

        let parent = tempfile::tempdir().unwrap();
        let root = parent.path().join("cache");
        let moved_root = parent.path().join("moved-cache");
        std::fs::create_dir_all(root.join("victim/nested")).unwrap();
        std::fs::write(root.join("victim/nested/cache-data"), b"cache").unwrap();
        let outside = tempfile::tempdir().unwrap();
        let sentinel = outside.path().join("sentinel");
        std::fs::write(&sentinel, b"outside").unwrap();
        let anchored = AnchoredDirectory::open(&root, "test cache root").unwrap();

        std::fs::rename(&root, &moved_root).unwrap();
        symlink(outside.path(), &root).unwrap();
        anchored
            .remove_tree(OsStr::new("victim"), "test cache tree")
            .unwrap();

        assert!(!moved_root.join("victim").exists());
        assert_eq!(std::fs::read(sentinel).unwrap(), b"outside");
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn iterative_cleanup_rejects_excessive_directory_depth() {
        let root = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(root.path().join("one/two/three")).unwrap();
        let anchored = AnchoredDirectory::open(root.path(), "test cleanup root").unwrap();

        let error = anchored
            .remove_all_entries_with_limits(2, MAX_DIRECTORY_ENTRIES)
            .unwrap_err();

        assert!(error.to_string().contains("depth limit"), "{error}");
        assert!(root.path().join("one/two/three").is_dir());
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn iterative_cleanup_applies_one_total_entry_budget() {
        let root = tempfile::tempdir().unwrap();
        for name in ["one", "two", "three"] {
            std::fs::write(root.path().join(name), b"preserve").unwrap();
        }
        let anchored = AnchoredDirectory::open(root.path(), "test cleanup root").unwrap();

        let error = anchored
            .remove_all_entries_with_limits(crate::schema::MAX_PORTABLE_PATH_COMPONENTS, 2)
            .unwrap_err();

        assert!(error.to_string().contains("more than 2 entries"), "{error}");
        for name in ["one", "two", "three"] {
            assert_eq!(std::fs::read(root.path().join(name)).unwrap(), b"preserve");
        }
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn transaction_drop_cleans_the_anchored_root_after_path_replacement() {
        let parent = tempfile::tempdir().unwrap();
        let root = parent.path().join("cache");
        let moved_root = parent.path().join("moved-cache");
        std::fs::create_dir(&root).unwrap();
        let cache_lock = CacheMutationLock::shared(&root).unwrap();
        let transaction = cache_lock.begin_transaction("drop-test").unwrap();
        transaction
            .write_file(Path::new("payload"), b"old-root")
            .unwrap();
        let transaction_relative = transaction.relative_path().to_path_buf();

        std::fs::rename(&root, &moved_root).unwrap();
        std::fs::create_dir_all(root.join(&transaction_relative)).unwrap();
        let replacement_sentinel = root.join(&transaction_relative).join("payload");
        std::fs::write(&replacement_sentinel, b"replacement-root").unwrap();
        drop(transaction);

        assert!(!moved_root.join(&transaction_relative).exists());
        assert_eq!(
            std::fs::read(replacement_sentinel).unwrap(),
            b"replacement-root",
        );
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn transaction_keeps_the_shared_lease_after_its_origin_guard_is_dropped() {
        let root = tempfile::tempdir().unwrap();
        let cache_lock = CacheMutationLock::shared(root.path()).unwrap();
        let transaction = cache_lock.begin_transaction("retained-lease").unwrap();
        drop(cache_lock);

        let competing = OpenOptions::new()
            .read(true)
            .write(true)
            .open(staging_lock_path(root.path()))
            .unwrap();
        let error = competing.try_lock().unwrap_err();
        assert!(matches!(error, std::fs::TryLockError::WouldBlock));

        drop(transaction);
        competing.try_lock().unwrap();
        competing.unlock().unwrap();
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn identity_lock_keeps_the_shared_lease_after_its_origin_guard_is_dropped() {
        let root = tempfile::tempdir().unwrap();
        let cache_lock = CacheMutationLock::shared(root.path()).unwrap();
        let identity_lock = cache_lock
            .identity_lock("source:github.com/acme/lib@1.0.0")
            .unwrap();
        drop(cache_lock);

        let competing = OpenOptions::new()
            .read(true)
            .write(true)
            .open(staging_lock_path(root.path()))
            .unwrap();
        let error = competing.try_lock().unwrap_err();
        assert!(matches!(error, std::fs::TryLockError::WouldBlock));

        drop(identity_lock);
        competing.try_lock().unwrap();
        competing.unlock().unwrap();
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn lock_scaffolding_creation_uses_the_opened_root_inode() {
        let parent = tempfile::tempdir().unwrap();
        let root = parent.path().join("cache");
        let moved_root = parent.path().join("moved-cache");
        std::fs::create_dir(&root).unwrap();
        let anchored = AnchoredDirectory::open(&root, "test cache root").unwrap();

        std::fs::rename(&root, &moved_root).unwrap();
        std::fs::create_dir(&root).unwrap();
        anchored
            .ensure_relative_directory(Path::new(STAGING_DIR), "test staging")
            .unwrap();

        assert!(moved_root.join(STAGING_DIR).is_dir());
        assert!(!root.join(STAGING_DIR).exists());
        let error = anchored
            .require_path_identity(&root, "test cache root")
            .unwrap_err();
        assert!(error.to_string().contains("changed identity"), "{error}");
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn transaction_publish_rejects_a_pre_rename_root_rebind_without_publishing() {
        let parent = tempfile::tempdir().unwrap();
        let root = parent.path().join("cache");
        let moved_root = parent.path().join("moved-cache");
        std::fs::create_dir(&root).unwrap();
        let cache_lock = CacheMutationLock::shared(&root).unwrap();
        let mut transaction = cache_lock.begin_transaction("publish-test").unwrap();
        transaction
            .write_file(Path::new("payload"), b"old-root")
            .unwrap();
        let transaction_relative = transaction.relative_path().to_path_buf();

        std::fs::rename(&root, &moved_root).unwrap();
        std::fs::create_dir(&root).unwrap();
        let replacement_sentinel = root.join("sentinel");
        std::fs::write(&replacement_sentinel, b"replacement").unwrap();

        let write_error = transaction
            .write_file(Path::new("second"), b"must-fail")
            .unwrap_err();
        assert!(write_error.to_string().contains("changed identity"));
        let error = transaction
            .publish_file(
                Path::new("payload"),
                Path::new("github.com@acme@lib/v1.2.3/artifact"),
            )
            .unwrap_err();

        assert!(error.to_string().contains("changed identity"), "{error}");
        assert!(!moved_root
            .join("github.com@acme@lib/v1.2.3/artifact")
            .exists());
        assert!(!root.join("github.com@acme@lib").exists());
        assert_eq!(
            std::fs::read(&replacement_sentinel).unwrap(),
            b"replacement"
        );
        assert!(moved_root.join(&transaction_relative).is_dir());
        drop(transaction);
        assert!(!moved_root.join(transaction_relative).exists());
        assert_eq!(std::fs::read(replacement_sentinel).unwrap(), b"replacement");
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn held_guard_rejects_new_operations_after_root_rebind() {
        let parent = tempfile::tempdir().unwrap();
        let root = parent.path().join("cache");
        let moved_root = parent.path().join("moved-cache");
        std::fs::create_dir(&root).unwrap();
        let cache_lock = CacheMutationLock::shared(&root).unwrap();
        std::fs::rename(&root, &moved_root).unwrap();
        std::fs::create_dir(&root).unwrap();

        for error in [
            cache_lock
                .ensure_directory(Path::new("new-directory"))
                .unwrap_err(),
            cache_lock
                .identity_lock("source:github.com/acme/lib@1.0.0")
                .unwrap_err(),
            cache_lock.begin_transaction("new-transaction").unwrap_err(),
            cache_lock.entry_kind(Path::new("entry")).unwrap_err(),
        ] {
            assert!(error.to_string().contains("changed identity"), "{error}");
        }
        assert!(std::fs::read_dir(&root).unwrap().next().is_none());
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn transaction_reports_a_post_rename_root_rebind_as_committed() {
        let parent = tempfile::tempdir().unwrap();
        let root = parent.path().join("cache");
        let moved_root = parent.path().join("moved-cache");
        let replacement_root = parent.path().join("replacement-cache");
        std::fs::create_dir(&root).unwrap();
        std::fs::create_dir(&replacement_root).unwrap();
        let replacement_sentinel = replacement_root.join("sentinel");
        std::fs::write(&replacement_sentinel, b"replacement").unwrap();
        let cache_lock = CacheMutationLock::shared(&root).unwrap();
        let mut transaction = cache_lock.begin_transaction("post-rename-rebind").unwrap();
        transaction
            .write_file(Path::new("payload"), b"old-root")
            .unwrap();
        let destination = Path::new("github.com@acme@lib/v1.2.3/artifact");
        rebind_root_after_publication_for_test(
            &root.join(destination),
            &root,
            &moved_root,
            &replacement_root,
        );

        let error = transaction
            .publish_file(Path::new("payload"), destination)
            .unwrap_err();

        assert!(matches!(
            error,
            Error::CachePublicationLocationUnconfirmed { ref path, .. }
                if path == "github.com@acme@lib/v1.2.3/artifact"
        ));
        assert_eq!(
            std::fs::read(moved_root.join(destination)).unwrap(),
            b"old-root",
        );
        assert!(!root.join(destination).exists());
        assert_eq!(
            std::fs::read(root.join("sentinel")).unwrap(),
            b"replacement"
        );
        assert!(cache_lock
            .ensure_directory(Path::new("must-not-be-created"))
            .is_err());
        drop(transaction);
        assert!(moved_root.join(destination).is_file());
        assert!(!root.join("must-not-be-created").exists());
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn transaction_reports_a_post_rename_destination_parent_rebind_as_unconfirmed() {
        let root = tempfile::tempdir().unwrap();
        let cache_lock = CacheMutationLock::shared(root.path()).unwrap();
        let mut transaction = cache_lock
            .begin_transaction("post-rename-parent-rebind")
            .unwrap();
        transaction
            .write_file(Path::new("payload"), b"detached-parent")
            .unwrap();

        let destination = Path::new("installed/artifact");
        let destination_parent = root.path().join("installed");
        let moved_parent = root.path().join("moved-installed");
        let replacement_parent = root.path().join("replacement-installed");
        std::fs::create_dir(&replacement_parent).unwrap();
        std::fs::write(replacement_parent.join("sentinel"), b"replacement").unwrap();
        rebind_destination_parent_after_publication_for_test(
            &root.path().join(destination),
            &destination_parent,
            &moved_parent,
            &replacement_parent,
        );

        let error = transaction
            .publish_file(Path::new("payload"), destination)
            .unwrap_err();

        assert!(matches!(
            error,
            Error::CachePublicationLocationUnconfirmed { ref path, .. }
                if path == "installed/artifact"
        ));
        assert_eq!(
            std::fs::read(moved_parent.join("artifact")).unwrap(),
            b"detached-parent",
        );
        assert!(!root.path().join(destination).exists());
        assert_eq!(
            std::fs::read(destination_parent.join("sentinel")).unwrap(),
            b"replacement",
        );
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn transaction_reports_a_post_rename_staging_lock_replacement_as_unconfirmed() {
        let root = tempfile::tempdir().unwrap();
        let cache_lock = CacheMutationLock::shared(root.path()).unwrap();
        let mut transaction = cache_lock
            .begin_transaction("post-rename-lock-replacement")
            .unwrap();
        transaction
            .write_file(Path::new("payload"), b"committed")
            .unwrap();
        let destination = Path::new("installed/artifact");
        replace_staging_lock_after_publication_for_test(&root.path().join(destination));

        let error = transaction
            .publish_file(Path::new("payload"), destination)
            .unwrap_err();

        assert!(matches!(
            error,
            Error::CachePublicationLocationUnconfirmed { ref path, .. }
                if path == "installed/artifact"
        ));
        assert_eq!(
            std::fs::read(root.path().join(destination)).unwrap(),
            b"committed",
        );
        assert!(cache_lock
            .ensure_directory(Path::new("must-not-be-created"))
            .is_err());
        assert!(!root.path().join("must-not-be-created").exists());
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn publication_reports_a_valid_source_replacement_as_committed_but_unconfirmed() {
        let root = tempfile::tempdir().unwrap();
        let cache_lock = CacheMutationLock::shared(root.path()).unwrap();
        let mut transaction = cache_lock.begin_transaction("source-rebind").unwrap();
        transaction
            .write_file(Path::new("payload"), b"original")
            .unwrap();
        let source = root
            .path()
            .join(transaction.relative_path())
            .join("payload");
        let moved_source = root.path().join("moved-original-source");
        let replacement_source = root.path().join("replacement-source");
        std::fs::write(&replacement_source, b"replacement").unwrap();
        rebind_source_before_publication_for_test(&source, &moved_source, &replacement_source);
        let destination = Path::new("github.com@acme@lib/v1.2.3/artifact");

        let error = transaction
            .publish_file(Path::new("payload"), destination)
            .unwrap_err();

        assert!(matches!(
            error,
            Error::CachePublicationLocationUnconfirmed { ref path, .. }
                if path == "github.com@acme@lib/v1.2.3/artifact"
        ));
        assert_eq!(
            std::fs::read(root.path().join(destination)).unwrap(),
            b"replacement"
        );
        assert_eq!(std::fs::read(moved_source).unwrap(), b"original");
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn descriptor_relative_parent_walk_rejects_portable_case_aliases() {
        let root = tempfile::tempdir().unwrap();
        let cache_lock = CacheMutationLock::shared(root.path()).unwrap();
        cache_lock.ensure_directory(Path::new("Parent")).unwrap();

        let error = cache_lock
            .ensure_directory(Path::new("parent/child"))
            .unwrap_err();

        assert!(error.to_string().contains("portable spelling"), "{error}");
        assert!(!root.path().join("Parent/child").exists());
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn transaction_reads_require_exact_leaf_spelling() {
        let root = tempfile::tempdir().unwrap();
        let cache_lock = CacheMutationLock::shared(root.path()).unwrap();
        let transaction = cache_lock.begin_transaction("read-case").unwrap();
        transaction
            .write_file(Path::new("Payload"), b"content")
            .unwrap();

        let error = transaction.read_file(Path::new("payload"), 7).unwrap_err();

        assert!(error.to_string().contains("portable spelling"), "{error}");
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn concurrent_installers_can_create_the_same_real_parent_chain() {
        use std::sync::{Arc, Barrier};

        let root = tempfile::tempdir().unwrap().keep();
        let barrier = Arc::new(Barrier::new(8));
        let threads = (0..8)
            .map(|_| {
                let root = root.clone();
                let barrier = Arc::clone(&barrier);
                std::thread::spawn(move || {
                    barrier.wait();
                    let cache_lock = CacheMutationLock::shared(&root).unwrap();
                    cache_lock.ensure_directory(Path::new("github.com@acme@lib"))
                })
            })
            .collect::<Vec<_>>();

        for thread in threads {
            thread.join().unwrap().unwrap();
        }
        assert!(root.join("github.com@acme@lib").is_dir());
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn post_rename_sync_failure_keeps_the_published_tree_for_validation() {
        let root = tempfile::tempdir().unwrap();
        let cache_lock = CacheMutationLock::shared(root.path()).unwrap();
        let mut transaction = cache_lock.begin_transaction("sync-failure").unwrap();
        transaction
            .write_file(Path::new("payload"), b"published")
            .unwrap();
        let destination = Path::new("github.com@acme@lib/v1.2.3");
        fail_publication_sync_for_test(&root.path().join(destination));

        let error = transaction.publish_tree(destination).unwrap_err();

        assert!(matches!(
            error,
            Error::CachePublicationDurabilityUnconfirmed { ref path, .. }
                if path == "github.com@acme@lib/v1.2.3"
        ));
        assert!(destination_sync_followed_injected_source_failure_for_test(
            &root.path().join(destination),
        ));
        assert_eq!(
            std::fs::read(root.path().join("github.com@acme@lib/v1.2.3/payload")).unwrap(),
            b"published",
        );
        assert_eq!(
            cache_lock
                .entry_kind(Path::new("github.com@acme@lib/v1.2.3"))
                .unwrap(),
            FileSystemEntryKind::Directory,
        );
        transaction.cleanup().unwrap();
        assert!(root.path().join("github.com@acme@lib/v1.2.3").is_dir());
    }

    fn error_io_kind(error: &Error) -> Option<io::ErrorKind> {
        match error {
            Error::Io(error) => Some(error.kind()),
            _ => None,
        }
    }
}
