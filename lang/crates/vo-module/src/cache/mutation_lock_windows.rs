use std::ffi::{OsStr, OsString};
use std::fs::File;
use std::io::{self, Read, Seek, SeekFrom, Write};
use std::os::windows::ffi::{OsStrExt, OsStringExt};
use std::os::windows::io::{AsRawHandle, FromRawHandle};
use std::path::{Component, Path, PathBuf, Prefix};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{SystemTime, UNIX_EPOCH};

use vo_common::vfs::{FileSystemEntryKind, MAX_DIRECTORY_ENTRIES};
use windows_sys::Wdk::Foundation::OBJECT_ATTRIBUTES;
use windows_sys::Wdk::Storage::FileSystem::{
    FileDispositionInformationEx, FileRenameInformationEx, NtCreateFile, NtFlushBuffersFile,
    NtSetInformationFile, FILE_CREATE, FILE_DIRECTORY_FILE, FILE_DISPOSITION_DELETE,
    FILE_DISPOSITION_IGNORE_READONLY_ATTRIBUTE, FILE_DISPOSITION_INFORMATION_EX,
    FILE_DISPOSITION_POSIX_SEMANTICS, FILE_NON_DIRECTORY_FILE, FILE_OPEN,
    FILE_OPEN_FOR_BACKUP_INTENT, FILE_OPEN_REPARSE_POINT, FILE_RENAME_INFORMATION,
    FILE_SYNCHRONOUS_IO_NONALERT, FILE_WRITE_THROUGH,
};
use windows_sys::Win32::Foundation::{
    CloseHandle, RtlNtStatusToDosError, HANDLE, INVALID_HANDLE_VALUE, OBJ_DONT_REPARSE,
    UNICODE_STRING,
};
use windows_sys::Win32::Storage::FileSystem::{
    FileIdExtdDirectoryInfo, FileIdExtdDirectoryRestartInfo, GetFileInformationByHandleEx,
    LockFileEx, UnlockFileEx, DELETE, FILE_ADD_FILE, FILE_ADD_SUBDIRECTORY,
    FILE_ATTRIBUTE_DIRECTORY, FILE_ATTRIBUTE_NORMAL, FILE_ATTRIBUTE_REPARSE_POINT,
    FILE_ID_EXTD_DIR_INFO, FILE_LIST_DIRECTORY, FILE_READ_ATTRIBUTES, FILE_READ_DATA,
    FILE_SHARE_DELETE, FILE_SHARE_READ, FILE_SHARE_WRITE, FILE_TRAVERSE, FILE_WRITE_ATTRIBUTES,
    FILE_WRITE_DATA, LOCKFILE_EXCLUSIVE_LOCK, SYNCHRONIZE,
};
use windows_sys::Win32::System::IO::{IO_STATUS_BLOCK, OVERLAPPED};

use crate::cache::layout::{STAGING_DIR, STAGING_LOCK_FILE};
use crate::digest::Digest;
use crate::windows_file::WindowsFileInformation;
use crate::Error;

pub(crate) const CACHE_OWNER_MARKER: &str = ".vo-cache-owner";

const CACHE_OWNER_MARKER_CONTENT: &[u8] = b"volang-module-cache-v2\n";
const OWNER_MARKER_ACQUISITION_ATTEMPTS: usize = 64;
const TRANSACTION_ACQUISITION_ATTEMPTS: usize = 64;
const LOCK_POOL_SLOTS: usize = 256;
const DIRECTORY_QUERY_BUFFER_BYTES: usize = 64 * 1024;
const DIRECTORY_READ_ACCESS: u32 =
    FILE_LIST_DIRECTORY | FILE_TRAVERSE | FILE_READ_ATTRIBUTES | SYNCHRONIZE;
const DIRECTORY_MUTATION_ACCESS: u32 =
    DIRECTORY_READ_ACCESS | FILE_ADD_FILE | FILE_ADD_SUBDIRECTORY;
const FILE_FLAG_BACKUP_SEMANTICS: u32 = 0x0200_0000;
const FILE_FLAG_OPEN_REPARSE_POINT: u32 = 0x0020_0000;

static TRANSACTION_COUNTER: AtomicU64 = AtomicU64::new(0);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LeaseMode {
    Shared,
    Exclusive,
}

/// One process-owned Windows byte-range lease. `LockFileEx` releases the lock
/// when this exact handle closes after process failure.
///
/// Byte 0 is an admission gate and byte 1 is the lifetime state lock. A shared
/// lease briefly takes shared(G), then shared(S), then releases G. An exclusive
/// lease retains exclusive(G)+exclusive(S). Immutable content-addressed cache
/// entries only publish at a missing destination, so no lock upgrade exists.
#[derive(Debug)]
struct LockedFile {
    file: File,
    mode: LeaseMode,
}

impl LockedFile {
    fn acquire(file: File, mode: LeaseMode) -> Result<Self, Error> {
        acquire_lease(&file, mode)?;
        Ok(Self { file, mode })
    }

    fn file(&self) -> &File {
        &self.file
    }

    /// Convert an exclusive lease into a shared lease without opening an
    /// admission gap. The exclusive admission byte prevents new participants
    /// from reaching the state byte while its lock mode changes.
    fn downgrade(mut self) -> Result<Self, Error> {
        if self.mode == LeaseMode::Shared {
            return Ok(self);
        }
        unlock_range(&self.file, STATE_BYTE)?;
        if let Err(error) = lock_range(&self.file, STATE_BYTE, LeaseMode::Shared) {
            let _ = unlock_range(&self.file, ADMISSION_BYTE);
            return Err(Error::Io(error));
        }
        self.mode = LeaseMode::Shared;
        unlock_range(&self.file, ADMISSION_BYTE)?;
        Ok(self)
    }
}

impl Drop for LockedFile {
    fn drop(&mut self) {
        match self.mode {
            LeaseMode::Shared => {
                let _ = unlock_range(&self.file, STATE_BYTE);
            }
            LeaseMode::Exclusive => {
                let _ = unlock_range(&self.file, STATE_BYTE);
                let _ = unlock_range(&self.file, ADMISSION_BYTE);
            }
        }
    }
}

/// An operating-system lock shared by installers and held exclusively by
/// cleanup. Every cache authority is rooted in open, non-reparse handles.
#[derive(Debug)]
pub(crate) struct CacheMutationLock {
    lease: Arc<LockedFile>,
    /// Identity guards retain the originating cache lease until their own
    /// exclusive pool lock has been released.
    _retained_cache_lease: Option<Arc<LockedFile>>,
    cache_root: Option<AnchoredDirectory>,
    owner_marker: Option<Arc<LockedFile>>,
    staging: Option<AnchoredDirectory>,
}

impl CacheMutationLock {
    pub(crate) fn shared(cache_root: &Path) -> Result<Self, Error> {
        Self::acquire(cache_root, LeaseMode::Shared, true)
    }

    /// Open an initialized cache without creating or recovering any state.
    pub(crate) fn shared_existing(cache_root: &Path) -> Result<Self, Error> {
        Self::acquire(cache_root, LeaseMode::Shared, false)
    }

    pub(crate) fn exclusive(cache_root: &Path) -> Result<Self, Error> {
        Self::acquire(cache_root, LeaseMode::Exclusive, false)
    }

    fn acquire(cache_root: &Path, mode: LeaseMode, create: bool) -> Result<Self, Error> {
        let normalized = normalize_cache_root_path(cache_root)?;
        let root = open_cache_root(&normalized, create)?;
        let owner_marker = open_cache_owner_marker(&root, create)?;
        let staging = if create {
            root.ensure_relative_directory(
                Path::new(STAGING_DIR),
                "module cache staging directory",
            )?
        } else {
            root.open_child(OsStr::new(STAGING_DIR), "module cache staging directory")?
        };
        let lease_file = open_lock_file_at(
            &staging,
            OsStr::new(STAGING_LOCK_FILE),
            "module cache staging lock",
            create,
        )?;
        let lease = Arc::new(LockedFile::acquire(lease_file, mode)?);
        let acquired = Self {
            lease,
            _retained_cache_lease: None,
            cache_root: Some(root),
            owner_marker: Some(owner_marker),
            staging: Some(staging),
        };
        acquired.validate_cache_ownership()?;
        acquired.open_locked_staging_directory()?;
        acquired.validate_cache_ownership()?;
        Ok(acquired)
    }

    /// Serialize one publication identity through a fixed 256-slot lock pool.
    pub(crate) fn identity_lock(&self, identity: &str) -> Result<Self, Error> {
        self.cache_root.as_ref().ok_or_else(|| {
            invalid_cache_state(
                "identity-only cache lock cannot create another identity lock".to_string(),
            )
        })?;
        self.validate_cache_ownership()?;
        let staging = self.open_locked_staging_directory()?;
        let locks =
            staging.ensure_relative_directory(Path::new("locks"), "module cache identity locks")?;
        let digest = Digest::from_sha256(identity.as_bytes());
        let digest_hex = &digest.as_str()["sha256:".len()..];
        let slot = usize::from_str_radix(&digest_hex[..2], 16)
            .expect("SHA-256 hexadecimal prefix is valid")
            % LOCK_POOL_SLOTS;
        let name = OsString::from(format!("slot-{slot:02x}.lock"));
        let file = open_lock_file_at(&locks, &name, "module cache identity lock", true)?;
        let identity_lease = Arc::new(LockedFile::acquire(file, LeaseMode::Exclusive)?);
        staging.require_child_identity(
            OsStr::new("locks"),
            &locks,
            "module cache identity-lock directory",
        )?;
        validate_open_file_at(
            identity_lease.file(),
            &locks,
            &name,
            "module cache identity lock",
        )?;
        self.validate_cache_ownership()?;
        Ok(Self {
            lease: identity_lease,
            _retained_cache_lease: Some(Arc::clone(&self.lease)),
            cache_root: None,
            owner_marker: None,
            staging: None,
        })
    }

    pub(crate) fn cache_root_directory(&self) -> Result<AnchoredDirectory, Error> {
        let root = self.root_capability("inspect the cache root")?;
        self.validate_cache_ownership()?;
        root.duplicate()
    }

    pub(crate) fn validate_cache_ownership(&self) -> Result<(), Error> {
        let root = self.root_capability("validate cache ownership")?;
        let marker = self.owner_marker.as_ref().ok_or_else(|| {
            invalid_cache_state("cache-root capability has no owner marker".to_string())
        })?;
        let staging = self.staging.as_ref().ok_or_else(|| {
            invalid_cache_state("cache-root capability has no staging directory".to_string())
        })?;
        validate_cache_authority(root, marker.file(), staging, self.lease.file())
    }

    pub(crate) fn publish_noreplace(&self, from: &Path, to: &Path) -> Result<(), Error> {
        let root = self.root_capability("authorize publication")?;
        let marker = self.owner_marker.as_ref().ok_or_else(|| {
            invalid_cache_state("cache publication capability has no owner marker".to_string())
        })?;
        let staging = self.staging.as_ref().ok_or_else(|| {
            invalid_cache_state("cache publication capability has no staging directory".to_string())
        })?;
        publish_from_anchor(root, marker.file(), staging, self.lease.file(), from, to)
    }

    pub(crate) fn begin_transaction(&self, identity: &str) -> Result<CacheTransaction, Error> {
        let root = self.root_capability("create a transaction")?;
        self.validate_cache_ownership()?;
        let staging = self.open_locked_staging_directory()?;
        let transaction_root = root.duplicate()?;
        let transaction_owner_marker = Arc::clone(self.owner_marker.as_ref().ok_or_else(|| {
            invalid_cache_state("cache-root transaction capability has no owner marker".to_string())
        })?);
        for _ in 0..TRANSACTION_ACQUISITION_ATTEMPTS {
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
                        lease: Arc::clone(&self.lease),
                        owner_marker: transaction_owner_marker,
                        staging,
                        directory,
                        name,
                    });
                }
                Err(Error::Io(error)) if error.kind() == io::ErrorKind::AlreadyExists => {}
                Err(error) => return Err(error),
            }
        }
        Err(invalid_cache_state(
            "failed to allocate a unique module cache transaction directory".to_string(),
        ))
    }

    pub(crate) fn ensure_directory(&self, relative: &Path) -> Result<(), Error> {
        let root = self.root_capability("create cache directories")?;
        self.validate_cache_ownership()?;
        root.ensure_relative_directory(relative, "module cache directory")?;
        self.validate_cache_ownership()
    }

    pub(crate) fn entry_kind(&self, relative: &Path) -> Result<FileSystemEntryKind, Error> {
        let root = self.root_capability("inspect cache entries")?;
        self.validate_cache_ownership()?;
        let kind = root.relative_entry_kind(relative)?;
        self.validate_cache_ownership()?;
        Ok(kind)
    }

    pub(crate) fn open_directory(
        &self,
        relative: &Path,
        context: &str,
    ) -> Result<AnchoredDirectory, Error> {
        let root = self.root_capability("inspect cache directories")?;
        self.validate_cache_ownership()?;
        let directory = root.open_relative_directory(relative, context)?;
        self.validate_cache_ownership()?;
        Ok(directory)
    }

    pub(crate) fn file_system(&self) -> AnchoredCacheFs<'_> {
        AnchoredCacheFs { cache_lock: self }
    }

    fn read_file(&self, relative: &Path, max_bytes: usize) -> Result<Vec<u8>, Error> {
        let root = self.root_capability("read cache entries")?;
        self.validate_cache_ownership()?;
        let bytes = root.read_file(relative, max_bytes)?;
        self.validate_cache_ownership()?;
        Ok(bytes)
    }

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

    fn root_capability(&self, operation: &str) -> Result<&AnchoredDirectory, Error> {
        self.cache_root.as_ref().ok_or_else(|| {
            invalid_cache_state(format!("identity-only cache lock cannot {operation}"))
        })
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

    fn executable_mode(&self, _path: &Path) -> io::Result<Option<bool>> {
        // NTFS has no portable POSIX execute bit. The authenticated package
        // manifest remains the mode authority on Windows.
        Ok(None)
    }
}

/// One cache transaction rooted in the same handle authority as its shared
/// mutation lock. Drop performs best-effort handle-relative cleanup.
#[derive(Debug)]
pub(crate) struct CacheTransaction {
    relative_path: PathBuf,
    state: CacheTransactionState,
    root: AnchoredDirectory,
    lease: Arc<LockedFile>,
    owner_marker: Arc<LockedFile>,
    staging: AnchoredDirectory,
    directory: AnchoredDirectory,
    name: OsString,
}

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
        self.require_root_invariants()?;
        self.directory
            .ensure_relative_directory(relative, "module cache transaction directory")?;
        self.require_root_invariants()
    }

    pub(crate) fn write_file(&self, relative: &Path, bytes: &[u8]) -> Result<(), Error> {
        self.require_root_invariants()?;
        self.directory
            .create_file(relative, bytes, "module cache transaction file")?;
        self.require_root_invariants()
    }

    pub(crate) fn write_source_file(
        &self,
        relative: &Path,
        bytes: &[u8],
        _mode: crate::schema::SourceFileMode,
    ) -> Result<(), Error> {
        self.require_root_invariants()?;
        self.directory
            .create_file(relative, bytes, "module cache transaction source file")?;
        self.require_root_invariants()
    }

    pub(crate) fn read_file(&self, relative: &Path, max_bytes: usize) -> Result<Vec<u8>, Error> {
        self.require_root_invariants()?;
        let bytes = self.directory.read_file(relative, max_bytes)?;
        self.require_root_invariants()?;
        Ok(bytes)
    }

    pub(crate) fn entry_kind(&self, relative: &Path) -> Result<FileSystemEntryKind, Error> {
        self.require_root_invariants()?;
        let kind = self.directory.relative_entry_kind(relative)?;
        self.require_root_invariants()?;
        Ok(kind)
    }

    pub(crate) fn publish_tree(&mut self, destination: &Path) -> Result<(), Error> {
        self.require_active()?;
        self.require_root_invariants()?;
        validate_publication_destination(destination)?;
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
        self.directory.sync()?;
        let source =
            self.staging
                .open_movable_entry(&self.name, true, "module cache transaction")?;
        source.require_identity(self.directory.identity(), "module cache transaction")?;
        self.require_root_invariants()?;
        self.root.require_relative_directory_identity(
            parent,
            &destination_parent,
            "module cache destination parent",
        )?;
        rename_entry(&source.file, &destination_parent, name)?;
        self.state = CacheTransactionState::PublishedTree;
        complete_publication(
            PublicationConfirmation {
                root: &self.root,
                owner_marker: self.owner_marker.file(),
                staging: &self.staging,
                lease_file: self.lease.file(),
                source_parent: &self.staging,
                destination_parent: &destination_parent,
                destination_parent_path: parent,
                destination,
            },
            || {
                destination_parent.require_child_file_identity(
                    name,
                    source.identity,
                    "published module cache transaction",
                )
            },
        )
    }

    pub(crate) fn publish_file(&mut self, source: &Path, destination: &Path) -> Result<(), Error> {
        self.publish_entry(source, destination, false)
    }

    pub(crate) fn publish_directory(
        &mut self,
        source: &Path,
        destination: &Path,
    ) -> Result<(), Error> {
        self.publish_entry(source, destination, true)
    }

    fn publish_entry(
        &mut self,
        source: &Path,
        destination: &Path,
        directory: bool,
    ) -> Result<(), Error> {
        self.require_active()?;
        self.require_root_invariants()?;
        let (source_parent_path, source_name) = split_relative_entry(source, "transaction source")?;
        let source_parent = self
            .directory
            .open_relative_directory(source_parent_path, "transaction source parent")?;
        let source = source_parent.open_movable_entry(
            source_name,
            directory,
            "module cache transaction source",
        )?;
        if directory {
            source.sync()?;
        }
        validate_publication_destination(destination)?;
        let (destination_parent_path, destination_name) =
            split_relative_entry(destination, "transaction destination")?;
        let destination_parent = self.root.ensure_relative_directory(
            destination_parent_path,
            "module cache destination directory",
        )?;
        destination_parent
            .reject_portable_child_aliases(destination_name, "module cache destination")?;
        self.require_root_invariants()?;
        source_parent.require_child_file_identity(
            source_name,
            source.identity,
            "transaction source",
        )?;
        self.root.require_relative_directory_identity(
            destination_parent_path,
            &destination_parent,
            "module cache destination parent",
        )?;
        rename_entry(&source.file, &destination_parent, destination_name)?;
        self.state = CacheTransactionState::PublishedEntry;
        complete_publication(
            PublicationConfirmation {
                root: &self.root,
                owner_marker: self.owner_marker.file(),
                staging: &self.staging,
                lease_file: self.lease.file(),
                source_parent: &source_parent,
                destination_parent: &destination_parent,
                destination_parent_path,
                destination,
            },
            || {
                destination_parent.require_child_file_identity(
                    destination_name,
                    source.identity,
                    "published module cache entry",
                )
            },
        )
    }

    pub(crate) fn cleanup(&mut self) -> Result<(), Error> {
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

    pub(crate) fn file_system(&self) -> AnchoredTransactionFs<'_> {
        AnchoredTransactionFs { transaction: self }
    }

    fn require_active(&self) -> Result<(), Error> {
        if self.state == CacheTransactionState::Active {
            return Ok(());
        }
        Err(invalid_cache_state(format!(
            "module cache transaction is no longer publishable: {:?}",
            self.state,
        )))
    }

    fn require_root_invariants(&self) -> Result<(), Error> {
        require_cache_root_path_identity(&self.root)?;
        self.require_anchored_cache_invariants()?;
        require_cache_root_path_identity(&self.root)
    }

    fn require_anchored_cache_invariants(&self) -> Result<(), Error> {
        validate_cache_owner_marker_file(self.owner_marker.file(), &self.root)?;
        self.root.require_child_identity(
            OsStr::new(STAGING_DIR),
            &self.staging,
            "module cache staging directory",
        )?;
        validate_open_file_at(
            self.lease.file(),
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

    fn executable_mode(&self, _path: &Path) -> io::Result<Option<bool>> {
        Ok(None)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct FileIdentity {
    volume: u64,
    file: [u8; 16],
}

impl FileIdentity {
    fn from_information(information: &WindowsFileInformation) -> Self {
        Self {
            volume: information.volume,
            file: information.file,
        }
    }
}

#[derive(Debug)]
struct MovableEntry {
    file: File,
    identity: FileIdentity,
}

impl MovableEntry {
    fn require_identity(&self, expected: FileIdentity, context: &str) -> Result<(), Error> {
        if self.identity == expected {
            Ok(())
        } else {
            Err(invalid_cache_state(format!(
                "{context} changed identity while it was opened"
            )))
        }
    }

    fn sync(&self) -> Result<(), Error> {
        nt_flush(&self.file).map_err(Error::Io)
    }
}

#[derive(Debug)]
struct DirectoryEntry {
    name: OsString,
    identity: FileIdentity,
    attributes: u32,
}

impl DirectoryEntry {
    fn kind(&self) -> FileSystemEntryKind {
        if self.attributes & FILE_ATTRIBUTE_REPARSE_POINT != 0 {
            FileSystemEntryKind::Symlink
        } else if self.attributes & FILE_ATTRIBUTE_DIRECTORY != 0 {
            FileSystemEntryKind::Directory
        } else {
            FileSystemEntryKind::RegularFile
        }
    }
}

/// A Windows directory capability. Descendants are opened with `NtCreateFile`
/// relative to this handle and `OBJ_DONT_REPARSE`; rename and deletion operate
/// on the opened child handle, so path replacement cannot redirect mutation.
#[derive(Debug)]
pub(crate) struct AnchoredDirectory {
    file: File,
    display_path: PathBuf,
    identity: FileIdentity,
}

#[derive(Debug)]
struct CleanupDirectoryFrame {
    directory: AnchoredDirectory,
    name_in_parent: Option<OsString>,
    entries: Vec<OsString>,
    depth: usize,
}

impl AnchoredDirectory {
    fn from_file(file: File, display_path: PathBuf, context: &str) -> Result<Self, Error> {
        let information = validate_directory_information(&file, &display_path, context)?;
        Ok(Self {
            identity: FileIdentity::from_information(&information),
            file,
            display_path,
        })
    }

    fn identity(&self) -> FileIdentity {
        self.identity
    }

    pub(crate) fn require_path_identity(&self, path: &Path, context: &str) -> Result<(), Error> {
        let current =
            open_absolute_directory(&normalize_absolute_cache_path(path)?, false, false, context)?;
        if current.identity != self.identity {
            return Err(invalid_cache_state(format!(
                "{context} {} changed identity after it was opened",
                path.display(),
            )));
        }
        Ok(())
    }

    pub(crate) fn entries(&self) -> Result<Vec<OsString>, Error> {
        let mut count = 0usize;
        self.entries_with_budget(
            &mut count,
            MAX_DIRECTORY_ENTRIES,
            "module cache directory",
            &self.display_path,
        )
        .map(|entries| entries.into_iter().map(|entry| entry.name).collect())
    }

    fn entry_records(&self) -> Result<Vec<DirectoryEntry>, Error> {
        let mut count = 0usize;
        self.entries_with_budget(
            &mut count,
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
    ) -> Result<Vec<DirectoryEntry>, Error> {
        let mut entries = query_directory_entries(&self.file)?;
        entries.retain(|entry| entry.name != OsStr::new(".") && entry.name != OsStr::new(".."));
        *entry_count = entry_count.checked_add(entries.len()).ok_or_else(|| {
            invalid_cache_state(format!("{limit_context} entry count overflows usize"))
        })?;
        if *entry_count > max_entries {
            return Err(invalid_cache_state(format!(
                "{limit_context} {} contains more than {max_entries} entries",
                limit_path.display(),
            )));
        }
        entries.sort_by(|left, right| left.name.encode_wide().cmp(right.name.encode_wide()));
        Ok(entries)
    }

    pub(crate) fn entry_kind(&self, name: &OsStr) -> Result<FileSystemEntryKind, Error> {
        validate_child_name(name)?;
        Ok(self
            .find_exact_entry(name, "module cache entry")?
            .map_or(FileSystemEntryKind::Missing, |entry| entry.kind()))
    }

    pub(crate) fn open_child(&self, name: &OsStr, context: &str) -> Result<Self, Error> {
        let expected = self
            .find_exact_entry(name, context)?
            .ok_or_else(|| missing_exact_entry(&self.display_path.join(name), context))?;
        if expected.kind() != FileSystemEntryKind::Directory {
            return Err(invalid_cache_state(format!(
                "{context} {} must be a non-reparse directory; found {:?}",
                self.display_path.join(name).display(),
                expected.kind(),
            )));
        }
        let file = nt_open_relative(
            &self.file,
            name,
            DIRECTORY_MUTATION_ACCESS,
            FILE_OPEN,
            FILE_DIRECTORY_FILE | FILE_OPEN_REPARSE_POINT | FILE_SYNCHRONOUS_IO_NONALERT,
            FILE_ATTRIBUTE_NORMAL,
        )?;
        let opened = Self::from_file(file, self.display_path.join(name), context)?;
        if opened.identity != expected.identity {
            return Err(invalid_cache_state(format!(
                "{context} {} changed identity while it was opened",
                self.display_path.join(name).display(),
            )));
        }
        Ok(opened)
    }

    fn open_regular_file(&self, name: &OsStr, context: &str) -> Result<File, Error> {
        let entry = self
            .find_exact_entry(name, context)?
            .ok_or_else(|| missing_exact_entry(&self.display_path.join(name), context))?;
        if entry.kind() != FileSystemEntryKind::RegularFile {
            return Err(invalid_cache_state(format!(
                "{context} {} must be a non-reparse regular file; found {:?}",
                self.display_path.join(name).display(),
                entry.kind(),
            )));
        }
        let file = nt_open_relative(
            &self.file,
            name,
            FILE_READ_DATA | FILE_READ_ATTRIBUTES | SYNCHRONIZE,
            FILE_OPEN,
            FILE_NON_DIRECTORY_FILE | FILE_OPEN_REPARSE_POINT | FILE_SYNCHRONOUS_IO_NONALERT,
            FILE_ATTRIBUTE_NORMAL,
        )?;
        validate_open_regular_file(
            &file,
            &self.display_path.join(name),
            context,
            Some(entry.identity),
        )?;
        Ok(file)
    }

    fn open_movable_entry(
        &self,
        name: &OsStr,
        directory: bool,
        context: &str,
    ) -> Result<MovableEntry, Error> {
        let entry = self
            .find_exact_entry(name, context)?
            .ok_or_else(|| missing_exact_entry(&self.display_path.join(name), context))?;
        let expected_kind = if directory {
            FileSystemEntryKind::Directory
        } else {
            FileSystemEntryKind::RegularFile
        };
        if entry.kind() != expected_kind {
            return Err(invalid_cache_state(format!(
                "{context} {} must be a non-reparse {expected_kind:?}; found {:?}",
                self.display_path.join(name).display(),
                entry.kind(),
            )));
        }
        let desired = DELETE
            | FILE_READ_ATTRIBUTES
            | SYNCHRONIZE
            | if directory {
                FILE_LIST_DIRECTORY | FILE_TRAVERSE | FILE_ADD_FILE | FILE_ADD_SUBDIRECTORY
            } else {
                FILE_READ_DATA
            };
        let options = FILE_OPEN_REPARSE_POINT
            | FILE_SYNCHRONOUS_IO_NONALERT
            | if directory {
                FILE_DIRECTORY_FILE
            } else {
                FILE_NON_DIRECTORY_FILE
            };
        let file = nt_open_relative(
            &self.file,
            name,
            desired,
            FILE_OPEN,
            options,
            FILE_ATTRIBUTE_NORMAL,
        )?;
        let information = if directory {
            validate_directory_information(&file, &self.display_path.join(name), context)?
        } else {
            validate_open_regular_file(
                &file,
                &self.display_path.join(name),
                context,
                Some(entry.identity),
            )?
        };
        let identity = FileIdentity::from_information(&information);
        if identity != entry.identity {
            return Err(invalid_cache_state(format!(
                "{context} {} changed identity while it was opened",
                self.display_path.join(name).display(),
            )));
        }
        Ok(MovableEntry { file, identity })
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
        let file = nt_open_relative(
            &self.file,
            name,
            DIRECTORY_MUTATION_ACCESS | DELETE,
            FILE_CREATE,
            FILE_DIRECTORY_FILE
                | FILE_OPEN_REPARSE_POINT
                | FILE_SYNCHRONOUS_IO_NONALERT
                | FILE_WRITE_THROUGH,
            FILE_ATTRIBUTE_NORMAL,
        )?;
        let opened = Self::from_file(file, self.display_path.join(name), context)?;
        let validation = (|| -> Result<(), Error> {
            post_create(&opened)?;
            self.require_child_identity_without_alias_scan(name, &opened, context)?;
            self.require_child_identity(name, &opened, context)?;
            self.require_child_identity_without_alias_scan(name, &opened, context)?;
            self.sync()?;
            Ok(())
        })();
        if let Err(error) = validation {
            return Err(self.rollback_created_child_directory(name, &opened, context, error));
        }
        Ok(opened)
    }

    fn rollback_created_child_directory(
        &self,
        name: &OsStr,
        opened: &Self,
        context: &str,
        original: Error,
    ) -> Error {
        match self.remove_created_empty_directory(name, opened, context) {
            Ok(()) => original,
            Err(rollback_error) => invalid_cache_state(format!(
                "{context} {} failed validation after creation: {original}; handle-bound rollback refused or failed: {rollback_error}",
                self.display_path.join(name).display(),
            )),
        }
    }

    fn remove_created_empty_directory(
        &self,
        name: &OsStr,
        opened: &Self,
        context: &str,
    ) -> Result<(), Error> {
        self.require_child_identity_without_alias_scan(name, opened, context)?;
        if !opened.entries()?.is_empty() {
            return Err(invalid_cache_state(format!(
                "{context} {} gained content after creation and was preserved",
                opened.display_path.display(),
            )));
        }
        self.require_child_identity_without_alias_scan(name, opened, context)?;
        delete_open_entry(&opened.file).map_err(Error::Io)?;
        self.sync()
    }

    fn reject_portable_child_aliases(&self, expected: &OsStr, context: &str) -> Result<(), Error> {
        if self.inspect_child_spelling(expected, context)?.is_some() {
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

    fn inspect_child_spelling(
        &self,
        expected: &OsStr,
        context: &str,
    ) -> Result<Option<DirectoryEntry>, Error> {
        validate_child_name(expected)?;
        let expected_text = expected.to_str().ok_or_else(|| {
            invalid_cache_state(format!(
                "{context} {} is not valid Unicode",
                self.display_path.join(expected).display(),
            ))
        })?;
        let expected_key = crate::schema::portable_case_key(expected_text);
        let mut exact = None;
        for actual in self.entry_records()? {
            if actual.name == expected {
                exact = Some(actual);
                continue;
            }
            let Some(actual_text) = actual.name.to_str() else {
                return Err(invalid_cache_state(format!(
                    "{context} directory {} contains a non-Unicode entry",
                    self.display_path.display(),
                )));
            };
            if crate::schema::portable_case_key(actual_text) == expected_key {
                return Err(invalid_cache_state(format!(
                    "{context} {} conflicts with portable spelling {:?}; exact spelling is required",
                    self.display_path.join(expected).display(),
                    actual_text,
                )));
            }
        }
        Ok(exact)
    }

    fn find_exact_entry(
        &self,
        expected: &OsStr,
        context: &str,
    ) -> Result<Option<DirectoryEntry>, Error> {
        self.inspect_child_spelling(expected, context)
    }

    fn ensure_relative_directory(&self, relative: &Path, context: &str) -> Result<Self, Error> {
        validate_relative_or_empty_path(relative)?;
        let mut current = self.duplicate()?;
        for component in relative.components() {
            let Component::Normal(component) = component else {
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
            let Component::Normal(component) = component else {
                return Err(invalid_cache_state(format!(
                    "module cache entry parent must be canonical: {}",
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

    fn create_file(&self, relative: &Path, bytes: &[u8], context: &str) -> Result<(), Error> {
        let (parent, name) = split_relative_entry(relative, context)?;
        let parent = self.ensure_relative_directory(parent, context)?;
        parent.reject_portable_child_aliases(name, context)?;
        let mut file = nt_open_relative(
            &parent.file,
            name,
            FILE_READ_DATA
                | FILE_WRITE_DATA
                | FILE_READ_ATTRIBUTES
                | FILE_WRITE_ATTRIBUTES
                | DELETE
                | SYNCHRONIZE,
            FILE_CREATE,
            FILE_NON_DIRECTORY_FILE
                | FILE_OPEN_REPARSE_POINT
                | FILE_SYNCHRONOUS_IO_NONALERT
                | FILE_WRITE_THROUGH,
            FILE_ATTRIBUTE_NORMAL,
        )?;
        let created_identity = FileIdentity::from_information(&validate_open_regular_file(
            &file,
            &parent.display_path.join(name),
            context,
            None,
        )?);
        let result = (|| -> Result<(), Error> {
            file.write_all(bytes)?;
            file.sync_all()?;
            parent.require_child_file_identity(name, created_identity, context)?;
            parent.sync()?;
            Ok(())
        })();
        if let Err(error) = result {
            let rollback =
                delete_open_entry(&file).and_then(|()| parent.sync().map_err(error_to_io));
            if let Err(rollback_error) = rollback {
                return Err(invalid_cache_state(format!(
                    "{context} {} failed after creation: {error}; rollback failed: {rollback_error}",
                    parent.display_path.join(name).display(),
                )));
            }
            return Err(error);
        }
        Ok(())
    }

    pub(crate) fn read_file(&self, relative: &Path, max_bytes: usize) -> Result<Vec<u8>, Error> {
        let (parent, name) = split_relative_entry(relative, "module cache file")?;
        let parent = self.open_relative_directory(parent, "module cache file parent")?;
        let mut file = parent.open_regular_file(name, "module cache file")?;
        let before = validate_open_regular_file(
            &file,
            &parent.display_path.join(name),
            "module cache file",
            None,
        )?;
        let max_len = u64::try_from(max_bytes).unwrap_or(u64::MAX);
        if before.size > max_len {
            return Err(invalid_cache_state(format!(
                "module cache file {} exceeds the {max_bytes}-byte limit",
                parent.display_path.join(name).display(),
            )));
        }
        file.seek(SeekFrom::Start(0))?;
        let mut bytes = Vec::new();
        bytes
            .try_reserve(
                usize::try_from(before.size)
                    .unwrap_or(max_bytes)
                    .min(max_bytes),
            )
            .map_err(|_| {
                Error::Io(io::Error::new(
                    io::ErrorKind::OutOfMemory,
                    format!(
                        "cannot allocate module cache file buffer for {}",
                        parent.display_path.join(name).display(),
                    ),
                ))
            })?;
        (&mut file)
            .take(max_len.saturating_add(1))
            .read_to_end(&mut bytes)?;
        if bytes.len() > max_bytes {
            return Err(invalid_cache_state(format!(
                "module cache file {} exceeds the {max_bytes}-byte limit",
                parent.display_path.join(name).display(),
            )));
        }
        let after = validate_open_regular_file(
            &file,
            &parent.display_path.join(name),
            "module cache file",
            None,
        )?;
        if before != after {
            return Err(invalid_cache_state(format!(
                "module cache file {} changed generation while it was read",
                parent.display_path.join(name).display(),
            )));
        }
        parent.require_child_file_identity(
            name,
            FileIdentity::from_information(&before),
            "module cache file",
        )?;
        Ok(bytes)
    }

    pub(crate) fn remove_tree(&self, name: &OsStr, context: &str) -> Result<(), Error> {
        let child = self.open_child(name, context)?;
        child.remove_all_entries()?;
        self.remove_open_directory(name, &child, context)
    }

    pub(crate) fn remove_regular_file(&self, name: &OsStr, context: &str) -> Result<(), Error> {
        let opened = self.open_movable_entry(name, false, context)?;
        self.require_child_file_identity(name, opened.identity, context)?;
        delete_open_entry(&opened.file)?;
        self.sync()?;
        Ok(())
    }

    fn remove_reparse_point(&self, name: &OsStr, context: &str) -> Result<(), Error> {
        let entry = self
            .find_exact_entry(name, context)?
            .ok_or_else(|| missing_exact_entry(&self.display_path.join(name), context))?;
        if entry.attributes & FILE_ATTRIBUTE_REPARSE_POINT == 0 {
            return Err(invalid_cache_state(format!(
                "{context} {} changed from a reparse point before cleanup",
                self.display_path.join(name).display(),
            )));
        }
        let type_option = if entry.attributes & FILE_ATTRIBUTE_DIRECTORY != 0 {
            FILE_DIRECTORY_FILE
        } else {
            FILE_NON_DIRECTORY_FILE
        };
        let file = nt_open_relative_reparse(
            &self.file,
            name,
            DELETE | FILE_READ_ATTRIBUTES | SYNCHRONIZE,
            FILE_OPEN,
            FILE_OPEN_REPARSE_POINT | FILE_SYNCHRONOUS_IO_NONALERT | type_option,
            FILE_ATTRIBUTE_NORMAL,
        )?;
        let information = crate::windows_file::file_information(&file)
            .map_err(|error| contextual_io_error(context, &self.display_path.join(name), error))?;
        if information.delete_pending
            || information.attributes & FILE_ATTRIBUTE_REPARSE_POINT == 0
            || FileIdentity::from_information(&information) != entry.identity
        {
            return Err(invalid_cache_state(format!(
                "{context} {} changed identity or type while it was opened",
                self.display_path.join(name).display(),
            )));
        }
        self.require_child_file_identity(name, entry.identity, context)?;
        delete_open_entry(&file)?;
        self.sync()?;
        Ok(())
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
        self.require_child_file_identity(name, opened.identity, context)
    }

    fn require_child_identity_without_alias_scan(
        &self,
        name: &OsStr,
        opened: &Self,
        context: &str,
    ) -> Result<(), Error> {
        validate_external_child_name(name)?;
        let current = self
            .entry_records()?
            .into_iter()
            .find(|entry| entry.name == name)
            .ok_or_else(|| missing_exact_entry(&self.display_path.join(name), context))?;
        if current.identity != opened.identity {
            return Err(invalid_cache_state(format!(
                "{context} {} changed identity after it was opened",
                self.display_path.join(name).display(),
            )));
        }
        Ok(())
    }

    fn require_child_file_identity(
        &self,
        name: &OsStr,
        expected: FileIdentity,
        context: &str,
    ) -> Result<(), Error> {
        let current = self
            .find_exact_entry(name, context)?
            .ok_or_else(|| missing_exact_entry(&self.display_path.join(name), context))?;
        if current.identity != expected {
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
        let current = self.open_relative_directory(relative, context)?;
        if current.identity != opened.identity {
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
            identity: self.identity,
        })
    }

    fn open_relative_directory(&self, relative: &Path, context: &str) -> Result<Self, Error> {
        validate_relative_or_empty_path(relative)?;
        let mut current = self.duplicate()?;
        if relative.as_os_str().is_empty() || relative == Path::new(".") {
            return Ok(current);
        }
        for component in relative.components() {
            let Component::Normal(component) = component else {
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
                FileSystemEntryKind::RegularFile => {
                    frame
                        .directory
                        .remove_regular_file(&name, "module cache cleanup entry")?;
                }
                FileSystemEntryKind::Missing => {
                    return Err(invalid_cache_state(format!(
                        "module cache entry {} disappeared during cleanup",
                        frame.directory.display_path.join(&name).display(),
                    )));
                }
                FileSystemEntryKind::Symlink => {
                    frame
                        .directory
                        .remove_reparse_point(&name, "module cache cleanup reparse point")?;
                }
                FileSystemEntryKind::Special | FileSystemEntryKind::Unknown => {
                    return Err(invalid_cache_state(format!(
                        "module cache entry {} has an unsafe type during cleanup",
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
        self.require_child_identity(name, opened, context)?;
        if !opened.entries()?.is_empty() {
            return Err(invalid_cache_state(format!(
                "{context} {} must remain empty during cleanup",
                opened.display_path.display(),
            )));
        }
        let movable = self.open_movable_entry(name, true, context)?;
        movable.require_identity(opened.identity, context)?;
        delete_open_entry(&movable.file)?;
        self.sync()?;
        Ok(())
    }

    fn sync(&self) -> Result<(), Error> {
        nt_flush(&self.file).map_err(Error::Io)
    }
}

fn cleanup_directory_frame(
    directory: AnchoredDirectory,
    name_in_parent: Option<OsString>,
    depth: usize,
    entry_count: &mut usize,
    max_entries: usize,
    root: &Path,
) -> Result<CleanupDirectoryFrame, Error> {
    let mut records =
        directory.entries_with_budget(entry_count, max_entries, "module cache cleanup", root)?;
    records.reverse();
    Ok(CleanupDirectoryFrame {
        directory,
        name_in_parent,
        entries: records.into_iter().map(|entry| entry.name).collect(),
        depth,
    })
}

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

fn normalize_cache_root_path(cache_root: &Path) -> Result<PathBuf, Error> {
    let normalized = normalize_absolute_cache_path(cache_root)?;
    let namespace = windows_namespace_anchor(&normalized)?;
    if normalized == namespace {
        return Err(invalid_cache_state(
            "a Windows volume or share root cannot be used as the module cache root".to_string(),
        ));
    }
    let current_directory = normalize_absolute_cache_path(&std::env::current_dir()?)?;
    if paths_equal_portably(&normalized, &current_directory) {
        return Err(invalid_cache_state(format!(
            "the current working directory cannot be used as the module cache root: {}",
            normalized.display(),
        )));
    }
    Ok(normalized)
}

fn normalize_absolute_cache_path(path: &Path) -> Result<PathBuf, Error> {
    let absolute = if path.is_absolute() {
        path.to_path_buf()
    } else {
        std::env::current_dir()?.join(path)
    };
    let mut normalized = PathBuf::new();
    let mut saw_prefix = false;
    let mut saw_root = false;
    for component in absolute.components() {
        match component {
            Component::Prefix(prefix) => {
                if saw_prefix || saw_root {
                    return Err(invalid_cache_state(format!(
                        "module cache root has an invalid Windows prefix: {}",
                        path.display(),
                    )));
                }
                if !matches!(
                    prefix.kind(),
                    Prefix::Disk(_)
                        | Prefix::VerbatimDisk(_)
                        | Prefix::UNC(_, _)
                        | Prefix::VerbatimUNC(_, _)
                ) {
                    return Err(invalid_cache_state(format!(
                        "module cache root must use a Windows drive or UNC namespace: {}",
                        path.display(),
                    )));
                }
                saw_prefix = true;
                normalized.push(prefix.as_os_str());
            }
            Component::RootDir => {
                saw_root = true;
                normalized.push(component.as_os_str());
            }
            Component::CurDir => {}
            Component::Normal(component) => normalized.push(component),
            Component::ParentDir => {
                let anchor = windows_namespace_anchor(&normalized)?;
                if paths_equal_portably(&normalized, &anchor) || !normalized.pop() {
                    return Err(invalid_cache_state(format!(
                        "module cache root escapes its Windows namespace: {}",
                        path.display(),
                    )));
                }
            }
        }
    }
    if !saw_prefix || !saw_root {
        return Err(invalid_cache_state(format!(
            "module cache root must use an absolute Windows drive or UNC path: {}",
            path.display(),
        )));
    }
    Ok(normalized)
}

fn windows_namespace_anchor(path: &Path) -> Result<PathBuf, Error> {
    let mut anchor = PathBuf::new();
    let mut saw_prefix = false;
    let mut saw_root = false;
    for component in path.components() {
        match component {
            Component::Prefix(prefix) => {
                saw_prefix = true;
                anchor.push(prefix.as_os_str());
            }
            Component::RootDir => {
                saw_root = true;
                anchor.push(component.as_os_str());
            }
            Component::CurDir => {}
            Component::ParentDir | Component::Normal(_) => break,
        }
    }
    if !saw_prefix || !saw_root {
        return Err(invalid_cache_state(format!(
            "path {} has no absolute Windows namespace anchor",
            path.display(),
        )));
    }
    Ok(anchor)
}

fn paths_equal_portably(left: &Path, right: &Path) -> bool {
    match (left.to_str(), right.to_str()) {
        (Some(left), Some(right)) => {
            crate::schema::portable_case_key(left) == crate::schema::portable_case_key(right)
        }
        _ => left
            .as_os_str()
            .encode_wide()
            .eq(right.as_os_str().encode_wide()),
    }
}

fn open_cache_root(cache_root: &Path, create: bool) -> Result<AnchoredDirectory, Error> {
    let root = open_absolute_directory(cache_root, create, true, "module cache root")?;
    let current = open_current_directory()?;
    if root.identity == current.identity {
        return Err(invalid_cache_state(format!(
            "the current working directory cannot be used as the module cache root: {}",
            cache_root.display(),
        )));
    }
    root.require_path_identity(cache_root, "module cache root")?;
    Ok(root)
}

fn open_current_directory() -> Result<AnchoredDirectory, Error> {
    use std::os::windows::fs::OpenOptionsExt;

    let display_path = normalize_absolute_cache_path(&std::env::current_dir()?)?;
    let file = std::fs::OpenOptions::new()
        .read(true)
        .share_mode(FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE)
        .custom_flags(FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT)
        .open(Path::new("."))
        .map_err(|error| contextual_io_error("current working directory", Path::new("."), error))?;
    AnchoredDirectory::from_file(file, display_path, "current working directory")
}

fn open_absolute_directory(
    path: &Path,
    create: bool,
    mutation_access: bool,
    context: &str,
) -> Result<AnchoredDirectory, Error> {
    use std::os::windows::fs::OpenOptionsExt;

    let namespace = windows_namespace_anchor(path)?;
    let file = std::fs::OpenOptions::new()
        .read(true)
        .share_mode(FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE)
        .custom_flags(FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT)
        .open(&namespace)
        .map_err(|error| contextual_io_error(context, &namespace, error))?;
    let mut current = AnchoredDirectory::from_file(file, namespace.clone(), context)?;
    let relative = path.strip_prefix(&namespace).map_err(|_| {
        invalid_cache_state(format!(
            "normalized Windows cache path {} is outside namespace {}",
            path.display(),
            namespace.display(),
        ))
    })?;
    for component in relative.components() {
        let Component::Normal(name) = component else {
            return Err(invalid_cache_state(format!(
                "{context} must be a canonical absolute path: {}",
                path.display(),
            )));
        };
        current = match current.entry_kind_external(name, context)? {
            FileSystemEntryKind::Missing if create => {
                let mutable_parent = reopen_directory_for_mutation(&current, context)?;
                match mutable_parent.create_child_directory_external(name, context) {
                    Ok(directory) => directory,
                    Err(Error::Io(error)) if error.kind() == io::ErrorKind::AlreadyExists => {
                        mutable_parent.open_child_external(name, context)?
                    }
                    Err(error) => return Err(error),
                }
            }
            FileSystemEntryKind::Missing => {
                return Err(Error::Io(io::Error::new(
                    io::ErrorKind::NotFound,
                    format!("{context} {} does not exist", path.display()),
                )));
            }
            FileSystemEntryKind::Directory => current.open_child_external(name, context)?,
            other => {
                return Err(invalid_cache_state(format!(
                    "{context} component {} must be a non-reparse directory; found {other:?}",
                    current.display_path.join(name).display(),
                )));
            }
        };
    }
    if mutation_access {
        reopen_directory_for_mutation(&current, context)
    } else {
        Ok(current)
    }
}

fn reopen_directory_for_mutation(
    directory: &AnchoredDirectory,
    context: &str,
) -> Result<AnchoredDirectory, Error> {
    use std::os::windows::fs::OpenOptionsExt;

    let file = std::fs::OpenOptions::new()
        .access_mode(DIRECTORY_MUTATION_ACCESS)
        .share_mode(FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE)
        .custom_flags(FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT)
        .open(&directory.display_path)
        .map_err(|error| contextual_io_error(context, &directory.display_path, error))?;
    let reopened = AnchoredDirectory::from_file(file, directory.display_path.clone(), context)?;
    if reopened.identity != directory.identity {
        return Err(invalid_cache_state(format!(
            "{context} {} changed identity while mutation access was acquired",
            directory.display_path.display(),
        )));
    }
    Ok(reopened)
}

impl AnchoredDirectory {
    /// External cache-root components may contain host-specific names. They
    /// still require exact spelling and reject reparse points, while internal
    /// cache paths additionally pass the portable component validator.
    fn entry_kind_external(
        &self,
        name: &OsStr,
        context: &str,
    ) -> Result<FileSystemEntryKind, Error> {
        validate_external_child_name(name)?;
        Ok(self
            .find_exact_external_entry(name, context)?
            .map_or(FileSystemEntryKind::Missing, |entry| entry.kind()))
    }

    fn find_exact_external_entry(
        &self,
        expected: &OsStr,
        context: &str,
    ) -> Result<Option<DirectoryEntry>, Error> {
        validate_external_child_name(expected)?;
        let mut exact = None;
        for entry in self.entry_records()? {
            if entry.name == expected {
                exact = Some(entry);
                continue;
            }
            if let (Some(expected), Some(actual)) = (expected.to_str(), entry.name.to_str()) {
                if crate::schema::portable_case_key(expected)
                    == crate::schema::portable_case_key(actual)
                {
                    return Err(invalid_cache_state(format!(
                        "{context} {} conflicts with existing spelling {:?}",
                        self.display_path.join(expected).display(),
                        actual,
                    )));
                }
            }
        }
        Ok(exact)
    }

    fn open_child_external(&self, name: &OsStr, context: &str) -> Result<Self, Error> {
        let expected = self
            .find_exact_external_entry(name, context)?
            .ok_or_else(|| missing_exact_entry(&self.display_path.join(name), context))?;
        if expected.kind() != FileSystemEntryKind::Directory {
            return Err(invalid_cache_state(format!(
                "{context} {} must be a non-reparse directory",
                self.display_path.join(name).display(),
            )));
        }
        let file = nt_open_relative_external(
            &self.file,
            name,
            DIRECTORY_READ_ACCESS,
            FILE_OPEN,
            FILE_DIRECTORY_FILE | FILE_OPEN_REPARSE_POINT | FILE_SYNCHRONOUS_IO_NONALERT,
            FILE_ATTRIBUTE_NORMAL,
        )?;
        let opened = Self::from_file(file, self.display_path.join(name), context)?;
        if opened.identity != expected.identity {
            return Err(invalid_cache_state(format!(
                "{context} {} changed identity while it was opened",
                opened.display_path.display(),
            )));
        }
        Ok(opened)
    }

    fn create_child_directory_external(&self, name: &OsStr, context: &str) -> Result<Self, Error> {
        self.create_child_directory_external_with_post_create(name, context, |_| Ok(()))
    }

    fn create_child_directory_external_with_post_create(
        &self,
        name: &OsStr,
        context: &str,
        post_create: impl FnOnce(&Self) -> Result<(), Error>,
    ) -> Result<Self, Error> {
        validate_external_child_name(name)?;
        if self.find_exact_external_entry(name, context)?.is_some() {
            return Err(Error::Io(io::Error::new(
                io::ErrorKind::AlreadyExists,
                format!(
                    "{context} {} already exists",
                    self.display_path.join(name).display()
                ),
            )));
        }
        let file = nt_open_relative_external(
            &self.file,
            name,
            DIRECTORY_MUTATION_ACCESS | DELETE,
            FILE_CREATE,
            FILE_DIRECTORY_FILE
                | FILE_OPEN_REPARSE_POINT
                | FILE_SYNCHRONOUS_IO_NONALERT
                | FILE_WRITE_THROUGH,
            FILE_ATTRIBUTE_NORMAL,
        )?;
        let opened = Self::from_file(file, self.display_path.join(name), context)?;
        let validation = (|| -> Result<(), Error> {
            post_create(&opened)?;
            self.require_child_identity_without_alias_scan(name, &opened, context)?;
            let current = self
                .find_exact_external_entry(name, context)?
                .ok_or_else(|| missing_exact_entry(&self.display_path.join(name), context))?;
            if current.identity != opened.identity {
                return Err(invalid_cache_state(format!(
                    "{context} {} changed identity while it was created",
                    opened.display_path.display(),
                )));
            }
            self.require_child_identity_without_alias_scan(name, &opened, context)?;
            self.sync()?;
            Ok(())
        })();
        if let Err(error) = validation {
            return Err(self.rollback_created_child_directory(name, &opened, context, error));
        }
        Ok(opened)
    }
}

fn open_cache_owner_marker(
    root: &AnchoredDirectory,
    create: bool,
) -> Result<Arc<LockedFile>, Error> {
    let name = OsStr::new(CACHE_OWNER_MARKER);
    let mut observed_incomplete = false;
    for _ in 0..OWNER_MARKER_ACQUISITION_ATTEMPTS {
        match root.entry_kind(name)? {
            FileSystemEntryKind::Missing => {
                if !create {
                    return Err(invalid_cache_state(format!(
                        "module cache root {} is missing required owner marker {CACHE_OWNER_MARKER}",
                        root.display_path.display(),
                    )));
                }
                if !root.entries()?.is_empty() {
                    return Err(invalid_cache_state(format!(
                        "module cache root {} is non-empty but missing required owner marker {CACHE_OWNER_MARKER}; move or remove it and retry",
                        root.display_path.display(),
                    )));
                }
                let file = match create_regular_file_handle(root, name) {
                    Ok(file) => file,
                    Err(Error::Io(error)) if error.kind() == io::ErrorKind::AlreadyExists => {
                        observed_incomplete = true;
                        continue;
                    }
                    Err(error) => return Err(error),
                };
                let mut locked = LockedFile::acquire(file, LeaseMode::Exclusive)?;
                let initialization = (|| -> Result<(), Error> {
                    let entries = root.entries()?;
                    if entries != [name.to_os_string()] {
                        return Err(invalid_cache_state(format!(
                            "module cache root {} gained entries while owner marker was initialized",
                            root.display_path.display(),
                        )));
                    }
                    locked.file.seek(SeekFrom::Start(0))?;
                    locked.file.write_all(CACHE_OWNER_MARKER_CONTENT)?;
                    locked.file.sync_all()?;
                    root.sync()?;
                    validate_cache_owner_marker_file(&locked.file, root)
                })();
                if let Err(error) = initialization {
                    let rollback = delete_open_entry(&locked.file)
                        .and_then(|()| root.sync().map_err(error_to_io));
                    if let Err(rollback_error) = rollback {
                        return Err(invalid_cache_state(format!(
                            "owner-marker initialization failed: {error}; rollback failed: {rollback_error}"
                        )));
                    }
                    return Err(error);
                }
                return Ok(Arc::new(locked.downgrade()?));
            }
            FileSystemEntryKind::RegularFile => {
                let file = open_read_write_file(root, name, "module cache owner marker")?;
                let locked = Arc::new(LockedFile::acquire(file, LeaseMode::Shared)?);
                match validate_cache_owner_marker_file(locked.file(), root) {
                    Ok(()) => return Ok(locked),
                    Err(error) => {
                        let information = crate::windows_file::file_information(locked.file())?;
                        if !create || information.size != 0 {
                            return Err(error);
                        }
                        require_cache_root_path_identity(root)?;
                        validate_open_file_at(
                            locked.file(),
                            root,
                            name,
                            "incomplete module cache owner marker",
                        )?;
                        observed_incomplete = true;
                        drop(locked);
                        std::thread::yield_now();
                    }
                }
            }
            other => {
                return Err(invalid_cache_state(format!(
                    "module cache owner marker {} must be a non-reparse regular file; found {other:?}",
                    root.display_path.join(name).display(),
                )));
            }
        }
    }
    Err(invalid_cache_state(format!(
        "module cache owner marker {} did not reach a complete generation after {OWNER_MARKER_ACQUISITION_ATTEMPTS} attempts{}",
        root.display_path.join(name).display(),
        if observed_incomplete {
            " while another initializer was observed"
        } else {
            ""
        },
    )))
}

fn validate_cache_owner_marker_file(file: &File, root: &AnchoredDirectory) -> Result<(), Error> {
    let path = root.display_path.join(CACHE_OWNER_MARKER);
    let before = validate_open_regular_file(file, &path, "module cache owner marker", None)?;
    if before.size != CACHE_OWNER_MARKER_CONTENT.len() as u64 {
        return Err(invalid_cache_state(format!(
            "module cache owner marker {} has invalid length {}",
            path.display(),
            before.size,
        )));
    }
    let mut reader = file.try_clone()?;
    reader.seek(SeekFrom::Start(0))?;
    let mut content = Vec::new();
    reader
        .take((CACHE_OWNER_MARKER_CONTENT.len() + 1) as u64)
        .read_to_end(&mut content)?;
    let after = validate_open_regular_file(
        file,
        &path,
        "module cache owner marker",
        Some(FileIdentity::from_information(&before)),
    )?;
    if before != after || content != CACHE_OWNER_MARKER_CONTENT {
        return Err(invalid_cache_state(format!(
            "module cache owner marker {} changed or has invalid content",
            path.display(),
        )));
    }
    validate_open_file_at(
        file,
        root,
        OsStr::new(CACHE_OWNER_MARKER),
        "module cache owner marker",
    )
}

fn open_lock_file_at(
    directory: &AnchoredDirectory,
    name: &OsStr,
    context: &str,
    create: bool,
) -> Result<File, Error> {
    for _ in 0..OWNER_MARKER_ACQUISITION_ATTEMPTS {
        match directory.entry_kind(name)? {
            FileSystemEntryKind::Missing if create => {
                match create_regular_file_handle(directory, name) {
                    Ok(file) => {
                        file.sync_all()?;
                        directory.sync()?;
                        validate_open_file_at(&file, directory, name, context)?;
                        return Ok(file);
                    }
                    Err(Error::Io(error)) if error.kind() == io::ErrorKind::AlreadyExists => {}
                    Err(error) => return Err(error),
                }
            }
            FileSystemEntryKind::Missing => {
                return Err(Error::Io(io::Error::new(
                    io::ErrorKind::NotFound,
                    format!(
                        "{context} {} does not exist",
                        directory.display_path.join(name).display()
                    ),
                )));
            }
            FileSystemEntryKind::RegularFile => {
                let file = open_read_write_file(directory, name, context)?;
                validate_open_file_at(&file, directory, name, context)?;
                return Ok(file);
            }
            other => {
                return Err(invalid_cache_state(format!(
                    "{context} {} must be a non-reparse regular file; found {other:?}",
                    directory.display_path.join(name).display(),
                )));
            }
        }
    }
    Err(invalid_cache_state(format!(
        "failed to acquire stable {context} {}",
        directory.display_path.join(name).display(),
    )))
}

fn create_regular_file_handle(directory: &AnchoredDirectory, name: &OsStr) -> Result<File, Error> {
    directory.reject_portable_child_aliases(name, "module cache file")?;
    nt_open_relative(
        &directory.file,
        name,
        FILE_READ_DATA
            | FILE_WRITE_DATA
            | FILE_READ_ATTRIBUTES
            | FILE_WRITE_ATTRIBUTES
            | DELETE
            | SYNCHRONIZE,
        FILE_CREATE,
        FILE_NON_DIRECTORY_FILE
            | FILE_OPEN_REPARSE_POINT
            | FILE_SYNCHRONOUS_IO_NONALERT
            | FILE_WRITE_THROUGH,
        FILE_ATTRIBUTE_NORMAL,
    )
}

fn open_read_write_file(
    directory: &AnchoredDirectory,
    name: &OsStr,
    context: &str,
) -> Result<File, Error> {
    let entry = directory
        .find_exact_entry(name, context)?
        .ok_or_else(|| missing_exact_entry(&directory.display_path.join(name), context))?;
    if entry.kind() != FileSystemEntryKind::RegularFile {
        return Err(invalid_cache_state(format!(
            "{context} {} must be a regular file",
            directory.display_path.join(name).display(),
        )));
    }
    let file = nt_open_relative(
        &directory.file,
        name,
        FILE_READ_DATA
            | FILE_WRITE_DATA
            | FILE_READ_ATTRIBUTES
            | FILE_WRITE_ATTRIBUTES
            | DELETE
            | SYNCHRONIZE,
        FILE_OPEN,
        FILE_NON_DIRECTORY_FILE | FILE_OPEN_REPARSE_POINT | FILE_SYNCHRONOUS_IO_NONALERT,
        FILE_ATTRIBUTE_NORMAL,
    )?;
    validate_open_regular_file(
        &file,
        &directory.display_path.join(name),
        context,
        Some(entry.identity),
    )?;
    Ok(file)
}

const ADMISSION_BYTE: u64 = 0;
const STATE_BYTE: u64 = 1;

fn acquire_lease(file: &File, mode: LeaseMode) -> Result<(), Error> {
    match mode {
        LeaseMode::Shared => {
            lock_range(file, ADMISSION_BYTE, LeaseMode::Shared)?;
            if let Err(error) = lock_range(file, STATE_BYTE, LeaseMode::Shared) {
                let _ = unlock_range(file, ADMISSION_BYTE);
                return Err(Error::Io(error));
            }
            if let Err(error) = unlock_range(file, ADMISSION_BYTE) {
                let _ = unlock_range(file, STATE_BYTE);
                return Err(Error::Io(error));
            }
        }
        LeaseMode::Exclusive => {
            lock_range(file, ADMISSION_BYTE, LeaseMode::Exclusive)?;
            if let Err(error) = lock_range(file, STATE_BYTE, LeaseMode::Exclusive) {
                let _ = unlock_range(file, ADMISSION_BYTE);
                return Err(Error::Io(error));
            }
        }
    }
    Ok(())
}

fn lock_range(file: &File, offset: u64, mode: LeaseMode) -> io::Result<()> {
    let mut overlapped = OVERLAPPED::default();
    overlapped.Anonymous.Anonymous.Offset = offset as u32;
    overlapped.Anonymous.Anonymous.OffsetHigh = (offset >> 32) as u32;
    let flags = if mode == LeaseMode::Exclusive {
        LOCKFILE_EXCLUSIVE_LOCK
    } else {
        0
    };
    let result = unsafe {
        LockFileEx(
            file.as_raw_handle() as HANDLE,
            flags,
            0,
            1,
            0,
            &mut overlapped,
        )
    };
    if result == 0 {
        Err(io::Error::last_os_error())
    } else {
        Ok(())
    }
}

fn unlock_range(file: &File, offset: u64) -> io::Result<()> {
    let mut overlapped = OVERLAPPED::default();
    overlapped.Anonymous.Anonymous.Offset = offset as u32;
    overlapped.Anonymous.Anonymous.OffsetHigh = (offset >> 32) as u32;
    let result = unsafe { UnlockFileEx(file.as_raw_handle() as HANDLE, 0, 1, 0, &mut overlapped) };
    if result == 0 {
        Err(io::Error::last_os_error())
    } else {
        Ok(())
    }
}

fn nt_open_relative(
    parent: &File,
    name: &OsStr,
    desired_access: u32,
    disposition: u32,
    options: u32,
    attributes: u32,
) -> Result<File, Error> {
    validate_child_name(name)?;
    nt_open_relative_external(
        parent,
        name,
        desired_access,
        disposition,
        options,
        attributes,
    )
}

fn nt_open_relative_reparse(
    parent: &File,
    name: &OsStr,
    desired_access: u32,
    disposition: u32,
    options: u32,
    attributes: u32,
) -> Result<File, Error> {
    validate_child_name(name)?;
    nt_open_relative_external_with_attributes(
        parent,
        name,
        desired_access,
        disposition,
        options,
        attributes,
        0,
    )
}

fn nt_open_relative_external(
    parent: &File,
    name: &OsStr,
    desired_access: u32,
    disposition: u32,
    options: u32,
    attributes: u32,
) -> Result<File, Error> {
    nt_open_relative_external_with_attributes(
        parent,
        name,
        desired_access,
        disposition,
        options,
        attributes,
        OBJ_DONT_REPARSE,
    )
}

fn nt_open_relative_external_with_attributes(
    parent: &File,
    name: &OsStr,
    desired_access: u32,
    disposition: u32,
    options: u32,
    attributes: u32,
    object_attributes: u32,
) -> Result<File, Error> {
    validate_external_child_name(name)?;
    let mut name_wide = name.encode_wide().collect::<Vec<_>>();
    let byte_len = name_wide
        .len()
        .checked_mul(std::mem::size_of::<u16>())
        .ok_or_else(|| invalid_cache_state("Windows cache name length overflow".to_string()))?;
    let length = u16::try_from(byte_len).map_err(|_| {
        invalid_cache_state(format!(
            "Windows cache name is too long: {:?}",
            name.to_string_lossy(),
        ))
    })?;
    let unicode = UNICODE_STRING {
        Length: length,
        MaximumLength: length,
        Buffer: name_wide.as_mut_ptr(),
    };
    let attributes_struct = OBJECT_ATTRIBUTES {
        Length: u32::try_from(std::mem::size_of::<OBJECT_ATTRIBUTES>())
            .expect("OBJECT_ATTRIBUTES size fits in u32"),
        RootDirectory: parent.as_raw_handle() as HANDLE,
        ObjectName: &unicode,
        Attributes: object_attributes,
        SecurityDescriptor: std::ptr::null(),
        SecurityQualityOfService: std::ptr::null(),
    };
    let mut status_block = IO_STATUS_BLOCK::default();
    let mut handle: HANDLE = std::ptr::null_mut();
    let status = unsafe {
        NtCreateFile(
            &mut handle,
            desired_access,
            &attributes_struct,
            &mut status_block,
            std::ptr::null(),
            attributes,
            FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
            disposition,
            options | FILE_OPEN_FOR_BACKUP_INTENT,
            std::ptr::null(),
            0,
        )
    };
    if status < 0 {
        if !handle.is_null() && handle != INVALID_HANDLE_VALUE {
            unsafe {
                CloseHandle(handle);
            }
        }
        return Err(Error::Io(ntstatus_error(status)));
    }
    if handle.is_null() || handle == INVALID_HANDLE_VALUE {
        return Err(invalid_cache_state(
            "NtCreateFile succeeded without returning a valid handle".to_string(),
        ));
    }
    Ok(unsafe { File::from_raw_handle(handle.cast()) })
}

fn ntstatus_error(status: i32) -> io::Error {
    let code = unsafe { RtlNtStatusToDosError(status) };
    io::Error::from_raw_os_error(i32::try_from(code).unwrap_or(i32::MAX))
}

fn nt_flush(file: &File) -> io::Result<()> {
    let mut status_block = IO_STATUS_BLOCK::default();
    let status = unsafe { NtFlushBuffersFile(file.as_raw_handle() as HANDLE, &mut status_block) };
    if status < 0 {
        Err(ntstatus_error(status))
    } else {
        Ok(())
    }
}

fn rename_entry(
    source: &File,
    destination_parent: &AnchoredDirectory,
    destination_name: &OsStr,
) -> Result<(), Error> {
    validate_child_name(destination_name)?;
    destination_parent
        .reject_portable_child_aliases(destination_name, "module cache rename destination")?;
    let name = destination_name.encode_wide().collect::<Vec<_>>();
    let name_bytes = name
        .len()
        .checked_mul(std::mem::size_of::<u16>())
        .ok_or_else(|| invalid_cache_state("Windows rename name length overflow".to_string()))?;
    let file_name_offset = std::mem::offset_of!(FILE_RENAME_INFORMATION, FileName);
    let buffer_len = file_name_offset
        .checked_add(name_bytes)
        .ok_or_else(|| invalid_cache_state("Windows rename buffer length overflow".to_string()))?;
    const _: () =
        assert!(std::mem::align_of::<u64>() >= std::mem::align_of::<FILE_RENAME_INFORMATION>());
    let word_len = buffer_len
        .checked_add(std::mem::size_of::<u64>() - 1)
        .ok_or_else(|| {
            invalid_cache_state("Windows rename buffer alignment overflow".to_string())
        })?
        / std::mem::size_of::<u64>();
    let mut storage = vec![0u64; word_len];
    let information = storage.as_mut_ptr().cast::<FILE_RENAME_INFORMATION>();
    unsafe {
        (*information).Anonymous.Flags = 0;
        (*information).RootDirectory = destination_parent.file.as_raw_handle() as HANDLE;
        (*information).FileNameLength = u32::try_from(name_bytes)
            .map_err(|_| invalid_cache_state("Windows rename name is too long".to_string()))?;
        std::ptr::copy_nonoverlapping(
            name.as_ptr(),
            std::ptr::addr_of_mut!((*information).FileName).cast::<u16>(),
            name.len(),
        );
    }
    let mut status_block = IO_STATUS_BLOCK::default();
    let status = unsafe {
        NtSetInformationFile(
            source.as_raw_handle() as HANDLE,
            &mut status_block,
            information.cast(),
            u32::try_from(buffer_len).map_err(|_| {
                invalid_cache_state("Windows rename buffer is too large".to_string())
            })?,
            FileRenameInformationEx,
        )
    };
    if status < 0 {
        return Err(Error::Io(ntstatus_error(status)));
    }
    Ok(())
}

fn delete_open_entry(file: &File) -> io::Result<()> {
    let information = FILE_DISPOSITION_INFORMATION_EX {
        Flags: FILE_DISPOSITION_DELETE
            | FILE_DISPOSITION_POSIX_SEMANTICS
            | FILE_DISPOSITION_IGNORE_READONLY_ATTRIBUTE,
    };
    let mut status_block = IO_STATUS_BLOCK::default();
    let status = unsafe {
        NtSetInformationFile(
            file.as_raw_handle() as HANDLE,
            &mut status_block,
            std::ptr::from_ref(&information).cast(),
            u32::try_from(std::mem::size_of::<FILE_DISPOSITION_INFORMATION_EX>())
                .expect("FILE_DISPOSITION_INFORMATION_EX size fits in u32"),
            FileDispositionInformationEx,
        )
    };
    if status < 0 {
        Err(ntstatus_error(status))
    } else {
        Ok(())
    }
}

fn query_directory_entries(file: &File) -> Result<Vec<DirectoryEntry>, Error> {
    let mut result = Vec::new();
    let mut restart = true;
    let volume = crate::windows_file::file_information(file)?.volume;
    loop {
        const BUFFER_WORDS: usize =
            DIRECTORY_QUERY_BUFFER_BYTES.div_ceil(std::mem::size_of::<u64>());
        const _: () =
            assert!(std::mem::align_of::<u64>() >= std::mem::align_of::<FILE_ID_EXTD_DIR_INFO>());
        let mut storage = vec![0u64; BUFFER_WORDS];
        let buffer_len = storage.len() * std::mem::size_of::<u64>();
        let buffer = storage.as_mut_ptr().cast::<u8>();
        let class = if restart {
            FileIdExtdDirectoryRestartInfo
        } else {
            FileIdExtdDirectoryInfo
        };
        let succeeded = unsafe {
            GetFileInformationByHandleEx(
                file.as_raw_handle() as HANDLE,
                class,
                buffer.cast(),
                u32::try_from(buffer_len).expect("directory query buffer fits in u32"),
            )
        };
        if succeeded == 0 {
            let error = io::Error::last_os_error();
            if error.raw_os_error() == Some(18) {
                break;
            }
            return Err(Error::Io(error));
        }
        restart = false;
        let mut offset = 0usize;
        loop {
            if !offset.is_multiple_of(std::mem::align_of::<FILE_ID_EXTD_DIR_INFO>()) {
                return Err(invalid_cache_state(
                    "Windows directory enumeration returned a misaligned entry offset".to_string(),
                ));
            }
            let header_end = offset
                .checked_add(std::mem::offset_of!(FILE_ID_EXTD_DIR_INFO, FileName))
                .ok_or_else(|| {
                    invalid_cache_state("Windows directory entry offset overflow".to_string())
                })?;
            if header_end > buffer_len {
                return Err(invalid_cache_state(
                    "Windows directory enumeration returned a truncated header".to_string(),
                ));
            }
            let entry = unsafe { &*buffer.add(offset).cast::<FILE_ID_EXTD_DIR_INFO>() };
            let name_bytes = usize::try_from(entry.FileNameLength).map_err(|_| {
                invalid_cache_state("Windows directory name length overflow".to_string())
            })?;
            if name_bytes % std::mem::size_of::<u16>() != 0 {
                return Err(invalid_cache_state(
                    "Windows directory enumeration returned an odd UTF-16 length".to_string(),
                ));
            }
            let name_end = header_end.checked_add(name_bytes).ok_or_else(|| {
                invalid_cache_state("Windows directory name range overflow".to_string())
            })?;
            if name_end > buffer_len {
                return Err(invalid_cache_state(
                    "Windows directory enumeration returned a truncated name".to_string(),
                ));
            }
            let name = unsafe {
                std::slice::from_raw_parts(
                    buffer.add(header_end).cast::<u16>(),
                    name_bytes / std::mem::size_of::<u16>(),
                )
            };
            result.push(DirectoryEntry {
                name: OsString::from_wide(name),
                identity: FileIdentity {
                    volume,
                    file: entry.FileId.Identifier,
                },
                attributes: entry.FileAttributes,
            });
            if result.len() > MAX_DIRECTORY_ENTRIES {
                return Err(invalid_cache_state(format!(
                    "Windows module cache directory contains more than {MAX_DIRECTORY_ENTRIES} entries"
                )));
            }
            if entry.NextEntryOffset == 0 {
                break;
            }
            let next = usize::try_from(entry.NextEntryOffset).map_err(|_| {
                invalid_cache_state("Windows directory next-entry offset overflow".to_string())
            })?;
            if next < std::mem::offset_of!(FILE_ID_EXTD_DIR_INFO, FileName) {
                return Err(invalid_cache_state(
                    "Windows directory enumeration returned a non-progressing entry offset"
                        .to_string(),
                ));
            }
            offset = offset.checked_add(next).ok_or_else(|| {
                invalid_cache_state("Windows directory entry offset overflow".to_string())
            })?;
            if offset >= buffer_len {
                return Err(invalid_cache_state(
                    "Windows directory enumeration returned an out-of-range entry offset"
                        .to_string(),
                ));
            }
        }
    }
    Ok(result)
}

fn validate_directory_information(
    file: &File,
    path: &Path,
    context: &str,
) -> Result<WindowsFileInformation, Error> {
    let information = crate::windows_file::file_information(file)
        .map_err(|error| contextual_io_error(context, path, error))?;
    if !information.directory
        || information.delete_pending
        || information.attributes & FILE_ATTRIBUTE_DIRECTORY == 0
        || information.attributes & FILE_ATTRIBUTE_REPARSE_POINT != 0
    {
        return Err(invalid_cache_state(format!(
            "{context} {} must remain a non-reparse directory",
            path.display(),
        )));
    }
    Ok(information)
}

fn validate_open_regular_file(
    file: &File,
    path: &Path,
    context: &str,
    expected: Option<FileIdentity>,
) -> Result<WindowsFileInformation, Error> {
    let information = crate::windows_file::file_information(file)
        .map_err(|error| contextual_io_error(context, path, error))?;
    let identity = FileIdentity::from_information(&information);
    if information.directory
        || information.delete_pending
        || information.links != 1
        || information.attributes & FILE_ATTRIBUTE_DIRECTORY != 0
        || information.attributes & FILE_ATTRIBUTE_REPARSE_POINT != 0
        || expected.is_some_and(|expected| expected != identity)
    {
        return Err(invalid_cache_state(format!(
            "{context} {} must remain one non-reparse regular-file entry",
            path.display(),
        )));
    }
    Ok(information)
}

fn validate_open_file_at(
    file: &File,
    directory: &AnchoredDirectory,
    name: &OsStr,
    context: &str,
) -> Result<(), Error> {
    let information =
        validate_open_regular_file(file, &directory.display_path.join(name), context, None)?;
    directory.require_child_file_identity(
        name,
        FileIdentity::from_information(&information),
        context,
    )
}

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
            "module cache publication must move from {STAGING_DIR} into the installed cache"
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
    let directory = match from_parent.entry_kind(from_name)? {
        FileSystemEntryKind::Directory => true,
        FileSystemEntryKind::RegularFile => false,
        source_kind => {
            return Err(invalid_cache_state(format!(
                "module cache publication source {} must be a real directory or regular file; found {source_kind:?}",
                from.display(),
            )));
        }
    };
    let source =
        from_parent.open_movable_entry(from_name, directory, "module cache publication source")?;
    if directory {
        source.sync()?;
    }
    validate_cache_authority(root, owner_marker, staging, lease_file)?;
    root.require_relative_directory_identity(
        to_parent_path,
        &to_parent,
        "module cache destination parent",
    )?;
    from_parent.require_child_file_identity(
        from_name,
        source.identity,
        "module cache publication source",
    )?;
    rename_entry(&source.file, &to_parent, to_name)?;
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
        || {
            to_parent.require_child_file_identity(
                to_name,
                source.identity,
                "published module cache source",
            )
        },
    )
}

fn complete_publication<F>(
    confirmation: PublicationConfirmation<'_>,
    mut validate_committed_identity: F,
) -> Result<(), Error>
where
    F: FnMut() -> Result<(), Error>,
{
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

fn sync_published_parents(
    source_parent: &AnchoredDirectory,
    destination_parent: &AnchoredDirectory,
    destination: &Path,
) -> Result<(), Error> {
    let source_result = source_parent.sync();
    // A committed rename requires both durability attempts even when the first
    // flush fails, so the error honestly reports every unconfirmed namespace.
    let destination_result = destination_parent.sync();
    match (source_result.err(), destination_result.err()) {
        (None, None) => Ok(()),
        (Some(source), None) => Err(publication_committed_error(
            destination,
            format!(
                "source parent {}: {source}",
                source_parent.display_path.display(),
            ),
        )),
        (None, Some(destination_error)) => Err(publication_committed_error(
            destination,
            format!(
                "destination parent {}: {destination_error}",
                destination_parent.display_path.display(),
            ),
        )),
        (Some(source), Some(destination_error)) => Err(publication_committed_error(
            destination,
            format!(
                "source parent {}: {source}; destination parent {}: {destination_error}",
                source_parent.display_path.display(),
                destination_parent.display_path.display(),
            ),
        )),
    }
}

fn publication_committed_error(destination: &Path, message: String) -> Error {
    Error::CachePublicationDurabilityUnconfirmed {
        path: destination.display().to_string(),
        message,
    }
}

fn publication_relative_path(path: &Path, root: &Path) -> Option<PathBuf> {
    let normalized = normalize_absolute_cache_path(path).ok()?;
    normalized.strip_prefix(root).ok().map(Path::to_path_buf)
}

fn require_cache_root_path_identity(root: &AnchoredDirectory) -> Result<(), Error> {
    root.require_path_identity(&root.display_path, "module cache root")
}

pub(crate) fn require_secure_cache_mutation_support() -> Result<(), Error> {
    Ok(())
}

pub(crate) fn ensure_real_directory(path: &Path, context: &str) -> Result<(), Error> {
    let path = normalize_absolute_cache_path(path)?;
    let directory = open_absolute_directory(&path, true, true, context)?;
    directory.require_path_identity(&path, context)?;
    directory.sync()
}

pub(crate) fn require_real_directory(path: &Path, context: &str) -> Result<(), Error> {
    let path = normalize_absolute_cache_path(path)?;
    let directory = open_absolute_directory(&path, false, false, context)?;
    directory.require_path_identity(&path, context)
}

pub(crate) fn reject_portable_leaf_aliases(path: &Path, context: &str) -> Result<(), Error> {
    inspect_leaf_spelling(path, context, false)
}

pub(crate) fn require_exact_leaf_spelling(path: &Path, context: &str) -> Result<(), Error> {
    inspect_leaf_spelling(path, context, true)
}

fn inspect_leaf_spelling(path: &Path, context: &str, require_exact: bool) -> Result<(), Error> {
    let absolute = normalize_absolute_cache_path(path)?;
    let Some(expected) = absolute.file_name() else {
        return Ok(());
    };
    validate_child_name(expected)?;
    let parent_path = absolute.parent().ok_or_else(|| {
        invalid_cache_state(format!(
            "{context} {} has no parent directory",
            path.display()
        ))
    })?;
    let parent = open_absolute_directory(parent_path, false, false, context)?;
    let found = parent.find_exact_entry(expected, context)?;
    if require_exact && found.is_none() {
        return Err(missing_exact_entry(&absolute, context));
    }
    if !require_exact && found.is_some() {
        return Err(Error::Io(io::Error::new(
            io::ErrorKind::AlreadyExists,
            format!("{context} {} already exists", absolute.display()),
        )));
    }
    Ok(())
}

#[cfg(test)]
pub(crate) fn staging_lock_path(cache_root: &Path) -> PathBuf {
    cache_root.join(STAGING_DIR).join(STAGING_LOCK_FILE)
}

fn validate_publication_destination(path: &Path) -> Result<(), Error> {
    validate_relative_path(path)?;
    if relative_starts_with(path, STAGING_DIR) {
        return Err(invalid_cache_state(
            "transaction destination must be outside the staging directory".to_string(),
        ));
    }
    Ok(())
}

fn validate_relative_or_empty_path(path: &Path) -> Result<(), Error> {
    if path.as_os_str().is_empty() || path == Path::new(".") {
        return Ok(());
    }
    validate_relative_path(path)
}

fn validate_relative_path(path: &Path) -> Result<(), Error> {
    if path.as_os_str().is_empty() || path.is_absolute() {
        return Err(invalid_cache_state(format!(
            "module cache path must be non-empty, canonical, and relative: {}",
            path.display(),
        )));
    }
    let mut components = 0usize;
    for component in path.components() {
        let Component::Normal(name) = component else {
            return Err(invalid_cache_state(format!(
                "module cache path must be non-empty, canonical, and relative: {}",
                path.display(),
            )));
        };
        validate_child_name(name)?;
        components = components.checked_add(1).ok_or_else(|| {
            invalid_cache_state("module cache path component count overflow".to_string())
        })?;
        if components > crate::schema::MAX_PORTABLE_PATH_COMPONENTS {
            return Err(invalid_cache_state(format!(
                "module cache path {} exceeds the {}-component limit",
                path.display(),
                crate::schema::MAX_PORTABLE_PATH_COMPONENTS,
            )));
        }
    }
    Ok(())
}

fn split_relative_entry<'a>(path: &'a Path, context: &str) -> Result<(&'a Path, &'a OsStr), Error> {
    validate_relative_path(path)?;
    let parent = path.parent().unwrap_or_else(|| Path::new(""));
    let name = path.file_name().ok_or_else(|| {
        invalid_cache_state(format!(
            "{context} has no final path component: {}",
            path.display()
        ))
    })?;
    validate_child_name(name)?;
    Ok((parent, name))
}

fn relative_starts_with(path: &Path, expected: &str) -> bool {
    matches!(
        path.components().next(),
        Some(Component::Normal(component)) if component == OsStr::new(expected)
    )
}

fn validate_child_name(name: &OsStr) -> Result<(), Error> {
    validate_external_child_name(name)?;
    let text = name.to_str().ok_or_else(|| {
        invalid_cache_state("module cache entry names must be valid Unicode".to_string())
    })?;
    let mut components = Path::new(text).components();
    if !matches!(components.next(), Some(Component::Normal(_))) || components.next().is_some() {
        return Err(invalid_cache_state(format!(
            "module cache entry name {text:?} must contain exactly one path component"
        )));
    }
    crate::schema::portable_relative_path_from_path(Path::new(text)).map_err(|error| {
        invalid_cache_state(format!(
            "module cache entry name {text:?} is not portable: {error}"
        ))
    })?;
    Ok(())
}

fn validate_external_child_name(name: &OsStr) -> Result<(), Error> {
    if name.is_empty()
        || name == OsStr::new(".")
        || name == OsStr::new("..")
        || name.encode_wide().any(|unit| unit == 0)
    {
        return Err(invalid_cache_state(format!(
            "invalid Windows cache path component: {:?}",
            name.to_string_lossy(),
        )));
    }
    Ok(())
}

fn missing_exact_entry(path: &Path, context: &str) -> Error {
    invalid_cache_state(format!(
        "{context} {} does not exist with its exact spelling",
        path.display(),
    ))
}

fn cache_error_to_io(error: Error) -> io::Error {
    match error {
        Error::Io(error) => error,
        other => io::Error::new(io::ErrorKind::InvalidData, other.to_string()),
    }
}

fn contextual_io_error(context: &str, path: &Path, error: io::Error) -> Error {
    Error::Io(io::Error::new(
        error.kind(),
        format!("{context} {}: {error}", path.display()),
    ))
}

fn error_to_io(error: Error) -> io::Error {
    match error {
        Error::Io(error) => error,
        other => io::Error::other(other.to_string()),
    }
}

fn invalid_cache_state(detail: String) -> Error {
    Error::Io(io::Error::new(io::ErrorKind::InvalidData, detail))
}

#[cfg(test)]
mod tests {
    use std::sync::mpsc::{self, RecvTimeoutError};
    use std::sync::{Arc, Barrier};
    use std::thread;
    use std::time::Duration;

    use super::*;

    const LOCK_WAIT: Duration = Duration::from_secs(10);
    const MUST_STILL_BE_BLOCKED: Duration = Duration::from_millis(150);

    #[test]
    fn owner_marker_generation_matches_cache_layout_generation() {
        let expected = format!(
            "volang-module-cache-{}\n",
            crate::cache::CACHE_LAYOUT_GENERATION,
        );

        assert_eq!(CACHE_OWNER_MARKER_CONTENT, expected.as_bytes());
    }

    #[test]
    fn post_create_portable_alias_failure_removes_only_the_created_directory() {
        let root = tempfile::tempdir().unwrap();
        let parent = open_absolute_directory(root.path(), false, true, "test parent").unwrap();
        let requested = OsStr::new("Straße");
        let alias = root.path().join("STRASSE");

        let error = parent
            .create_child_directory_with_post_create(requested, "test directory", |_| {
                std::fs::create_dir(&alias).unwrap();
                Ok(())
            })
            .unwrap_err();

        assert!(error.to_string().contains("portable spelling"), "{error}");
        assert!(!root.path().join(requested).exists());
        assert!(alias.is_dir());
    }

    #[test]
    fn external_post_create_alias_failure_removes_only_the_created_directory() {
        let root = tempfile::tempdir().unwrap();
        let parent = open_absolute_directory(root.path(), false, true, "test parent").unwrap();
        let requested = OsStr::new("Straße");
        let alias = root.path().join("STRASSE");

        let error = parent
            .create_child_directory_external_with_post_create(
                requested,
                "test external directory",
                |_| {
                    std::fs::create_dir(&alias).unwrap();
                    Ok(())
                },
            )
            .unwrap_err();

        assert!(error.to_string().contains("existing spelling"), "{error}");
        assert!(!root.path().join(requested).exists());
        assert!(alias.is_dir());
    }

    #[test]
    fn post_create_path_replacement_is_preserved_when_rollback_loses_identity() {
        let root = tempfile::tempdir().unwrap();
        let parent = open_absolute_directory(root.path(), false, true, "test parent").unwrap();
        let requested = OsStr::new("created");
        let requested_path = root.path().join(requested);
        let moved = root.path().join("moved-created");

        let error = parent
            .create_child_directory_with_post_create(requested, "test directory", |opened| {
                std::fs::rename(&opened.display_path, &moved).unwrap();
                std::fs::create_dir(&opened.display_path).unwrap();
                Err(invalid_cache_state(
                    "injected post-create validation failure".to_string(),
                ))
            })
            .unwrap_err();

        assert!(error.to_string().contains("changed identity"), "{error}");
        assert!(error.to_string().contains("rollback refused"), "{error}");
        assert!(requested_path.is_dir());
        assert!(moved.is_dir());
    }

    #[test]
    fn post_create_nonempty_directory_is_preserved_when_rollback_is_unsafe() {
        let root = tempfile::tempdir().unwrap();
        let parent = open_absolute_directory(root.path(), false, true, "test parent").unwrap();
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

        assert!(error.to_string().contains("gained content"), "{error}");
        assert!(error.to_string().contains("rollback refused"), "{error}");
        assert_eq!(std::fs::read(sentinel).unwrap(), b"preserve");
    }

    #[test]
    fn cache_initialization_is_explicit_and_reopenable() {
        let temporary = tempfile::tempdir().unwrap();
        let cache = temporary.path().join("cache");

        assert!(CacheMutationLock::shared_existing(&cache).is_err());
        let initialized = CacheMutationLock::shared(&cache).unwrap();
        initialized.validate_cache_ownership().unwrap();
        assert_eq!(
            std::fs::read(cache.join(CACHE_OWNER_MARKER)).unwrap(),
            CACHE_OWNER_MARKER_CONTENT,
        );
        drop(initialized);

        let reopened = CacheMutationLock::shared_existing(&cache).unwrap();
        reopened.validate_cache_ownership().unwrap();
    }

    #[test]
    fn concurrent_first_acquirers_observe_one_complete_owner_marker() {
        const WORKERS: usize = 8;

        let temporary = tempfile::tempdir().unwrap();
        let cache = temporary.path().join("cache");
        let barrier = Arc::new(Barrier::new(WORKERS));
        let mut workers = Vec::new();
        for _ in 0..WORKERS {
            let cache = cache.clone();
            let barrier = Arc::clone(&barrier);
            workers.push(thread::spawn(move || {
                barrier.wait();
                let guard = CacheMutationLock::shared(&cache)?;
                guard.validate_cache_ownership()
            }));
        }
        for worker in workers {
            worker.join().unwrap().unwrap();
        }
        assert_eq!(
            std::fs::read(cache.join(CACHE_OWNER_MARKER)).unwrap(),
            CACHE_OWNER_MARKER_CONTENT,
        );
    }

    #[test]
    fn exclusive_lease_waits_for_every_shared_lease() {
        let temporary = tempfile::tempdir().unwrap();
        let cache = temporary.path().join("cache");
        let first = CacheMutationLock::shared(&cache).unwrap();
        let second = CacheMutationLock::shared_existing(&cache).unwrap();
        let (sender, receiver) = mpsc::channel();
        let exclusive_cache = cache.clone();
        let worker = thread::spawn(move || {
            let result = CacheMutationLock::exclusive(&exclusive_cache);
            sender.send(result).unwrap();
        });

        assert!(matches!(
            receiver.recv_timeout(MUST_STILL_BE_BLOCKED),
            Err(RecvTimeoutError::Timeout),
        ));
        drop(first);
        assert!(matches!(
            receiver.recv_timeout(MUST_STILL_BE_BLOCKED),
            Err(RecvTimeoutError::Timeout),
        ));
        drop(second);

        let exclusive = receiver
            .recv_timeout(LOCK_WAIT)
            .expect("exclusive Windows cache lease remained blocked")
            .unwrap();
        exclusive.validate_cache_ownership().unwrap();
        drop(exclusive);
        worker.join().unwrap();
    }

    #[test]
    fn transaction_publication_is_atomic_and_no_replace() {
        let temporary = tempfile::tempdir().unwrap();
        let cache = temporary.path().join("cache");
        let cache_lock = CacheMutationLock::shared(&cache).unwrap();

        let mut first = cache_lock.begin_transaction("first").unwrap();
        first.create_dir_all(Path::new("source")).unwrap();
        first
            .write_file(Path::new("source/main.vo"), b"first")
            .unwrap();
        first
            .publish_directory(Path::new("source"), Path::new("packages/demo"))
            .unwrap();
        first.cleanup().unwrap();
        assert_eq!(
            cache_lock
                .read_file(Path::new("packages/demo/main.vo"), 16)
                .unwrap(),
            b"first",
        );

        let mut second = cache_lock.begin_transaction("second").unwrap();
        second.create_dir_all(Path::new("source")).unwrap();
        second
            .write_file(Path::new("source/main.vo"), b"second")
            .unwrap();
        assert!(second
            .publish_directory(Path::new("source"), Path::new("packages/demo"))
            .is_err());
        second.cleanup().unwrap();
        assert_eq!(
            cache_lock
                .read_file(Path::new("packages/demo/main.vo"), 16)
                .unwrap(),
            b"first",
        );
    }

    #[test]
    fn portable_alias_cannot_publish_a_second_generation() {
        let temporary = tempfile::tempdir().unwrap();
        let cache = temporary.path().join("cache");
        let cache_lock = CacheMutationLock::shared(&cache).unwrap();

        let mut first = cache_lock.begin_transaction("first-alias").unwrap();
        first.create_dir_all(Path::new("source")).unwrap();
        first
            .write_file(Path::new("source/main.vo"), b"first")
            .unwrap();
        first
            .publish_directory(Path::new("source"), Path::new("packages/Foo"))
            .unwrap();
        first.cleanup().unwrap();

        let mut second = cache_lock.begin_transaction("second-alias").unwrap();
        second.create_dir_all(Path::new("source")).unwrap();
        second
            .write_file(Path::new("source/main.vo"), b"second")
            .unwrap();
        assert!(second
            .publish_directory(Path::new("source"), Path::new("packages/foo"))
            .is_err());
        second.cleanup().unwrap();
        assert_eq!(
            cache_lock
                .read_file(Path::new("packages/Foo/main.vo"), 16)
                .unwrap(),
            b"first",
        );
    }
}
