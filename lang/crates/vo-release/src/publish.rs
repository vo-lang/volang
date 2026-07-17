#[cfg(all(test, unix))]
use std::cell::{Cell, RefCell};
use std::collections::BTreeSet;
#[cfg(unix)]
use std::ffi::OsStr;
#[cfg(unix)]
use std::io;
#[cfg(unix)]
use std::path::{Component, Path};

use crate::{ReleaseError, ReleaseResult};

// A process crash can leave one of these private directories behind. We never
// delete an unknown matching directory automatically because ownership and
// liveness cannot be established safely from its name alone.
pub(crate) const RELEASE_STAGE_DIR_PREFIX: &str = ".vo-release-stage-";
#[cfg(unix)]
const MAX_STAGE_DIR_ATTEMPTS: usize = 128;

#[cfg(unix)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PendingState {
    Open,
    Sealed,
    Published,
}

#[cfg(unix)]
fn validate_asset_name(name: &str) -> ReleaseResult<()> {
    let mut components = Path::new(name).components();
    if !matches!(components.next(), Some(Component::Normal(_))) || components.next().is_some() {
        return Err(ReleaseError::ManifestSerialize(format!(
            "release asset name {name:?} must be one normal path component"
        )));
    }
    Ok(())
}

#[cfg(unix)]
pub(crate) fn new_stage_dir_name() -> io::Result<String> {
    let mut random = [0_u8; 16];
    getrandom::fill(&mut random).map_err(|error| {
        io::Error::other(format!("operating-system randomness failed: {error}"))
    })?;
    let mut name = format!("{RELEASE_STAGE_DIR_PREFIX}{}-", std::process::id());
    for byte in random {
        name.push(char::from(b"0123456789abcdef"[usize::from(byte >> 4)]));
        name.push(char::from(b"0123456789abcdef"[usize::from(byte & 0x0f)]));
    }
    Ok(name)
}

#[cfg(unix)]
fn final_name(final_dir: &Path) -> ReleaseResult<&OsStr> {
    final_dir.file_name().ok_or_else(|| {
        ReleaseError::IoError(
            final_dir.to_path_buf(),
            "output directory must end in a normal directory name".to_string(),
        )
    })
}

#[cfg(all(test, unix))]
thread_local! {
    static FAIL_NEXT_PARENT_SYNC_AFTER_PUBLISH: Cell<bool> = const { Cell::new(false) };
    static AFTER_RENAME_HOOK: RefCell<Option<Box<dyn FnOnce()>>> = const { RefCell::new(None) };
    static AFTER_FIRST_SOURCE_CHUNK_HOOK: RefCell<Option<Box<dyn FnOnce()>>> = const { RefCell::new(None) };
}

#[cfg(all(test, unix))]
pub(crate) fn fail_next_parent_sync_after_publish() {
    FAIL_NEXT_PARENT_SYNC_AFTER_PUBLISH.with(|flag| flag.set(true));
}

#[cfg(all(test, unix))]
pub(crate) fn set_after_rename_hook(hook: impl FnOnce() + 'static) {
    AFTER_RENAME_HOOK.with(|slot| {
        let previous = slot.replace(Some(Box::new(hook)));
        assert!(previous.is_none(), "after-rename test hook is already set");
    });
}

#[cfg(all(test, unix))]
pub(crate) fn set_after_first_source_chunk_hook(hook: impl FnOnce() + 'static) {
    AFTER_FIRST_SOURCE_CHUNK_HOOK.with(|slot| {
        let previous = slot.replace(Some(Box::new(hook)));
        assert!(
            previous.is_none(),
            "after-source-chunk test hook is already set"
        );
    });
}

#[cfg(unix)]
fn injected_parent_sync_failure() -> io::Result<()> {
    #[cfg(test)]
    if FAIL_NEXT_PARENT_SYNC_AFTER_PUBLISH.with(|flag| flag.replace(false)) {
        return Err(io::Error::other(
            "injected parent-directory synchronization failure",
        ));
    }
    Ok(())
}

#[cfg(unix)]
fn run_after_rename_hook() {
    #[cfg(test)]
    AFTER_RENAME_HOOK.with(|slot| {
        if let Some(hook) = slot.borrow_mut().take() {
            hook();
        }
    });
}

#[cfg(unix)]
fn run_after_first_source_chunk_hook() {
    #[cfg(test)]
    AFTER_FIRST_SOURCE_CHUNK_HOOK.with(|slot| {
        if let Some(hook) = slot.borrow_mut().take() {
            hook();
        }
    });
}

#[cfg(unix)]
mod imp {
    use std::ffi::{OsStr, OsString};
    use std::fs::File;
    use std::io::{self, Read, Write};
    use std::os::unix::ffi::OsStringExt;
    use std::os::unix::fs::MetadataExt;
    use std::path::{Component, Path, PathBuf};

    use rustix::fd::OwnedFd;
    use rustix::fs::{AtFlags, Dir, FileType, Mode, OFlags};
    use rustix::io::Errno;
    use sha2::{Digest, Sha256};

    use super::{
        final_name, injected_parent_sync_failure, new_stage_dir_name,
        run_after_first_source_chunk_hook, run_after_rename_hook, validate_asset_name, BTreeSet,
        PendingState, ReleaseError, ReleaseResult, MAX_STAGE_DIR_ATTEMPTS,
        RELEASE_STAGE_DIR_PREFIX,
    };

    struct CreatedFile {
        name: String,
        device: i128,
        inode: u64,
    }

    struct RecordedFile {
        name: String,
        device: i128,
        inode: u64,
        size: u64,
        mode: u32,
        link_count: u64,
        digest: String,
    }

    struct OpenedFile {
        file: File,
        device: i128,
        inode: u64,
        size: u64,
        mode: u32,
        link_count: u64,
    }

    struct BoundedDigestWriter {
        file: File,
        hasher: Sha256,
        total: u64,
        max_bytes: u64,
    }

    impl BoundedDigestWriter {
        fn new(file: File, max_bytes: u64) -> Self {
            Self {
                file,
                hasher: Sha256::new(),
                total: 0,
                max_bytes,
            }
        }

        fn finish(self) -> (File, u64, String) {
            (
                self.file,
                self.total,
                format!("sha256:{:x}", self.hasher.finalize()),
            )
        }
    }

    impl Write for BoundedDigestWriter {
        fn write(&mut self, bytes: &[u8]) -> io::Result<usize> {
            let requested = u64::try_from(bytes.len())
                .map_err(|_| io::Error::other("release asset write size exceeds u64"))?;
            let projected = self
                .total
                .checked_add(requested)
                .ok_or_else(|| io::Error::other("release asset size overflow"))?;
            if projected > self.max_bytes {
                return Err(io::Error::other(format!(
                    "release asset exceeds the {}-byte limit",
                    self.max_bytes
                )));
            }
            let written = self.file.write(bytes)?;
            self.total = self
                .total
                .checked_add(
                    u64::try_from(written)
                        .map_err(|_| io::Error::other("release asset write size exceeds u64"))?,
                )
                .ok_or_else(|| io::Error::other("release asset size overflow"))?;
            self.hasher.update(&bytes[..written]);
            Ok(written)
        }

        fn flush(&mut self) -> io::Result<()> {
            self.file.flush()
        }
    }

    const NORMALIZED_MODE_MASK: u64 = 0o177_777;

    fn normalized_mode<T: Into<u64>>(mode: T) -> Option<u32> {
        u32::try_from(mode.into() & NORMALIZED_MODE_MASK).ok()
    }

    fn normalized_link_count<T: Into<u64>>(link_count: T) -> u64 {
        link_count.into()
    }

    fn normalized_device<T: Into<i128>>(device: T) -> i128 {
        device.into()
    }

    pub(crate) struct PendingOutputDir {
        parent_path: PathBuf,
        final_path: PathBuf,
        final_name: std::ffi::OsString,
        parent_fd: OwnedFd,
        parent_device: i128,
        parent_inode: u64,
        stage_name: String,
        stage_fd: OwnedFd,
        stage_device: i128,
        stage_inode: u64,
        created_files: Vec<CreatedFile>,
        recorded_files: Vec<RecordedFile>,
        state: PendingState,
    }

    impl PendingOutputDir {
        pub(crate) fn create(final_dir: &Path) -> ReleaseResult<Self> {
            let parent_path = final_dir.parent().ok_or_else(|| {
                ReleaseError::IoError(
                    final_dir.to_path_buf(),
                    "output directory must have a parent".to_string(),
                )
            })?;
            let final_name = final_name(final_dir)?.to_os_string();
            let parent_fd = rustix::fs::open(
                parent_path,
                OFlags::RDONLY | OFlags::DIRECTORY | OFlags::CLOEXEC | OFlags::NOFOLLOW,
                Mode::empty(),
            )
            .map_err(|error| {
                ReleaseError::IoError(
                    parent_path.to_path_buf(),
                    io::Error::from(error).to_string(),
                )
            })?;
            let parent_stat = rustix::fs::fstat(&parent_fd).map_err(|error| {
                ReleaseError::IoError(
                    parent_path.to_path_buf(),
                    io::Error::from(error).to_string(),
                )
            })?;
            ensure_absent(&parent_fd, &final_name, final_dir)?;

            for _ in 0..MAX_STAGE_DIR_ATTEMPTS {
                let stage_name = new_stage_dir_name().map_err(|error| {
                    ReleaseError::IoError(parent_path.to_path_buf(), error.to_string())
                })?;
                if OsStr::new(&stage_name) == final_name {
                    continue;
                }
                match rustix::fs::mkdirat(&parent_fd, stage_name.as_str(), Mode::RWXU) {
                    Ok(()) => {
                        let stage_fd = match rustix::fs::openat(
                            &parent_fd,
                            stage_name.as_str(),
                            OFlags::RDONLY | OFlags::DIRECTORY | OFlags::CLOEXEC | OFlags::NOFOLLOW,
                            Mode::empty(),
                        ) {
                            Ok(fd) => fd,
                            Err(error) => {
                                let _ = rustix::fs::unlinkat(
                                    &parent_fd,
                                    stage_name.as_str(),
                                    AtFlags::REMOVEDIR,
                                );
                                return Err(ReleaseError::IoError(
                                    parent_path.join(&stage_name),
                                    io::Error::from(error).to_string(),
                                ));
                            }
                        };
                        if let Err(error) = rustix::fs::fchmod(&stage_fd, Mode::RWXU) {
                            drop(stage_fd);
                            let _ = rustix::fs::unlinkat(
                                &parent_fd,
                                stage_name.as_str(),
                                AtFlags::REMOVEDIR,
                            );
                            return Err(ReleaseError::IoError(
                                parent_path.join(&stage_name),
                                io::Error::from(error).to_string(),
                            ));
                        }
                        let stat = match rustix::fs::fstat(&stage_fd) {
                            Ok(stat) => stat,
                            Err(error) => {
                                drop(stage_fd);
                                let _ = rustix::fs::unlinkat(
                                    &parent_fd,
                                    stage_name.as_str(),
                                    AtFlags::REMOVEDIR,
                                );
                                return Err(ReleaseError::IoError(
                                    parent_path.join(&stage_name),
                                    io::Error::from(error).to_string(),
                                ));
                            }
                        };
                        let Some(stage_mode) = normalized_mode(stat.st_mode) else {
                            drop(stage_fd);
                            let _ = rustix::fs::unlinkat(
                                &parent_fd,
                                stage_name.as_str(),
                                AtFlags::REMOVEDIR,
                            );
                            return Err(ReleaseError::IoError(
                                parent_path.join(&stage_name),
                                "release staging directory mode cannot be represented as u32"
                                    .to_string(),
                            ));
                        };
                        if FileType::from_raw_mode(stat.st_mode) != FileType::Directory
                            || stage_mode & 0o7_777 != 0o700
                        {
                            drop(stage_fd);
                            let _ = rustix::fs::unlinkat(
                                &parent_fd,
                                stage_name.as_str(),
                                AtFlags::REMOVEDIR,
                            );
                            return Err(ReleaseError::IoError(
                                parent_path.join(&stage_name),
                                format!(
                                    "newly created release staging directory has mode {stage_mode:#o}; expected permission bits 0o700"
                                ),
                            ));
                        }
                        let pending = Self {
                            parent_path: parent_path.to_path_buf(),
                            final_path: final_dir.to_path_buf(),
                            final_name,
                            parent_fd,
                            parent_device: normalized_device(parent_stat.st_dev),
                            parent_inode: parent_stat.st_ino,
                            stage_name,
                            stage_fd,
                            stage_device: normalized_device(stat.st_dev),
                            stage_inode: stat.st_ino,
                            created_files: Vec::new(),
                            recorded_files: Vec::new(),
                            state: PendingState::Open,
                        };
                        pending.validate_parent_path_identity()?;
                        pending.validate_stage_name_identity()?;
                        return Ok(pending);
                    }
                    Err(Errno::EXIST) => continue,
                    Err(error) => {
                        return Err(ReleaseError::IoError(
                            parent_path.join(stage_name),
                            io::Error::from(error).to_string(),
                        ));
                    }
                }
            }
            Err(ReleaseError::IoError(
                parent_path.to_path_buf(),
                format!(
                    "failed to allocate a unique release staging directory after {MAX_STAGE_DIR_ATTEMPTS} attempts; stale {RELEASE_STAGE_DIR_PREFIX}* directories are never removed automatically, so after confirming that no release process is active, inspect and remove only abandoned directories owned by the current user"
                ),
            ))
        }

        pub(crate) fn path(&self) -> PathBuf {
            self.parent_path.join(&self.stage_name)
        }

        pub(crate) fn write_new_file(&mut self, name: &str, bytes: &[u8]) -> ReleaseResult<()> {
            let expected_size = u64::try_from(bytes.len()).map_err(|_| {
                ReleaseError::IoError(
                    self.path().join(name),
                    "release asset size exceeds u64".to_string(),
                )
            })?;
            let mut file = self.create_new_file(name)?;
            file.write_all(bytes).map_err(|error| {
                ReleaseError::IoError(self.path().join(name), error.to_string())
            })?;
            self.finish_new_file(name, file, expected_size, digest_bytes(bytes))
        }

        /// Write a generated asset directly into its private staging file while
        /// enforcing the byte limit and computing the exact digest online.
        pub(crate) fn write_new_file_streaming(
            &mut self,
            name: &str,
            max_bytes: u64,
            write: impl FnOnce(&mut dyn Write) -> ReleaseResult<()>,
        ) -> ReleaseResult<(u64, String)> {
            let file = self.create_new_file(name)?;
            let mut target = BoundedDigestWriter::new(file, max_bytes);
            write(&mut target)?;
            target.flush().map_err(|error| {
                ReleaseError::IoError(self.path().join(name), error.to_string())
            })?;
            let (file, size, digest) = target.finish();
            self.finish_new_file(name, file, size, digest.clone())?;
            Ok((size, digest))
        }

        /// Stream one regular artifact into the private staging directory.
        ///
        /// The returned size and digest describe the exact bytes held by the
        /// staged file, so callers can build release metadata without keeping
        /// the complete artifact set resident in memory.
        pub(crate) fn copy_new_file(
            &mut self,
            name: &str,
            source_path: &Path,
            max_bytes: u64,
        ) -> ReleaseResult<(u64, String)> {
            let mut source = open_regular_source_no_follow(source_path).map_err(|error| {
                ReleaseError::IoError(source_path.to_path_buf(), error.to_string())
            })?;
            let source_metadata = source.metadata().map_err(|error| {
                ReleaseError::IoError(source_path.to_path_buf(), error.to_string())
            })?;
            if !source_metadata.file_type().is_file() {
                return Err(ReleaseError::InvalidArtifactPath(source_path.to_path_buf()));
            }
            let advertised_size = source_metadata.len();
            if advertised_size > max_bytes {
                return Err(ReleaseError::IoError(
                    source_path.to_path_buf(),
                    format!("release artifact exceeds the {max_bytes}-byte limit"),
                ));
            }

            let mut target = self.create_new_file(name)?;
            let mut hasher = Sha256::new();
            let mut total = 0_u64;
            let mut buffer = [0_u8; 64 * 1024];
            loop {
                let read = source.read(&mut buffer).map_err(|error| {
                    ReleaseError::IoError(source_path.to_path_buf(), error.to_string())
                })?;
                if read == 0 {
                    break;
                }
                total = total
                    .checked_add(u64::try_from(read).map_err(|_| {
                        ReleaseError::IoError(
                            source_path.to_path_buf(),
                            "release artifact read size exceeds u64".to_string(),
                        )
                    })?)
                    .ok_or_else(|| {
                        ReleaseError::IoError(
                            source_path.to_path_buf(),
                            "release artifact size overflow".to_string(),
                        )
                    })?;
                if total > max_bytes {
                    return Err(ReleaseError::IoError(
                        source_path.to_path_buf(),
                        format!("release artifact exceeds the {max_bytes}-byte limit"),
                    ));
                }
                run_after_first_source_chunk_hook();
                target.write_all(&buffer[..read]).map_err(|error| {
                    ReleaseError::IoError(self.path().join(name), error.to_string())
                })?;
                hasher.update(&buffer[..read]);
            }
            if total != advertised_size {
                return Err(ReleaseError::IoError(
                    source_path.to_path_buf(),
                    format!(
                        "release artifact size changed while being staged: expected {advertised_size}, read {total}"
                    ),
                ));
            }
            let source_after = source.metadata().map_err(|error| {
                ReleaseError::IoError(source_path.to_path_buf(), error.to_string())
            })?;
            if !same_source_metadata(&source_metadata, &source_after) {
                return Err(ReleaseError::IoError(
                    source_path.to_path_buf(),
                    "release artifact changed while being staged".to_string(),
                ));
            }
            let digest = format!("sha256:{:x}", hasher.finalize());
            self.finish_new_file(name, target, total, digest.clone())?;
            Ok((total, digest))
        }

        fn create_new_file(&mut self, name: &str) -> ReleaseResult<File> {
            validate_asset_name(name)?;
            if self.state != PendingState::Open {
                return Err(ReleaseError::IoError(
                    self.path(),
                    "release staging directory is already sealed".to_string(),
                ));
            }
            self.created_files.try_reserve(1).map_err(|_| {
                ReleaseError::IoError(
                    self.path(),
                    "failed to reserve release created-file metadata".to_string(),
                )
            })?;
            self.recorded_files.try_reserve(1).map_err(|_| {
                ReleaseError::IoError(
                    self.path(),
                    "failed to reserve release recorded-file metadata".to_string(),
                )
            })?;
            let fd = rustix::fs::openat(
                &self.stage_fd,
                name,
                OFlags::WRONLY | OFlags::CREATE | OFlags::EXCL | OFlags::CLOEXEC | OFlags::NOFOLLOW,
                Mode::RUSR,
            )
            .map_err(|error| {
                ReleaseError::IoError(self.path().join(name), io::Error::from(error).to_string())
            })?;
            let stat = match rustix::fs::fstat(&fd) {
                Ok(stat) => stat,
                Err(error) => {
                    drop(fd);
                    let _ = rustix::fs::unlinkat(&self.stage_fd, name, AtFlags::empty());
                    return Err(ReleaseError::IoError(
                        self.path().join(name),
                        io::Error::from(error).to_string(),
                    ));
                }
            };
            if FileType::from_raw_mode(stat.st_mode) != FileType::RegularFile {
                drop(fd);
                let _ = rustix::fs::unlinkat(&self.stage_fd, name, AtFlags::empty());
                return Err(ReleaseError::IoError(
                    self.path().join(name),
                    "newly staged release asset is not a regular file".to_string(),
                ));
            }
            self.created_files.push(CreatedFile {
                name: name.to_string(),
                device: normalized_device(stat.st_dev),
                inode: stat.st_ino,
            });
            Ok(File::from(fd))
        }

        fn finish_new_file(
            &mut self,
            name: &str,
            file: File,
            expected_size: u64,
            digest: String,
        ) -> ReleaseResult<()> {
            // Removing every write bit blocks accidental path-based rewrites.
            // The owning account can still chmod or replace entries, so seal
            // and publish both revalidate identity, size, mode, and digest.
            // The remaining boundary is cooperative same-account processes: a
            // hostile same-UID actor can race the final check because portable
            // Unix exposes no owner-immutable file bit to unprivileged code.
            rustix::fs::fchmod(&file, Mode::RUSR).map_err(|error| {
                ReleaseError::IoError(self.path().join(name), io::Error::from(error).to_string())
            })?;
            sync_regular_file_durably(&file).map_err(|error| {
                ReleaseError::IoError(self.path().join(name), error.to_string())
            })?;
            let stat = rustix::fs::fstat(&file).map_err(|error| {
                ReleaseError::IoError(self.path().join(name), io::Error::from(error).to_string())
            })?;
            if FileType::from_raw_mode(stat.st_mode) != FileType::RegularFile {
                return Err(ReleaseError::IoError(
                    self.path().join(name),
                    "newly staged release asset is not a regular file".to_string(),
                ));
            }
            let size = u64::try_from(stat.st_size).map_err(|_| {
                ReleaseError::IoError(
                    self.path().join(name),
                    "staged release asset has an invalid file size".to_string(),
                )
            })?;
            if size != expected_size {
                return Err(ReleaseError::IoError(
                    self.path().join(name),
                    "staged release asset size changed while being written".to_string(),
                ));
            }
            let mode = normalized_mode(stat.st_mode).ok_or_else(|| {
                ReleaseError::IoError(
                    self.path().join(name),
                    "staged release asset mode cannot be represented as u32".to_string(),
                )
            })?;
            if mode & 0o7_777 != 0o400 {
                return Err(ReleaseError::IoError(
                    self.path().join(name),
                    format!(
                        "newly staged release asset has mode {mode:#o}; expected permission bits 0o400"
                    ),
                ));
            }
            let link_count = normalized_link_count(stat.st_nlink);
            if link_count != 1 {
                return Err(ReleaseError::IoError(
                    self.path().join(name),
                    format!(
                        "newly staged release asset has link count {link_count}; expected exactly one link"
                    ),
                ));
            }
            let created = self.created_files.last().ok_or_else(|| {
                ReleaseError::IoError(
                    self.path().join(name),
                    "release staging lost the created-file identity record".to_string(),
                )
            })?;
            if created.name != name
                || created.device != normalized_device(stat.st_dev)
                || created.inode != stat.st_ino
            {
                return Err(ReleaseError::IoError(
                    self.path().join(name),
                    "newly staged release asset identity changed while being written".to_string(),
                ));
            }
            self.recorded_files.push(RecordedFile {
                name: name.to_string(),
                device: normalized_device(stat.st_dev),
                inode: stat.st_ino,
                size,
                mode,
                link_count,
                digest,
            });
            Ok(())
        }

        pub(crate) fn entry_names(&self) -> ReleaseResult<BTreeSet<String>> {
            let max_entries = vo_module::MAX_MODULE_ARTIFACTS
                .checked_add(3)
                .ok_or_else(|| {
                    ReleaseError::IoError(
                        self.path(),
                        "release output entry limit overflowed usize".to_string(),
                    )
                })?;
            let mut names = BTreeSet::new();
            let mut directory = Dir::read_from(&self.stage_fd).map_err(|error| {
                ReleaseError::IoError(self.path(), io::Error::from(error).to_string())
            })?;
            while let Some(entry) = directory.read() {
                let entry = entry.map_err(|error| {
                    ReleaseError::IoError(self.path(), io::Error::from(error).to_string())
                })?;
                let raw = entry.file_name().to_bytes();
                if matches!(raw, b"." | b"..") {
                    continue;
                }
                if names.len() >= max_entries {
                    return Err(ReleaseError::IoError(
                        self.path(),
                        format!("release staging contains more than the {max_entries}-entry limit"),
                    ));
                }
                let name = std::str::from_utf8(raw).map_err(|error| {
                    ReleaseError::IoError(
                        self.path(),
                        format!("staged release asset name is not valid UTF-8: {error}"),
                    )
                })?;
                let stat = rustix::fs::statat(&self.stage_fd, name, AtFlags::SYMLINK_NOFOLLOW)
                    .map_err(|error| {
                        ReleaseError::IoError(
                            self.path().join(name),
                            io::Error::from(error).to_string(),
                        )
                    })?;
                if FileType::from_raw_mode(stat.st_mode) != FileType::RegularFile {
                    return Err(ReleaseError::IoError(
                        self.path().join(name),
                        "staged release entries must be regular files".to_string(),
                    ));
                }
                names.insert(name.to_string());
            }
            Ok(names)
        }

        pub(crate) fn read_file(&self, name: &str, max_bytes: usize) -> ReleaseResult<Vec<u8>> {
            let mut opened = self.open_regular_file(name)?;
            let expected_size = usize::try_from(opened.size).map_err(|_| {
                ReleaseError::IoError(
                    self.path().join(name),
                    "staged release asset has an invalid file size".to_string(),
                )
            })?;
            if expected_size > max_bytes {
                return Err(ReleaseError::IoError(
                    self.path().join(name),
                    format!("staged release asset exceeds the {max_bytes}-byte limit"),
                ));
            }
            let mut bytes = Vec::new();
            bytes.try_reserve_exact(expected_size).map_err(|_| {
                ReleaseError::IoError(
                    self.path().join(name),
                    "failed to reserve staged release asset bytes".to_string(),
                )
            })?;
            let read_limit = u64::try_from(max_bytes)
                .unwrap_or(u64::MAX)
                .saturating_add(1);
            Read::by_ref(&mut opened.file)
                .take(read_limit)
                .read_to_end(&mut bytes)
                .map_err(|error| {
                    ReleaseError::IoError(self.path().join(name), error.to_string())
                })?;
            if bytes.len() != expected_size {
                return Err(ReleaseError::IoError(
                    self.path().join(name),
                    "staged release asset changed while being verified".to_string(),
                ));
            }
            Ok(bytes)
        }

        pub(crate) fn validate_file_digest(
            &self,
            name: &str,
            max_bytes: u64,
            expected_size: u64,
            expected_digest: &str,
        ) -> ReleaseResult<()> {
            let path = self.path().join(name);
            let mut opened = self.open_regular_file(name)?;
            if opened.size > max_bytes {
                return Err(ReleaseError::IoError(
                    path,
                    format!("staged release asset exceeds the {max_bytes}-byte limit"),
                ));
            }
            if opened.size != expected_size {
                return Err(ReleaseError::IoError(
                    path,
                    format!(
                        "staged release asset size mismatch: expected {expected_size}, found {}",
                        opened.size
                    ),
                ));
            }
            let found_digest = digest_open_file(&mut opened.file, &path, max_bytes, opened.size)?;
            if found_digest != expected_digest {
                return Err(ReleaseError::IoError(
                    path,
                    format!(
                        "staged release asset digest mismatch: expected {expected_digest}, found {found_digest}"
                    ),
                ));
            }
            Ok(())
        }

        /// Read a staged regular file through the anchored directory handle and
        /// revalidate its identity after the streaming consumer returns.
        pub(crate) fn with_file_reader<T>(
            &self,
            name: &str,
            max_bytes: u64,
            expected_size: u64,
            read: impl FnOnce(&mut File) -> ReleaseResult<T>,
        ) -> ReleaseResult<T> {
            let path = self.path().join(name);
            let mut opened = self.open_regular_file(name)?;
            if opened.size > max_bytes {
                return Err(ReleaseError::IoError(
                    path,
                    format!("staged release asset exceeds the {max_bytes}-byte limit"),
                ));
            }
            if opened.size != expected_size {
                return Err(ReleaseError::IoError(
                    path,
                    format!(
                        "staged release asset size mismatch: expected {expected_size}, found {}",
                        opened.size
                    ),
                ));
            }
            let value = read(&mut opened.file)?;
            let after = rustix::fs::fstat(&opened.file).map_err(|error| {
                ReleaseError::IoError(path.clone(), io::Error::from(error).to_string())
            })?;
            let after_size = u64::try_from(after.st_size).map_err(|_| {
                ReleaseError::IoError(
                    path.clone(),
                    "staged release asset has an invalid file size".to_string(),
                )
            })?;
            let after_mode = normalized_mode(after.st_mode).ok_or_else(|| {
                ReleaseError::IoError(
                    path.clone(),
                    "staged release asset mode cannot be represented as u32".to_string(),
                )
            })?;
            let after_links = normalized_link_count(after.st_nlink);
            if normalized_device(after.st_dev) != opened.device
                || after.st_ino != opened.inode
                || after_size != opened.size
                || after_mode != opened.mode
                || after_links != opened.link_count
            {
                return Err(ReleaseError::IoError(
                    path,
                    "staged release asset identity changed while being read".to_string(),
                ));
            }
            Ok(value)
        }

        fn open_regular_file(&self, name: &str) -> ReleaseResult<OpenedFile> {
            validate_asset_name(name)?;
            let path = self.path().join(name);
            let fd = rustix::fs::openat(
                &self.stage_fd,
                name,
                OFlags::RDONLY | OFlags::CLOEXEC | OFlags::NOFOLLOW,
                Mode::empty(),
            )
            .map_err(|error| {
                ReleaseError::IoError(path.clone(), io::Error::from(error).to_string())
            })?;
            let stat = rustix::fs::fstat(&fd).map_err(|error| {
                ReleaseError::IoError(path.clone(), io::Error::from(error).to_string())
            })?;
            if FileType::from_raw_mode(stat.st_mode) != FileType::RegularFile {
                return Err(ReleaseError::IoError(
                    path,
                    "staged release asset must be a regular file".to_string(),
                ));
            }
            let size = u64::try_from(stat.st_size).map_err(|_| {
                ReleaseError::IoError(
                    path.clone(),
                    "staged release asset has an invalid file size".to_string(),
                )
            })?;
            Ok(OpenedFile {
                file: File::from(fd),
                device: normalized_device(stat.st_dev),
                inode: stat.st_ino,
                size,
                mode: normalized_mode(stat.st_mode).ok_or_else(|| {
                    ReleaseError::IoError(
                        path.clone(),
                        "staged release asset mode cannot be represented as u32".to_string(),
                    )
                })?,
                link_count: normalized_link_count(stat.st_nlink),
            })
        }

        fn revalidate_recorded_files(&self) -> ReleaseResult<()> {
            if self.created_files.len() != self.recorded_files.len() {
                return Err(ReleaseError::IoError(
                    self.path(),
                    "release staging contains an incompletely recorded asset".to_string(),
                ));
            }
            let expected_names = self
                .recorded_files
                .iter()
                .map(|record| record.name.clone())
                .collect::<BTreeSet<_>>();
            let found_names = self.entry_names()?;
            if found_names != expected_names {
                return Err(ReleaseError::IoError(
                    self.path(),
                    "release staging asset set changed after validation".to_string(),
                ));
            }
            for record in &self.recorded_files {
                let path = self.path().join(&record.name);
                let mut opened = self.open_regular_file(&record.name)?;
                if opened.device != record.device || opened.inode != record.inode {
                    return Err(ReleaseError::IoError(
                        path,
                        "staged release asset identity changed after creation".to_string(),
                    ));
                }
                if opened.mode != record.mode {
                    return Err(ReleaseError::IoError(
                        path,
                        format!(
                            "staged release asset mode changed after creation: expected {:#o}, found {:#o}",
                            record.mode, opened.mode
                        ),
                    ));
                }
                if opened.link_count != record.link_count || opened.link_count != 1 {
                    return Err(ReleaseError::IoError(
                        path,
                        format!(
                            "staged release asset link count changed after creation: expected {}, found {}",
                            record.link_count, opened.link_count
                        ),
                    ));
                }
                if opened.size != record.size {
                    return Err(ReleaseError::IoError(
                        path,
                        format!(
                            "staged release asset size changed after creation: expected {}, found {}",
                            record.size, opened.size
                        ),
                    ));
                }
                let digest = digest_open_file(&mut opened.file, &path, record.size, record.size)?;
                if digest != record.digest {
                    return Err(ReleaseError::IoError(
                        path,
                        format!(
                            "staged release asset digest changed after creation: expected {}, found {digest}",
                            record.digest
                        ),
                    ));
                }
            }
            Ok(())
        }

        fn validate_parent_path_identity(&self) -> ReleaseResult<()> {
            let reopened = rustix::fs::open(
                &self.parent_path,
                OFlags::RDONLY | OFlags::DIRECTORY | OFlags::CLOEXEC | OFlags::NOFOLLOW,
                Mode::empty(),
            )
            .map_err(|error| {
                ReleaseError::IoError(
                    self.parent_path.clone(),
                    format!(
                        "release output parent path no longer opens as the anchored directory: {}",
                        io::Error::from(error)
                    ),
                )
            })?;
            let stat = rustix::fs::fstat(&reopened).map_err(|error| {
                ReleaseError::IoError(self.parent_path.clone(), io::Error::from(error).to_string())
            })?;
            if normalized_device(stat.st_dev) != self.parent_device
                || stat.st_ino != self.parent_inode
            {
                return Err(ReleaseError::IoError(
                    self.parent_path.clone(),
                    "release output parent path was rebound to a different directory".to_string(),
                ));
            }
            Ok(())
        }

        fn validate_stage_name_identity(&self) -> ReleaseResult<()> {
            validate_named_directory_identity(
                &self.parent_fd,
                self.stage_name.as_str(),
                self.stage_device,
                self.stage_inode,
                &self.path(),
                "release staging directory name",
            )
        }

        fn validate_final_name_identity(&self) -> ReleaseResult<()> {
            validate_named_directory_identity(
                &self.parent_fd,
                &self.final_name,
                self.stage_device,
                self.stage_inode,
                &self.final_path,
                "published release directory name",
            )
        }

        pub(crate) fn seal(&mut self) -> ReleaseResult<()> {
            if self.state != PendingState::Open {
                return Err(ReleaseError::IoError(
                    self.path(),
                    "release staging directory has already been sealed".to_string(),
                ));
            }
            self.validate_parent_path_identity()?;
            self.validate_stage_name_identity()?;
            self.revalidate_recorded_files()?;
            sync_directory_durably(&self.stage_fd)
                .map_err(|error| ReleaseError::IoError(self.path(), error.to_string()))?;
            self.validate_stage_name_identity()?;
            self.validate_parent_path_identity()?;
            self.state = PendingState::Sealed;
            Ok(())
        }

        pub(crate) fn publish(mut self) -> ReleaseResult<()> {
            if self.state != PendingState::Sealed {
                return Err(ReleaseError::IoError(
                    self.path(),
                    "release staging directory must be sealed before publication".to_string(),
                ));
            }
            self.validate_parent_path_identity()?;
            self.validate_stage_name_identity()?;
            self.revalidate_recorded_files()?;
            let rename_result = rename_noreplace(
                &self.parent_fd,
                self.stage_name.as_str(),
                &self.final_name,
                &self.final_path,
            );
            if let Err(error) = rename_result {
                if self.rename_error_left_published_output() {
                    self.state = PendingState::Published;
                    return Err(ReleaseError::PublishedButDurabilityUnconfirmed {
                        path: self.final_path.clone(),
                        message: format!(
                            "the filesystem reported a rename error after the staged directory reached the final name: {error}"
                        ),
                    });
                }
                return Err(error);
            }
            self.state = PendingState::Published;
            run_after_rename_hook();
            let mut issues = BTreeSet::new();
            if let Err(error) = self.validate_final_name_identity() {
                issues.insert(error.to_string());
            }
            if let Err(error) = self.validate_parent_path_identity() {
                issues.insert(error.to_string());
            }
            if let Err(error) = self.revalidate_recorded_files() {
                issues.insert(error.to_string());
            }
            if let Err(error) = injected_parent_sync_failure()
                .and_then(|()| sync_directory_durably(&self.parent_fd))
            {
                issues.insert(error.to_string());
            }
            if let Err(error) = self.validate_parent_path_identity() {
                issues.insert(error.to_string());
            }
            if let Err(error) = self.validate_final_name_identity() {
                issues.insert(error.to_string());
            }
            if !issues.is_empty() {
                return Err(ReleaseError::PublishedButDurabilityUnconfirmed {
                    path: self.final_path.clone(),
                    message: issues.into_iter().collect::<Vec<_>>().join("; "),
                });
            }
            Ok(())
        }

        fn rename_error_left_published_output(&self) -> bool {
            let source_absent = matches!(
                rustix::fs::statat(
                    &self.parent_fd,
                    self.stage_name.as_str(),
                    AtFlags::SYMLINK_NOFOLLOW,
                ),
                Err(Errno::NOENT)
            );
            if !source_absent {
                return false;
            }
            rustix::fs::statat(&self.parent_fd, &self.final_name, AtFlags::SYMLINK_NOFOLLOW)
                .is_ok_and(|stat| {
                    normalized_device(stat.st_dev) == self.stage_device
                        && stat.st_ino == self.stage_inode
                })
        }
    }

    impl Drop for PendingOutputDir {
        fn drop(&mut self) {
            if self.state == PendingState::Published {
                return;
            }
            for created in self.created_files.iter().rev() {
                let still_owned = rustix::fs::statat(
                    &self.stage_fd,
                    created.name.as_str(),
                    AtFlags::SYMLINK_NOFOLLOW,
                )
                .is_ok_and(|stat| {
                    FileType::from_raw_mode(stat.st_mode) == FileType::RegularFile
                        && normalized_device(stat.st_dev) == created.device
                        && stat.st_ino == created.inode
                });
                if still_owned {
                    let _ = rustix::fs::unlinkat(
                        &self.stage_fd,
                        created.name.as_str(),
                        AtFlags::empty(),
                    );
                }
            }
            let stage_name_still_owned = rustix::fs::statat(
                &self.parent_fd,
                self.stage_name.as_str(),
                AtFlags::SYMLINK_NOFOLLOW,
            )
            .is_ok_and(|stat| {
                FileType::from_raw_mode(stat.st_mode) == FileType::Directory
                    && normalized_device(stat.st_dev) == self.stage_device
                    && stat.st_ino == self.stage_inode
            });
            if stage_name_still_owned {
                let _ = rustix::fs::unlinkat(
                    &self.parent_fd,
                    self.stage_name.as_str(),
                    AtFlags::REMOVEDIR,
                );
            }
        }
    }

    fn digest_bytes(bytes: &[u8]) -> String {
        let mut hasher = Sha256::new();
        hasher.update(bytes);
        format!("sha256:{:x}", hasher.finalize())
    }

    fn digest_open_file(
        file: &mut File,
        path: &Path,
        max_bytes: u64,
        expected_size: u64,
    ) -> ReleaseResult<String> {
        let mut hasher = Sha256::new();
        let mut total = 0_u64;
        let mut buffer = [0_u8; 64 * 1024];
        loop {
            let read = file
                .read(&mut buffer)
                .map_err(|error| ReleaseError::IoError(path.to_path_buf(), error.to_string()))?;
            if read == 0 {
                break;
            }
            total = total
                .checked_add(u64::try_from(read).map_err(|_| {
                    ReleaseError::IoError(
                        path.to_path_buf(),
                        "staged release asset read size exceeds u64".to_string(),
                    )
                })?)
                .ok_or_else(|| {
                    ReleaseError::IoError(
                        path.to_path_buf(),
                        "staged release asset size overflow".to_string(),
                    )
                })?;
            if total > max_bytes {
                return Err(ReleaseError::IoError(
                    path.to_path_buf(),
                    format!("staged release asset exceeds the {max_bytes}-byte limit"),
                ));
            }
            hasher.update(&buffer[..read]);
        }
        if total != expected_size {
            return Err(ReleaseError::IoError(
                path.to_path_buf(),
                "staged release asset changed while being verified".to_string(),
            ));
        }
        Ok(format!("sha256:{:x}", hasher.finalize()))
    }

    /// Open one release-artifact input through an anchored directory chain.
    ///
    /// A privileged root-level alias is resolved once so operating-system
    /// paths such as macOS `/var -> /private/var` remain usable. Relative paths
    /// are anchored directly at an open handle for the current directory.
    /// Every later parent component and the final file are opened with
    /// `NOFOLLOW`. Once the file descriptor is open, later path renames cannot
    /// redirect the staged bytes.
    fn open_regular_source_no_follow(path: &Path) -> io::Result<File> {
        let file_name = path.file_name().ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidInput,
                "release artifact input path must name a file",
            )
        })?;
        let parent = path
            .parent()
            .filter(|parent| !parent.as_os_str().is_empty())
            .unwrap_or_else(|| Path::new("."));
        let mut remaining = parent.components();
        let mut directory = if parent.is_absolute() {
            if !matches!(remaining.next(), Some(Component::RootDir)) {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "absolute artifact parent has no root component",
                ));
            }
            let root = rustix::fs::open(
                "/",
                OFlags::RDONLY | OFlags::DIRECTORY | OFlags::CLOEXEC | OFlags::NOFOLLOW,
                Mode::empty(),
            )
            .map_err(io::Error::from)?;
            match remaining.next() {
                Some(Component::Normal(first)) => {
                    let first_stat = rustix::fs::statat(&root, first, AtFlags::SYMLINK_NOFOLLOW)
                        .map_err(io::Error::from)?;
                    if FileType::from_raw_mode(first_stat.st_mode) == FileType::Symlink {
                        if first_stat.st_uid != 0 {
                            return Err(io::Error::new(
                                io::ErrorKind::InvalidInput,
                                "artifact parent begins with an untrusted root-level symbolic link",
                            ));
                        }
                        open_root_alias_target(root, first)?
                    } else {
                        rustix::fs::openat(
                            &root,
                            first,
                            OFlags::RDONLY | OFlags::DIRECTORY | OFlags::CLOEXEC | OFlags::NOFOLLOW,
                            Mode::empty(),
                        )
                        .map_err(io::Error::from)?
                    }
                }
                None => root,
                _ => {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidInput,
                        "artifact parent contains an unexpected root component",
                    ));
                }
            }
        } else {
            rustix::fs::open(
                ".",
                OFlags::RDONLY | OFlags::DIRECTORY | OFlags::CLOEXEC | OFlags::NOFOLLOW,
                Mode::empty(),
            )
            .map_err(io::Error::from)?
        };
        for component in remaining {
            match component {
                Component::CurDir => continue,
                Component::Normal(name) => {
                    directory = rustix::fs::openat(
                        &directory,
                        name,
                        OFlags::RDONLY | OFlags::DIRECTORY | OFlags::CLOEXEC | OFlags::NOFOLLOW,
                        Mode::empty(),
                    )
                    .map_err(io::Error::from)?;
                }
                _ => {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidInput,
                        "artifact parent must not contain '..' or another root component",
                    ));
                }
            }
        }

        let opened = match rustix::fs::openat(
            &directory,
            file_name,
            OFlags::RDONLY | OFlags::CLOEXEC | OFlags::NOFOLLOW,
            Mode::empty(),
        ) {
            Ok(opened) => opened,
            Err(Errno::LOOP) => {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "release artifact input must be a regular file, not a symbolic link",
                ));
            }
            Err(error) => return Err(io::Error::from(error)),
        };
        let stat = rustix::fs::fstat(&opened).map_err(io::Error::from)?;
        if FileType::from_raw_mode(stat.st_mode) != FileType::RegularFile {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "release artifact input must be a regular file, not a symbolic link",
            ));
        }
        Ok(File::from(opened))
    }

    /// Resolve one trusted root-level alias lexically, then open every target
    /// component relative to the already-open root with `NOFOLLOW`. Passing the
    /// symlink itself to `openat` would allow the kernel to follow additional
    /// aliases embedded in its target.
    fn open_root_alias_target(root: OwnedFd, alias: &OsStr) -> io::Result<OwnedFd> {
        let target = rustix::fs::readlinkat(&root, alias, Vec::new()).map_err(io::Error::from)?;
        let target = PathBuf::from(OsString::from_vec(target.into_bytes()));
        open_lexical_directory_from_anchor(root, &target)
    }

    fn open_lexical_directory_from_anchor(
        mut directory: OwnedFd,
        target: &Path,
    ) -> io::Result<OwnedFd> {
        let mut components = Vec::<OsString>::new();
        for component in target.components() {
            match component {
                Component::RootDir | Component::CurDir => {}
                Component::Normal(name) => components.push(name.to_os_string()),
                Component::ParentDir => {
                    if components.pop().is_none() {
                        return Err(io::Error::new(
                            io::ErrorKind::InvalidInput,
                            "root-level artifact alias escapes the filesystem root",
                        ));
                    }
                }
                Component::Prefix(_) => {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidInput,
                        "root-level artifact alias has an unsupported target prefix",
                    ));
                }
            }
        }
        for component in components {
            directory = rustix::fs::openat(
                &directory,
                component.as_os_str(),
                OFlags::RDONLY | OFlags::DIRECTORY | OFlags::CLOEXEC | OFlags::NOFOLLOW,
                Mode::empty(),
            )
            .map_err(io::Error::from)?;
        }
        Ok(directory)
    }

    #[cfg(test)]
    pub(crate) fn test_open_alias_target_no_follow(anchor: &Path, alias: &OsStr) -> io::Result<()> {
        let root = rustix::fs::open(
            anchor,
            OFlags::RDONLY | OFlags::DIRECTORY | OFlags::CLOEXEC | OFlags::NOFOLLOW,
            Mode::empty(),
        )
        .map_err(io::Error::from)?;
        open_root_alias_target(root, alias).map(drop)
    }

    fn same_source_metadata(before: &std::fs::Metadata, after: &std::fs::Metadata) -> bool {
        before.dev() == after.dev()
            && before.ino() == after.ino()
            && before.mode() == after.mode()
            && before.nlink() == after.nlink()
            && before.size() == after.size()
            && before.mtime() == after.mtime()
            && before.mtime_nsec() == after.mtime_nsec()
            && before.ctime() == after.ctime()
            && before.ctime_nsec() == after.ctime_nsec()
    }

    fn validate_named_directory_identity(
        parent: &OwnedFd,
        name: impl rustix::path::Arg,
        expected_device: i128,
        expected_inode: u64,
        path: &Path,
        context: &str,
    ) -> ReleaseResult<()> {
        let stat =
            rustix::fs::statat(parent, name, AtFlags::SYMLINK_NOFOLLOW).map_err(|error| {
                ReleaseError::IoError(path.to_path_buf(), io::Error::from(error).to_string())
            })?;
        if FileType::from_raw_mode(stat.st_mode) != FileType::Directory
            || normalized_device(stat.st_dev) != expected_device
            || stat.st_ino != expected_inode
        {
            return Err(ReleaseError::IoError(
                path.to_path_buf(),
                format!("{context} no longer identifies the anchored staging directory"),
            ));
        }
        Ok(())
    }

    fn ensure_absent(parent: &OwnedFd, name: &OsStr, path: &Path) -> ReleaseResult<()> {
        match rustix::fs::statat(parent, name, AtFlags::SYMLINK_NOFOLLOW) {
            Err(Errno::NOENT) => Ok(()),
            Ok(_) => Err(ReleaseError::IoError(
                path.to_path_buf(),
                "release output path must not already exist".to_string(),
            )),
            Err(error) => Err(ReleaseError::IoError(
                path.to_path_buf(),
                io::Error::from(error).to_string(),
            )),
        }
    }

    fn sync_regular_file_durably<Fd: std::os::fd::AsFd>(fd: Fd) -> io::Result<()> {
        rustix::fs::fsync(&fd).map_err(io::Error::from)?;
        #[cfg(target_vendor = "apple")]
        rustix::fs::fcntl_fullfsync(&fd).map_err(io::Error::from)?;
        Ok(())
    }

    fn sync_directory_durably<Fd: std::os::fd::AsFd>(fd: Fd) -> io::Result<()> {
        rustix::fs::fsync(&fd).map_err(io::Error::from)
    }

    fn rename_noreplace(
        parent: &OwnedFd,
        from: &str,
        to: &OsStr,
        final_path: &Path,
    ) -> ReleaseResult<()> {
        #[cfg(any(target_os = "linux", target_os = "android", target_vendor = "apple"))]
        {
            rustix::fs::renameat_with(parent, from, parent, to, rustix::fs::RenameFlags::NOREPLACE)
                .map_err(|error| {
                    if matches!(error, Errno::NOSYS | Errno::NOTSUP | Errno::INVAL) {
                        ReleaseError::AtomicPublishUnsupported {
                            path: final_path.to_path_buf(),
                            message: format!(
                            "the filesystem rejected atomic no-replace directory publication: {}",
                            io::Error::from(error)
                        ),
                        }
                    } else {
                        ReleaseError::IoError(
                            final_path.to_path_buf(),
                            io::Error::from(error).to_string(),
                        )
                    }
                })
        }
        #[cfg(not(any(target_os = "linux", target_os = "android", target_vendor = "apple")))]
        Err(ReleaseError::AtomicPublishUnsupported {
            path: final_path.to_path_buf(),
            message: "this Unix target has no atomic no-replace directory rename primitive"
                .to_string(),
        })
    }
}

#[cfg(not(unix))]
mod imp {
    use std::io::Write;
    use std::path::{Path, PathBuf};

    use super::{BTreeSet, ReleaseError, ReleaseResult};

    pub(crate) struct PendingOutputDir;

    impl PendingOutputDir {
        pub(crate) fn create(final_dir: &Path) -> ReleaseResult<Self> {
            Err(ReleaseError::AtomicPublishUnsupported {
                path: final_dir.to_path_buf(),
                message: unsupported_platform_message().to_string(),
            })
        }

        pub(crate) fn path(&self) -> PathBuf {
            PathBuf::new()
        }

        pub(crate) fn write_new_file(&mut self, _name: &str, _bytes: &[u8]) -> ReleaseResult<()> {
            Err(unsupported_operation_error())
        }

        pub(crate) fn write_new_file_streaming(
            &mut self,
            _name: &str,
            _max_bytes: u64,
            _write: impl FnOnce(&mut dyn Write) -> ReleaseResult<()>,
        ) -> ReleaseResult<(u64, String)> {
            Err(unsupported_operation_error())
        }

        pub(crate) fn copy_new_file(
            &mut self,
            _name: &str,
            _source_path: &Path,
            _max_bytes: u64,
        ) -> ReleaseResult<(u64, String)> {
            Err(unsupported_operation_error())
        }

        pub(crate) fn entry_names(&self) -> ReleaseResult<BTreeSet<String>> {
            Err(unsupported_operation_error())
        }

        pub(crate) fn read_file(&self, _name: &str, _max: usize) -> ReleaseResult<Vec<u8>> {
            Err(unsupported_operation_error())
        }

        pub(crate) fn validate_file_digest(
            &self,
            _name: &str,
            _max_bytes: u64,
            _expected_size: u64,
            _expected_digest: &str,
        ) -> ReleaseResult<()> {
            Err(unsupported_operation_error())
        }

        pub(crate) fn with_file_reader<T>(
            &self,
            _name: &str,
            _max_bytes: u64,
            _expected_size: u64,
            _read: impl FnOnce(&mut std::fs::File) -> ReleaseResult<T>,
        ) -> ReleaseResult<T> {
            Err(unsupported_operation_error())
        }

        pub(crate) fn seal(&mut self) -> ReleaseResult<()> {
            Err(unsupported_operation_error())
        }

        pub(crate) fn publish(self) -> ReleaseResult<()> {
            Err(unsupported_operation_error())
        }
    }

    fn unsupported_operation_error() -> ReleaseError {
        ReleaseError::AtomicPublishUnsupported {
            path: PathBuf::new(),
            message: unsupported_platform_message().to_string(),
        }
    }

    #[cfg(windows)]
    fn unsupported_platform_message() -> &'static str {
        "Windows release publication is disabled because this implementation cannot yet prove that create, verify, no-replace rename, and directory flush all address the same anchored directory handles"
    }

    #[cfg(not(windows))]
    fn unsupported_platform_message() -> &'static str {
        "this platform has no supported atomic durable directory publication primitive"
    }
}

#[cfg(all(test, unix))]
pub(crate) use imp::test_open_alias_target_no_follow;
pub(crate) use imp::PendingOutputDir;
