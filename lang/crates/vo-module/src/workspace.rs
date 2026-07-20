use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

use vo_common::stable_hash::StableHasher;
#[cfg(test)]
use vo_common::vfs::RealFs;
use vo_common::vfs::{
    normalize_ancestor_discovery_start, normalize_fs_path, sort_fs_paths, FileSystem,
    FileSystemEntryKind, MAX_DIRECTORY_ENTRIES,
};

use crate::identity::{classify_import, ImportClass, ModIdentity, ModulePath};
use crate::schema::modfile::ModFile;
use crate::schema::workfile::WorkFile;
use crate::Error;

/// A resolved workspace member: canonical module path mapped to its local directory.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WorkspaceMember {
    pub module: ModulePath,
    pub local_dir: PathBuf,
    namespace_anchor: PathBuf,
    directory_identity: WorkspaceDirectoryIdentity,
    mod_file: ModFile,
    manifest_content: String,
    manifest_identity: StableRegularFileIdentity,
}

impl WorkspaceMember {
    /// Stable, opaque identity for the exact directory generation validated
    /// while this member was discovered.
    pub(crate) fn directory_generation_key(&self) -> String {
        self.directory_identity.generation_key()
    }

    /// Parsed member declaration captured from the same stable manifest
    /// generation that established this workspace member's identity.
    pub fn mod_file(&self) -> &ModFile {
        &self.mod_file
    }

    pub(crate) fn manifest_generation_key(&self) -> String {
        let mut hasher = StableHasher::new("vo-workspace-member-manifest-generation-v2");
        hasher.update_path("path", &normalize_fs_path(&self.local_dir.join("vo.mod")));
        hasher.update_str("content", &self.manifest_content);
        self.manifest_identity.update_generation_hash(&mut hasher);
        hasher.finish()
    }

    /// Revalidate the exact directory generation captured during discovery.
    ///
    /// Callers use this before granting source authority and after scanning
    /// sources so a same-path directory replacement cannot combine two
    /// workspace generations in one project context.
    pub(crate) fn validate_directory_generation<F: FileSystem>(&self, fs: &F) -> Result<(), Error> {
        require_workspace_directory_generation(
            fs,
            &self.namespace_anchor,
            &self.local_dir,
            &format!("workspace member {} directory", self.module),
            &self.directory_identity,
        )
    }

    pub(crate) fn validate_generation<F: FileSystem>(&self, fs: &F) -> Result<(), Error> {
        self.validate_directory_generation(fs)?;
        let path = self.local_dir.join("vo.mod");
        let (content, identity) = read_stable_regular_text_file_generation(
            fs,
            &path,
            vo_common::vfs::MAX_TEXT_FILE_BYTES,
            "workspace member vo.mod",
        )?;
        if content != self.manifest_content || identity != self.manifest_identity {
            return Err(stable_regular_file_generation_changed(
                &path,
                "workspace member vo.mod",
            ));
        }
        self.validate_directory_generation(fs)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub enum WorkspaceDiscovery {
    #[default]
    Auto,
    Disabled,
    Explicit(PathBuf),
}

/// Resolves the process workspace policy.
///
/// An unset `VOWORK` selects nearest-ancestor discovery, `VOWORK=off`
/// disables workspace use, and every other value explicitly selects that
/// workspace file. Relative explicit paths are resolved against the root
/// project directory by [`discover_workfile_in_with`].
pub fn workspace_discovery_from_environment() -> WorkspaceDiscovery {
    match std::env::var_os("VOWORK") {
        None => WorkspaceDiscovery::Auto,
        Some(value) if value == "off" => WorkspaceDiscovery::Disabled,
        Some(value) => WorkspaceDiscovery::Explicit(PathBuf::from(value)),
    }
}

pub fn discover_workfile_in_with<F: FileSystem>(
    fs: &F,
    project_dir: &Path,
    discovery: &WorkspaceDiscovery,
) -> Result<Option<PathBuf>, Error> {
    let selected = match discovery {
        WorkspaceDiscovery::Disabled => None,
        WorkspaceDiscovery::Auto => discover_nearest_workfile_in(fs, project_dir)?,
        WorkspaceDiscovery::Explicit(path) => {
            let workfile_path = if path.is_absolute() {
                normalize_fs_path(path)
            } else {
                normalize_fs_path(&project_dir.join(path))
            };
            reject_workfile_portable_aliases(
                fs,
                workfile_path.parent().unwrap_or_else(|| Path::new(".")),
            )?;
            match fs.entry_kind(&workfile_path).map_err(|error| {
                source_read_error(
                    error.kind(),
                    format!(
                        "cannot inspect explicit workspace file {}: {error}",
                        workfile_path.display()
                    ),
                )
            })? {
                FileSystemEntryKind::RegularFile => {}
                FileSystemEntryKind::Missing => {
                    return Err(Error::WorkFileParse(format!(
                        "explicit workspace file does not exist: {}",
                        workfile_path.display()
                    )))
                }
                found => {
                    return Err(Error::WorkFileParse(format!(
                        "explicit workspace file {} must be RegularFile without symbolic links or reparse points; found {found:?}",
                        workfile_path.display()
                    )))
                }
            }
            Some(workfile_path)
        }
    };
    if let Some(workfile_path) = selected.as_deref() {
        stable_regular_file_identity(fs, workfile_path, "workspace file")?;
    }
    Ok(selected)
}

fn discover_nearest_workfile_in<F: FileSystem>(
    fs: &F,
    project_dir: &Path,
) -> Result<Option<PathBuf>, Error> {
    let Some(mut dir) = normalize_ancestor_discovery_start(fs, project_dir).map_err(|error| {
        source_read_error(
            error.kind(),
            format!(
                "cannot normalize workspace discovery start {}: {error}",
                project_dir.display()
            ),
        )
    })?
    else {
        return Ok(None);
    };
    loop {
        reject_workfile_portable_aliases(fs, &dir)?;
        let candidate = dir.join("vo.work");
        match fs.entry_kind(&candidate).map_err(|error| {
            source_read_error(
                error.kind(),
                format!(
                    "cannot inspect workspace candidate {}: {error}",
                    candidate.display()
                ),
            )
        })? {
            FileSystemEntryKind::RegularFile => return Ok(Some(candidate)),
            FileSystemEntryKind::Missing => {}
            found => {
                return Err(Error::WorkFileParse(format!(
                    "workspace candidate {} must be RegularFile without symbolic links or reparse points; found {found:?}",
                    candidate.display()
                )))
            }
        }
        if !dir.pop() {
            return Ok(None);
        }
    }
}

fn reject_workfile_portable_aliases<F: FileSystem>(fs: &F, directory: &Path) -> Result<(), Error> {
    let entries =
        crate::schema::read_canonical_directory_entries(fs, directory).map_err(|error| {
            source_read_error(
                error.kind(),
                format!(
                    "cannot enumerate workspace directory {} for portable vo.work aliases: {error}",
                    directory.display(),
                ),
            )
        })?;
    if let Some((alias, canonical)) =
        crate::schema::first_portable_name_alias(&entries, &["vo.work"])
    {
        return Err(Error::WorkFileParse(format!(
            "workspace directory {} contains portable alias {} for canonical protocol file {canonical}",
            directory.display(),
            alias.display(),
        )));
    }
    Ok(())
}

#[cfg(test)]
fn discover_workspace_candidates_in_with<F: FileSystem>(
    fs: &F,
    project_dir: &Path,
    root_module: Option<&ModIdentity>,
    discovery: &WorkspaceDiscovery,
) -> Result<Vec<WorkspaceMember>, Error> {
    discover_workspace_candidates_in_with_provenance(fs, project_dir, root_module, discovery)
        .map(|(_, candidates)| candidates)
}

/// Discover workspace candidates together with the exact `vo.work` selected
/// by the configured discovery policy.
///
/// Returning discovery provenance from the same operation that parsed the
/// workspace prevents callers from observing a different workspace
/// generation through a second filesystem lookup. The returned entries carry
/// no dependency authority: callers must pass them through `ProjectContext`
/// before using any member as an effective workspace source.
pub fn discover_workspace_candidates_in_with_provenance<F: FileSystem>(
    fs: &F,
    project_dir: &Path,
    root_module: Option<&ModIdentity>,
    discovery: &WorkspaceDiscovery,
) -> Result<(Option<PathBuf>, Vec<WorkspaceMember>), Error> {
    let (workfile_generation, members) =
        discover_workspace_candidates_in_with_generation(fs, project_dir, root_module, discovery)?;
    Ok((
        workfile_generation.map(|generation| generation.path().to_path_buf()),
        members,
    ))
}

pub(crate) fn discover_workspace_candidates_in_with_generation<F: FileSystem>(
    fs: &F,
    project_dir: &Path,
    root_module: Option<&ModIdentity>,
    discovery: &WorkspaceDiscovery,
) -> Result<(Option<SelectedWorkfileGeneration>, Vec<WorkspaceMember>), Error> {
    let (workfile_generation, members) =
        load_workspace_members_in_with_generation(fs, project_dir, discovery)?;
    let normalized_project_dir = normalize_fs_path(project_dir);
    let inferred_root_module;
    let root_module = match root_module {
        Some(root_module) => Some(root_module),
        None if members
            .iter()
            .any(|member| member.local_dir == normalized_project_dir) =>
        {
            None
        }
        None if workfile_generation.is_some() => {
            inferred_root_module = read_active_module_in(fs, project_dir)?;
            inferred_root_module.as_ref()
        }
        None => None,
    };
    let members = exclude_active_module(fs, members, project_dir, root_module)?;
    Ok((workfile_generation, members))
}

/// Load and validate the complete member set together with the exact selected
/// `vo.work`. Member identities always come from their own `vo.mod` files.
pub fn load_workspace_members_in_with_provenance<F: FileSystem>(
    fs: &F,
    project_dir: &Path,
    discovery: &WorkspaceDiscovery,
) -> Result<(Option<PathBuf>, Vec<WorkspaceMember>), Error> {
    let (generation, members) =
        load_workspace_members_in_with_generation(fs, project_dir, discovery)?;
    Ok((
        generation.map(|generation| generation.path().to_path_buf()),
        members,
    ))
}

fn load_workspace_members_in_with_generation<F: FileSystem>(
    fs: &F,
    project_dir: &Path,
    discovery: &WorkspaceDiscovery,
) -> Result<(Option<SelectedWorkfileGeneration>, Vec<WorkspaceMember>), Error> {
    let Some(workfile_path) = discover_workfile_in_with(fs, project_dir, discovery)? else {
        return Ok((None, Vec::new()));
    };
    let (generation, members) = load_workspace_members_from_file(fs, project_dir, &workfile_path)?;
    let active_root = normalize_fs_path(project_dir);
    if members.iter().any(|member| member.local_dir == active_root) {
        return Ok((Some(generation), members));
    }
    match discovery {
        WorkspaceDiscovery::Auto => Ok((None, Vec::new())),
        WorkspaceDiscovery::Explicit(_) => Err(Error::WorkFileParse(format!(
            "explicit workspace {} does not list active root {}",
            workfile_path.display(),
            active_root.display(),
        ))),
        WorkspaceDiscovery::Disabled => Ok((None, Vec::new())),
    }
}

fn load_workspace_members_from_file<F: FileSystem>(
    fs: &F,
    project_dir: &Path,
    workfile_path: &Path,
) -> Result<(SelectedWorkfileGeneration, Vec<WorkspaceMember>), Error> {
    let workfile_generation = SelectedWorkfileGeneration::read(fs, workfile_path)?;
    let content = workfile_generation.content();
    let namespace_anchor = filesystem_namespace_anchor(workfile_path, "workspace file")?;
    let workfile_dir = workfile_path.parent().unwrap_or(project_dir);
    let workfile = WorkFile::parse(content).map_err(|e| {
        Error::WorkFileParse(format!("error parsing {}: {e}", workfile_path.display()))
    })?;
    let mut members = Vec::new();
    members
        .try_reserve(workfile.members.len())
        .map_err(|_| Error::WorkFileParse("cannot allocate resolved workspace members".into()))?;
    let mut seen_directories = BTreeMap::new();
    let mut seen_modules = BTreeSet::new();
    let mut manifest_bytes = 0usize;

    for (index, authored_path) in workfile.members.iter().enumerate() {
        let local_dir = resolve_path(workfile_dir, authored_path);
        let directory_identity =
            workspace_member_directory_identity(fs, &namespace_anchor, &local_dir, index)?;
        if let Some((first_index, first_path)) =
            seen_directories.insert(directory_identity.clone(), (index, local_dir.clone()))
        {
            return Err(Error::WorkFileParse(format!(
                "members[{index}]: duplicate resolved member directory {}; it identifies the same real directory as members[{first_index}] {}",
                local_dir.display(),
                first_path.display(),
            )));
        }

        let mod_path = local_dir.join("vo.mod");
        let (content, manifest_identity) = read_stable_regular_text_file_generation(
            fs,
            &mod_path,
            vo_common::vfs::MAX_TEXT_FILE_BYTES,
            &format!("members[{index}] vo.mod"),
        )?;
        charge_workspace_manifest_bytes(
            &mut manifest_bytes,
            content.len(),
            crate::MAX_SOLVER_MANIFEST_BYTES,
        )?;
        let mf = ModFile::parse(&content).map_err(|e| {
            Error::WorkFileParse(format!(
                "members[{index}]: error parsing {}: {e}",
                mod_path.display()
            ))
        })?;
        require_workspace_member_generation(
            fs,
            &namespace_anchor,
            &local_dir,
            index,
            &directory_identity,
        )?;
        let module = ModulePath::parse(mf.module.as_str()).map_err(|error| {
            Error::WorkFileParse(format!(
                "members[{index}]: invalid ModuleId in {}: {error}",
                local_dir.display(),
            ))
        })?;
        if !seen_modules.insert(module.as_str().to_string()) {
            return Err(Error::WorkFileParse(format!(
                "members[{index}]: duplicate workspace module {module}"
            )));
        }
        members.push(WorkspaceMember {
            module,
            local_dir,
            namespace_anchor: namespace_anchor.clone(),
            directory_identity,
            mod_file: mf,
            manifest_content: content,
            manifest_identity,
        });
    }

    for member in &members {
        member.validate_generation(fs)?;
    }
    workfile_generation.validate(fs)?;
    Ok((workfile_generation, members))
}

fn charge_workspace_manifest_bytes(
    total: &mut usize,
    bytes: usize,
    limit: usize,
) -> Result<(), Error> {
    let next = total.checked_add(bytes).ok_or_else(|| {
        Error::WorkFileParse("workspace member manifest byte count overflow".to_string())
    })?;
    if next > limit {
        return Err(Error::WorkFileParse(format!(
            "workspace member vo.mod files exceed the aggregate {limit}-byte manifest limit"
        )));
    }
    *total = next;
    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg(any(all(unix, not(target_arch = "wasm32")), windows))]
struct HostEntryIdentity {
    volume: u64,
    file: [u8; 16],
}

#[cfg(all(unix, not(target_arch = "wasm32")))]
fn unix_file_id(value: u64) -> [u8; 16] {
    let mut file = [0; 16];
    file[..8].copy_from_slice(&value.to_le_bytes());
    file
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg(any(all(unix, not(target_arch = "wasm32")), windows))]
struct HostFileMetadataSnapshot {
    identity: HostEntryIdentity,
    len: u64,
    #[cfg(all(unix, not(target_arch = "wasm32")))]
    mode: u32,
    #[cfg(all(unix, not(target_arch = "wasm32")))]
    links: u64,
    #[cfg(all(unix, not(target_arch = "wasm32")))]
    modified_seconds: i64,
    #[cfg(all(unix, not(target_arch = "wasm32")))]
    modified_nanoseconds: i64,
    #[cfg(all(unix, not(target_arch = "wasm32")))]
    changed_seconds: i64,
    #[cfg(all(unix, not(target_arch = "wasm32")))]
    changed_nanoseconds: i64,
    #[cfg(windows)]
    attributes: u32,
    #[cfg(windows)]
    creation_time: i64,
    #[cfg(windows)]
    last_write_time: i64,
    #[cfg(windows)]
    change_time: i64,
    #[cfg(windows)]
    links: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg(any(all(unix, not(target_arch = "wasm32")), windows))]
struct HostDirectoryMetadataSnapshot {
    identity: HostEntryIdentity,
}

#[cfg(any(all(unix, not(target_arch = "wasm32")), windows))]
struct OpenedHostRegularFile {
    file: std::fs::File,
    parent_directory: std::fs::File,
    identity: StableRegularFileIdentity,
    metadata: HostFileMetadataSnapshot,
    parent_metadata: HostDirectoryMetadataSnapshot,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum StableRegularFileIdentity {
    #[cfg(any(all(unix, not(target_arch = "wasm32")), windows))]
    Host(Vec<HostEntryIdentity>),
    Virtual(PathBuf),
}

impl StableRegularFileIdentity {
    fn update_generation_hash(&self, hasher: &mut StableHasher) {
        match self {
            #[cfg(any(all(unix, not(target_arch = "wasm32")), windows))]
            Self::Host(entries) => {
                hasher.update_str("identity-kind", "host");
                let mut encoded = Vec::with_capacity(entries.len().saturating_mul(24));
                for entry in entries {
                    encoded.extend_from_slice(&entry.volume.to_le_bytes());
                    encoded.extend_from_slice(&entry.file);
                }
                hasher.update_bytes("identity-chain", &encoded);
            }
            Self::Virtual(path) => {
                hasher.update_str("identity-kind", "virtual");
                hasher.update_path("identity-path", path);
            }
        }
    }
}

/// Return the trusted root of the VFS namespace containing `path`.
/// Relative paths are anchored at `.`. Absolute paths retain only their
/// platform prefix/root so every authored parent component remains subject to
/// no-follow validation.
fn filesystem_namespace_anchor(path: &Path, label: &str) -> Result<PathBuf, Error> {
    use std::path::Component;

    let path = normalize_fs_path(path);
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
    if saw_prefix && !saw_root {
        return Err(Error::WorkFileParse(format!(
            "{label} path {} uses a drive-relative namespace that cannot be validated safely",
            path.display()
        )));
    }
    if anchor.as_os_str().is_empty() {
        Ok(PathBuf::from("."))
    } else {
        Ok(anchor)
    }
}

fn normalize_host_stable_regular_file_path(path: &Path) -> PathBuf {
    let normalized = normalize_fs_path(path);
    // macOS exposes these fixed, operating-system-owned aliases into
    // `/private`. Normalize this closed set before the componentwise
    // no-follow walk; arbitrary links remain visible and fail closed.
    #[cfg(target_os = "macos")]
    let normalized = {
        let mut normalized = normalized;
        for (alias, target) in [
            (Path::new("/var"), Path::new("/private/var")),
            (Path::new("/tmp"), Path::new("/private/tmp")),
            (Path::new("/etc"), Path::new("/private/etc")),
        ] {
            if let Ok(suffix) = normalized.strip_prefix(alias) {
                if macos_system_alias_points_to(alias, target) {
                    normalized = target.join(suffix);
                    break;
                }
            }
        }
        normalized
    };
    normalized
}

#[cfg(target_os = "macos")]
fn macos_system_alias_points_to(alias: &Path, expected_target: &Path) -> bool {
    let Ok(metadata) = std::fs::symlink_metadata(alias) else {
        return false;
    };
    if !metadata.file_type().is_symlink() {
        return false;
    }
    let Ok(target) = std::fs::read_link(alias) else {
        return false;
    };
    let target = if target.is_absolute() {
        normalize_fs_path(&target)
    } else {
        normalize_fs_path(&alias.parent().unwrap_or(Path::new("/")).join(target))
    };
    if target != expected_target {
        return false;
    }
    std::fs::symlink_metadata(expected_target)
        .is_ok_and(|metadata| metadata.is_dir() && !metadata.file_type().is_symlink())
}

fn stable_regular_file_identity<F: FileSystem>(
    fs: &F,
    path: &Path,
    label: &str,
) -> Result<StableRegularFileIdentity, Error> {
    let path = normalize_fs_path(path);
    let Some((host_anchor, host_path)) =
        validated_stable_regular_file_host_paths(fs, &path, label)?
    else {
        return Ok(StableRegularFileIdentity::Virtual(path));
    };
    host_stable_regular_file_identity(&host_anchor, &host_path, label)
}

fn validated_stable_regular_file_host_paths<F: FileSystem>(
    fs: &F,
    path: &Path,
    label: &str,
) -> Result<Option<(PathBuf, PathBuf)>, Error> {
    let namespace_anchor = filesystem_namespace_anchor(path, label)?;
    let parent = path
        .parent()
        .map(normalize_fs_path)
        .unwrap_or_else(|| namespace_anchor.clone());
    let host_path = fs.resolve_host_path(path).map_err(|error| {
        source_read_error(
            error.kind(),
            format!(
                "cannot resolve host path for {label} {}: {error}",
                path.display()
            ),
        )
    })?;
    require_stable_regular_file_parent_components(
        fs,
        &namespace_anchor,
        &parent,
        &format!("{label} parent"),
        host_path.as_deref(),
    )?;
    require_entry_kind(fs, path, FileSystemEntryKind::RegularFile, label)?;

    let Some(host_path) = host_path else {
        return Ok(None);
    };
    let host_anchor = fs
        .resolve_host_path(&namespace_anchor)
        .map_err(|error| {
            source_read_error(
                error.kind(),
                format!(
                    "cannot resolve host namespace anchor {} for {label} {}: {error}",
                    namespace_anchor.display(),
                    path.display()
                ),
            )
        })?
        .ok_or_else(|| {
            Error::WorkFileParse(format!(
                "{label} {} has a host path while its namespace anchor {} does not",
                path.display(),
                namespace_anchor.display()
            ))
        })?;
    Ok(Some((
        normalize_host_stable_regular_file_path(&host_anchor),
        normalize_host_stable_regular_file_path(&host_path),
    )))
}

fn workspace_file_generation_changed(workfile_path: &Path) -> Error {
    Error::WorkFileParse(format!(
        "workspace file path {} changed identity or content while loading; refusing a mixed workspace generation",
        workfile_path.display()
    ))
}

/// Exact selected `vo.work` bytes and no-follow filesystem generation used to
/// discover workspace members. Project loading keeps this handle alive and
/// revalidates it after all authorization and source scanning completes.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct SelectedWorkfileGeneration {
    path: PathBuf,
    content: String,
    identity: StableRegularFileIdentity,
}

impl SelectedWorkfileGeneration {
    fn read<F: FileSystem>(fs: &F, path: &Path) -> Result<Self, Error> {
        let path = normalize_fs_path(path);
        let (content, identity) = read_workspace_file_generation(fs, &path)?;
        Ok(Self {
            path,
            content,
            identity,
        })
    }

    pub(crate) fn path(&self) -> &Path {
        &self.path
    }

    pub(crate) fn content(&self) -> &str {
        &self.content
    }

    pub(crate) fn generation_key(&self) -> String {
        let mut hasher = StableHasher::new("vo-selected-workfile-generation-v2");
        hasher.update_path("path", &self.path);
        hasher.update_str("content", &self.content);
        self.identity.update_generation_hash(&mut hasher);
        hasher.finish()
    }

    pub(crate) fn validate<F: FileSystem>(&self, fs: &F) -> Result<(), Error> {
        let (content, identity) = read_workspace_file_generation(fs, &self.path)?;
        if content != self.content || identity != self.identity {
            return Err(workspace_file_generation_changed(&self.path));
        }
        Ok(())
    }
}

fn read_workspace_file_generation<F: FileSystem>(
    fs: &F,
    workfile_path: &Path,
) -> Result<(String, StableRegularFileIdentity), Error> {
    read_stable_regular_text_file_generation(
        fs,
        workfile_path,
        vo_common::vfs::MAX_TEXT_FILE_BYTES,
        "workspace file",
    )
}

/// Read one regular UTF-8 text file from a stable, no-follow filesystem
/// generation. Every parent component and the leaf are validated before and
/// after the bounded read.
pub(crate) fn read_stable_regular_text_file<F: FileSystem>(
    fs: &F,
    path: &Path,
    max_bytes: usize,
    label: &str,
) -> Result<String, Error> {
    let bytes = read_stable_regular_file_bytes(fs, path, max_bytes, label)?;
    decode_stable_regular_text(bytes, path, label)
}

fn source_read_error(kind: std::io::ErrorKind, detail: impl Into<String>) -> Error {
    Error::SourceRead(std::io::Error::new(kind, detail.into()))
}

/// Read exact bytes from one stable regular-file generation. Host-backed
/// files are read through the same no-follow leaf descriptor that established
/// their identity; virtual files must return the same kind and bytes twice.
pub(crate) fn read_stable_regular_file_bytes<F: FileSystem>(
    fs: &F,
    path: &Path,
    max_bytes: usize,
    label: &str,
) -> Result<Vec<u8>, Error> {
    read_stable_regular_file_bytes_generation(fs, path, max_bytes, label).map(|(bytes, _)| bytes)
}

fn read_stable_regular_text_file_generation<F: FileSystem>(
    fs: &F,
    path: &Path,
    max_bytes: usize,
    label: &str,
) -> Result<(String, StableRegularFileIdentity), Error> {
    let (bytes, identity) = read_stable_regular_file_bytes_generation(fs, path, max_bytes, label)?;
    let content = decode_stable_regular_text(bytes, path, label)?;
    Ok((content, identity))
}

fn read_stable_regular_file_bytes_generation<F: FileSystem>(
    fs: &F,
    path: &Path,
    max_bytes: usize,
    label: &str,
) -> Result<(Vec<u8>, StableRegularFileIdentity), Error> {
    let path = normalize_fs_path(path);
    let Some((host_anchor, host_path)) =
        validated_stable_regular_file_host_paths(fs, &path, label)?
    else {
        let (bytes, identity) =
            read_virtual_stable_regular_file_bytes(fs, &path, max_bytes, label)?;
        let after = stable_regular_file_identity(fs, &path, label)?;
        if after != identity {
            return Err(stable_regular_file_generation_changed(&path, label));
        }
        return Ok((bytes, identity));
    };
    let (bytes, identity) =
        read_host_stable_regular_file_bytes(&host_anchor, &host_path, max_bytes, label)?;
    let after = stable_regular_file_identity(fs, &path, label)?;
    if after != identity {
        return Err(stable_regular_file_generation_changed(&path, label));
    }
    Ok((bytes, identity))
}

fn read_virtual_stable_regular_file_bytes<F: FileSystem>(
    fs: &F,
    path: &Path,
    max_bytes: usize,
    label: &str,
) -> Result<(Vec<u8>, StableRegularFileIdentity), Error> {
    require_entry_kind(fs, path, FileSystemEntryKind::RegularFile, label)?;
    let first = fs.read_bytes_limited(path, max_bytes).map_err(|error| {
        source_read_error(
            error.kind(),
            format!("cannot read {label} {}: {error}", path.display()),
        )
    })?;
    require_entry_kind(fs, path, FileSystemEntryKind::RegularFile, label)?;
    let second = fs.read_bytes_limited(path, max_bytes).map_err(|error| {
        source_read_error(
            error.kind(),
            format!("cannot reread {label} {}: {error}", path.display()),
        )
    })?;
    require_entry_kind(fs, path, FileSystemEntryKind::RegularFile, label)?;
    if first != second {
        return Err(stable_regular_file_generation_changed(path, label));
    }
    Ok((
        first,
        StableRegularFileIdentity::Virtual(path.to_path_buf()),
    ))
}

fn decode_stable_regular_text(bytes: Vec<u8>, path: &Path, label: &str) -> Result<String, Error> {
    String::from_utf8(bytes).map_err(|error| {
        source_read_error(
            std::io::ErrorKind::InvalidData,
            format!(
                "{label} {} is not valid UTF-8: {}",
                path.display(),
                error.utf8_error()
            ),
        )
    })
}

fn stable_regular_file_generation_changed(path: &Path, label: &str) -> Error {
    Error::WorkFileParse(format!(
        "{label} path {} changed identity or content while reading; refusing a mixed workspace generation",
        path.display()
    ))
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum WorkspaceDirectoryIdentity {
    #[cfg(any(all(unix, not(target_arch = "wasm32")), windows))]
    Host {
        volume: u64,
        file: [u8; 16],
    },
    #[cfg(not(any(all(unix, not(target_arch = "wasm32")), windows)))]
    HostPath(PathBuf),
    Virtual(PathBuf),
}

impl WorkspaceDirectoryIdentity {
    fn generation_key(&self) -> String {
        let mut hasher = StableHasher::new("vo-workspace-directory-identity-v2");
        match self {
            #[cfg(any(all(unix, not(target_arch = "wasm32")), windows))]
            Self::Host { volume, file } => {
                hasher.update_str("kind", "host");
                hasher.update_bytes("volume", &volume.to_le_bytes());
                hasher.update_bytes("file", file);
            }
            #[cfg(not(any(all(unix, not(target_arch = "wasm32")), windows)))]
            Self::HostPath(path) => {
                hasher.update_str("kind", "host-path");
                hasher.update_path("path", path);
            }
            Self::Virtual(path) => {
                hasher.update_str("kind", "virtual");
                hasher.update_path("path", path);
            }
        }
        hasher.finish()
    }
}

fn require_workspace_member_generation<F: FileSystem>(
    fs: &F,
    anchor: &Path,
    path: &Path,
    index: usize,
    expected: &WorkspaceDirectoryIdentity,
) -> Result<(), Error> {
    require_workspace_directory_generation(
        fs,
        anchor,
        path,
        &format!("members[{index}] directory"),
        expected,
    )
}

fn workspace_member_directory_identity<F: FileSystem>(
    fs: &F,
    anchor: &Path,
    path: &Path,
    index: usize,
) -> Result<WorkspaceDirectoryIdentity, Error> {
    let label = format!("members[{index}] directory");
    workspace_directory_identity(fs, anchor, path, &label)
}

fn require_workspace_directory_generation<F: FileSystem>(
    fs: &F,
    anchor: &Path,
    path: &Path,
    label: &str,
    expected: &WorkspaceDirectoryIdentity,
) -> Result<(), Error> {
    let current = workspace_directory_identity(fs, anchor, path, label)?;
    if &current != expected {
        return Err(Error::WorkFileParse(format!(
            "{label} {} changed identity after discovery; refusing a mixed workspace generation",
            path.display(),
        )));
    }
    Ok(())
}

fn workspace_directory_identity<F: FileSystem>(
    fs: &F,
    anchor: &Path,
    path: &Path,
    label: &str,
) -> Result<WorkspaceDirectoryIdentity, Error> {
    require_directory_components(fs, anchor, path, label)?;
    let host_anchor = fs.resolve_host_path(anchor).map_err(|error| {
        source_read_error(
            error.kind(),
            format!(
                "cannot resolve host path for workspace directory {}: {error}",
                anchor.display(),
            ),
        )
    })?;
    let Some(host_path) = fs.resolve_host_path(path).map_err(|error| {
        source_read_error(
            error.kind(),
            format!(
                "cannot resolve host path for {label} {}: {error}",
                path.display(),
            ),
        )
    })?
    else {
        return Ok(WorkspaceDirectoryIdentity::Virtual(path.to_path_buf()));
    };
    let Some(host_anchor) = host_anchor else {
        return Err(Error::WorkFileParse(format!(
            "{label} {} has a host path while its workspace directory {} does not",
            path.display(),
            anchor.display(),
        )));
    };
    host_directory_identity(&host_anchor, &host_path, label)
}

/// Validate every directory prefix in the VFS namespace. Host-backed file
/// systems receive a second, descriptor-anchored validation below; this pass
/// also gives virtual file systems the same fail-closed component policy.
fn require_directory_components<F: FileSystem>(
    fs: &F,
    anchor: &Path,
    path: &Path,
    label: &str,
) -> Result<(), Error> {
    require_directory_components_with_host_path(fs, anchor, path, label, None)
}

fn require_stable_regular_file_parent_components<F: FileSystem>(
    fs: &F,
    anchor: &Path,
    path: &Path,
    label: &str,
    host_path: Option<&Path>,
) -> Result<(), Error> {
    require_directory_components_with_host_path(fs, anchor, path, label, host_path)
}

fn require_directory_components_with_host_path<F: FileSystem>(
    fs: &F,
    anchor: &Path,
    path: &Path,
    label: &str,
    host_path: Option<&Path>,
) -> Result<(), Error> {
    use std::path::Component;

    let (mut prefix, traversal) = match directory_traversal(anchor, path) {
        DirectoryTraversal::Anchored(relative) => (normalize_fs_path(anchor), relative),
        DirectoryTraversal::Namespace(path) => (PathBuf::new(), path),
    };
    let mut inspected_component = false;
    for component in traversal.components() {
        match component {
            Component::Prefix(_) | Component::RootDir => {
                prefix.push(component.as_os_str());
                continue;
            }
            Component::CurDir => continue,
            Component::ParentDir | Component::Normal(_) => {
                prefix = normalize_fs_path(&prefix.join(component.as_os_str()));
            }
        }
        inspected_component = true;
        let component_label = if prefix == path {
            label.to_string()
        } else {
            format!("{label} component")
        };
        require_directory_entry_kind(fs, &prefix, &component_label, host_path)?;
    }
    if !inspected_component {
        require_directory_entry_kind(fs, path, label, host_path)?;
    }
    Ok(())
}

fn require_directory_entry_kind<F: FileSystem>(
    fs: &F,
    path: &Path,
    label: &str,
    host_path: Option<&Path>,
) -> Result<(), Error> {
    let found = fs.entry_kind(path).map_err(|error| {
        source_read_error(
            error.kind(),
            format!("cannot inspect {label} {}: {error}", path.display()),
        )
    })?;
    if found == FileSystemEntryKind::Directory {
        return Ok(());
    }
    if found == FileSystemEntryKind::Symlink
        && host_path.is_some_and(|host_path| verified_macos_system_alias_component(path, host_path))
    {
        return Ok(());
    }
    Err(Error::WorkFileParse(format!(
        "{label} {} must be {:?} without symbolic links; found {found:?}",
        path.display(),
        FileSystemEntryKind::Directory,
    )))
}

fn verified_macos_system_alias_component(path: &Path, host_path: &Path) -> bool {
    #[cfg(target_os = "macos")]
    {
        for (alias, target) in [
            (Path::new("/var"), Path::new("/private/var")),
            (Path::new("/tmp"), Path::new("/private/tmp")),
            (Path::new("/etc"), Path::new("/private/etc")),
        ] {
            if path == alias
                && (host_path.starts_with(alias) || host_path.starts_with(target))
                && macos_system_alias_points_to(alias, target)
            {
                return true;
            }
        }
    }
    #[cfg(not(target_os = "macos"))]
    let _ = (path, host_path);
    false
}

#[derive(Debug)]
enum DirectoryTraversal {
    /// Traverse relative to an already trusted directory capability.
    Anchored(PathBuf),
    /// Traverse from the process filesystem namespace when the two paths do
    /// not share a compatible root or prefix.
    Namespace(PathBuf),
}

fn directory_traversal(anchor: &Path, target: &Path) -> DirectoryTraversal {
    use std::path::Component;

    let anchor = normalize_fs_path(anchor);
    let target = normalize_fs_path(target);
    let anchor_components = anchor
        .components()
        .filter(|component| !matches!(component, Component::CurDir))
        .collect::<Vec<_>>();
    let target_components = target
        .components()
        .filter(|component| !matches!(component, Component::CurDir))
        .collect::<Vec<_>>();
    let anchor_root = anchor_components
        .iter()
        .take_while(|component| matches!(component, Component::Prefix(_) | Component::RootDir))
        .copied()
        .collect::<Vec<_>>();
    let target_root = target_components
        .iter()
        .take_while(|component| matches!(component, Component::Prefix(_) | Component::RootDir))
        .copied()
        .collect::<Vec<_>>();
    if anchor_root != target_root {
        return DirectoryTraversal::Namespace(target);
    }

    let common = anchor_components
        .iter()
        .zip(&target_components)
        .take_while(|(left, right)| left == right)
        .count();
    let anchor_tail = &anchor_components[common..];
    if anchor_tail
        .iter()
        .any(|component| !matches!(component, Component::Normal(_)))
    {
        return DirectoryTraversal::Namespace(target);
    }

    let mut relative = PathBuf::new();
    for _ in anchor_tail {
        relative.push("..");
    }
    for component in &target_components[common..] {
        if matches!(component, Component::Prefix(_) | Component::RootDir) {
            return DirectoryTraversal::Namespace(target);
        }
        relative.push(component.as_os_str());
    }
    if relative.as_os_str().is_empty() {
        relative.push(".");
    }
    DirectoryTraversal::Anchored(relative)
}

#[cfg(all(unix, not(target_arch = "wasm32")))]
fn host_file_metadata_snapshot(
    file: &std::fs::File,
    path: &Path,
    label: &str,
) -> Result<HostFileMetadataSnapshot, Error> {
    use std::os::unix::fs::MetadataExt;

    let metadata = file
        .metadata()
        .map_err(|error| host_directory_open_error(label, path, path, error))?;
    if !metadata.is_file() {
        return Err(Error::WorkFileParse(format!(
            "{label} host path {} must resolve to a regular file before reading",
            path.display()
        )));
    }
    Ok(HostFileMetadataSnapshot {
        identity: HostEntryIdentity {
            volume: metadata.dev(),
            file: unix_file_id(metadata.ino()),
        },
        len: metadata.size(),
        mode: metadata.mode(),
        links: metadata.nlink(),
        modified_seconds: metadata.mtime(),
        modified_nanoseconds: metadata.mtime_nsec(),
        changed_seconds: metadata.ctime(),
        changed_nanoseconds: metadata.ctime_nsec(),
    })
}

#[cfg(all(unix, not(target_arch = "wasm32")))]
fn host_directory_metadata_snapshot(
    directory: &std::fs::File,
    path: &Path,
    label: &str,
) -> Result<HostDirectoryMetadataSnapshot, Error> {
    use std::os::unix::fs::MetadataExt;

    let metadata = directory
        .metadata()
        .map_err(|error| host_directory_open_error(label, path, path, error))?;
    if !metadata.is_dir() {
        return Err(Error::WorkFileParse(format!(
            "{label} host path parent {} must remain a directory while reading",
            path.display()
        )));
    }
    Ok(HostDirectoryMetadataSnapshot {
        identity: HostEntryIdentity {
            volume: metadata.dev(),
            file: unix_file_id(metadata.ino()),
        },
    })
}

#[cfg(windows)]
fn host_file_metadata_snapshot(
    file: &std::fs::File,
    path: &Path,
    label: &str,
) -> Result<HostFileMetadataSnapshot, Error> {
    let metadata = file
        .metadata()
        .map_err(|error| host_directory_open_error(label, path, path, error))?;
    if !metadata.is_file() {
        return Err(Error::WorkFileParse(format!(
            "{label} host path {} must resolve to a regular file before reading",
            path.display()
        )));
    }
    let information = crate::windows_file::file_information(file)
        .map_err(|error| host_directory_open_error(label, path, path, error))?;
    const FILE_ATTRIBUTE_REPARSE_POINT: u32 = 0x0000_0400;
    if information.directory
        || information.delete_pending
        || information.links != 1
        || information.attributes & FILE_ATTRIBUTE_REPARSE_POINT != 0
    {
        return Err(Error::WorkFileParse(format!(
            "{label} host path {} must remain one non-reparse regular-file entry",
            path.display()
        )));
    }
    Ok(HostFileMetadataSnapshot {
        identity: HostEntryIdentity {
            volume: information.volume,
            file: information.file,
        },
        len: information.size,
        attributes: information.attributes,
        creation_time: information.creation_time,
        last_write_time: information.last_write_time,
        change_time: information.change_time,
        links: information.links,
    })
}

#[cfg(windows)]
fn host_directory_metadata_snapshot(
    directory: &std::fs::File,
    path: &Path,
    label: &str,
) -> Result<HostDirectoryMetadataSnapshot, Error> {
    let metadata = directory
        .metadata()
        .map_err(|error| host_directory_open_error(label, path, path, error))?;
    if !metadata.is_dir() {
        return Err(Error::WorkFileParse(format!(
            "{label} host path parent {} must remain a directory while reading",
            path.display()
        )));
    }
    let information = crate::windows_file::file_information(directory)
        .map_err(|error| host_directory_open_error(label, path, path, error))?;
    const FILE_ATTRIBUTE_REPARSE_POINT: u32 = 0x0000_0400;
    if !information.directory
        || information.delete_pending
        || information.attributes & FILE_ATTRIBUTE_REPARSE_POINT != 0
    {
        return Err(Error::WorkFileParse(format!(
            "{label} host path parent {} must remain one non-reparse directory entry",
            path.display()
        )));
    }
    Ok(HostDirectoryMetadataSnapshot {
        identity: HostEntryIdentity {
            volume: information.volume,
            file: information.file,
        },
    })
}

#[cfg(any(all(unix, not(target_arch = "wasm32")), windows))]
fn host_stable_regular_file_identity(
    anchor: &Path,
    path: &Path,
    label: &str,
) -> Result<StableRegularFileIdentity, Error> {
    open_host_stable_regular_file(anchor, path, label).map(|opened| opened.identity)
}

#[cfg(all(test, any(all(unix, not(target_arch = "wasm32")), windows)))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StableRegularFileReadTestPhase {
    BeforeDescriptorRead,
    AfterDescriptorRead,
}

#[cfg(all(test, any(all(unix, not(target_arch = "wasm32")), windows)))]
type StableRegularFileReadTestHook = Box<dyn FnMut(StableRegularFileReadTestPhase, &Path) + Send>;

#[cfg(all(test, any(all(unix, not(target_arch = "wasm32")), windows)))]
static STABLE_REGULAR_FILE_READ_TEST_HOOK: std::sync::OnceLock<
    std::sync::Mutex<Option<StableRegularFileReadTestHook>>,
> = std::sync::OnceLock::new();

#[cfg(all(test, any(all(unix, not(target_arch = "wasm32")), windows)))]
fn run_stable_regular_file_read_test_hook(phase: StableRegularFileReadTestPhase, path: &Path) {
    let hook = STABLE_REGULAR_FILE_READ_TEST_HOOK.get_or_init(Default::default);
    if let Some(hook) = hook
        .lock()
        .unwrap_or_else(|error| error.into_inner())
        .as_mut()
    {
        hook(phase, path);
    }
}

#[cfg(any(all(unix, not(target_arch = "wasm32")), windows))]
fn read_host_stable_regular_file_bytes(
    anchor: &Path,
    path: &Path,
    max_bytes: usize,
    label: &str,
) -> Result<(Vec<u8>, StableRegularFileIdentity), Error> {
    use std::io::Read;

    let mut opened = open_host_stable_regular_file(anchor, path, label)?;
    let max_bytes_u64 = u64::try_from(max_bytes).unwrap_or(u64::MAX);
    if opened.metadata.len > max_bytes_u64 {
        return Err(source_read_error(
            std::io::ErrorKind::InvalidData,
            format!(
                "{label} {} exceeds the {max_bytes}-byte size limit",
                path.display()
            ),
        ));
    }
    #[cfg(test)]
    run_stable_regular_file_read_test_hook(
        StableRegularFileReadTestPhase::BeforeDescriptorRead,
        path,
    );
    let read_limit = max_bytes_u64.saturating_add(1);
    let mut bytes = Vec::new();
    bytes
        .try_reserve(
            usize::try_from(opened.metadata.len)
                .unwrap_or(max_bytes)
                .min(max_bytes),
        )
        .map_err(|_| {
            source_read_error(
                std::io::ErrorKind::OutOfMemory,
                format!("cannot allocate buffer for {label} {}", path.display()),
            )
        })?;
    (&mut opened.file)
        .take(read_limit)
        .read_to_end(&mut bytes)
        .map_err(|error| {
            source_read_error(
                error.kind(),
                format!(
                    "cannot read {label} {} from its validated descriptor: {error}",
                    path.display()
                ),
            )
        })?;
    #[cfg(test)]
    run_stable_regular_file_read_test_hook(
        StableRegularFileReadTestPhase::AfterDescriptorRead,
        path,
    );
    if bytes.len() > max_bytes {
        return Err(source_read_error(
            std::io::ErrorKind::InvalidData,
            format!(
                "{label} {} exceeds the {max_bytes}-byte size limit",
                path.display()
            ),
        ));
    }
    let after_read = host_file_metadata_snapshot(&opened.file, path, label)?;
    let after_parent = host_directory_metadata_snapshot(&opened.parent_directory, path, label)?;
    if after_read != opened.metadata || after_parent != opened.parent_metadata {
        return Err(stable_regular_file_generation_changed(path, label));
    }
    let path_identity = host_stable_regular_file_identity(anchor, path, label)?;
    let final_descriptor = host_file_metadata_snapshot(&opened.file, path, label)?;
    let final_parent = host_directory_metadata_snapshot(&opened.parent_directory, path, label)?;
    if final_descriptor != opened.metadata
        || final_parent != opened.parent_metadata
        || path_identity != opened.identity
    {
        return Err(stable_regular_file_generation_changed(path, label));
    }
    Ok((bytes, opened.identity))
}

#[cfg(not(any(all(unix, not(target_arch = "wasm32")), windows)))]
fn read_host_stable_regular_file_bytes(
    _anchor: &Path,
    path: &Path,
    _max_bytes: usize,
    label: &str,
) -> Result<(Vec<u8>, StableRegularFileIdentity), Error> {
    Err(Error::WorkFileParse(format!(
        "{label} host path {} cannot be read through a no-follow descriptor on this platform",
        path.display()
    )))
}

#[cfg(all(unix, not(target_arch = "wasm32")))]
fn open_host_stable_regular_file(
    anchor: &Path,
    path: &Path,
    label: &str,
) -> Result<OpenedHostRegularFile, Error> {
    use std::ffi::{CString, OsString};
    use std::fs::{File, OpenOptions};
    use std::os::fd::{AsRawFd, FromRawFd};
    use std::os::unix::ffi::OsStrExt;
    use std::os::unix::fs::{MetadataExt, OpenOptionsExt};
    use std::path::Component;

    let (open_path, traversal, no_follow_anchor) = match directory_traversal(anchor, path) {
        DirectoryTraversal::Anchored(relative) => (anchor, relative, false),
        DirectoryTraversal::Namespace(path) => {
            let root = if path.is_absolute() {
                Path::new("/")
            } else {
                Path::new(".")
            };
            (root, path, true)
        }
    };
    let mut flags = libc::O_CLOEXEC | libc::O_DIRECTORY;
    if no_follow_anchor {
        flags |= libc::O_NOFOLLOW;
    }
    let mut directory = OpenOptions::new()
        .read(true)
        .custom_flags(flags)
        .open(open_path)
        .map_err(|error| host_directory_open_error(label, path, open_path, error))?;
    let anchor_metadata = directory
        .metadata()
        .map_err(|error| host_directory_open_error(label, path, open_path, error))?;
    if !anchor_metadata.is_dir() {
        return Err(Error::WorkFileParse(format!(
            "{label} namespace anchor {} must resolve to a directory",
            open_path.display()
        )));
    }
    let mut identities = vec![HostEntryIdentity {
        volume: anchor_metadata.dev(),
        file: unix_file_id(anchor_metadata.ino()),
    }];
    let mut components = Vec::<(OsString, bool)>::new();
    for component in traversal.components() {
        match component {
            Component::Prefix(_) => {
                return Err(Error::WorkFileParse(format!(
                    "{label} host path {} contains an unsupported platform prefix",
                    path.display()
                )))
            }
            Component::RootDir | Component::CurDir => {}
            Component::ParentDir => components.push((OsString::from(".."), false)),
            Component::Normal(name) => components.push((name.to_os_string(), true)),
        }
    }
    let Some((leaf_name, leaf_is_normal)) = components.pop() else {
        return Err(Error::WorkFileParse(format!(
            "{label} host path {} has no file-name component",
            path.display()
        )));
    };
    if !leaf_is_normal {
        return Err(Error::WorkFileParse(format!(
            "{label} host path {} must end in a regular file name",
            path.display()
        )));
    }

    let mut opened_path = open_path.to_path_buf();
    for (name, _) in components {
        let c_name = CString::new(name.as_os_str().as_bytes()).map_err(|_| {
            Error::WorkFileParse(format!(
                "{label} host path {} contains a NUL byte",
                path.display()
            ))
        })?;
        let descriptor = loop {
            let descriptor = unsafe {
                libc::openat(
                    directory.as_raw_fd(),
                    c_name.as_ptr(),
                    libc::O_RDONLY | libc::O_CLOEXEC | libc::O_DIRECTORY | libc::O_NOFOLLOW,
                )
            };
            if descriptor >= 0 {
                break descriptor;
            }
            let error = std::io::Error::last_os_error();
            if error.kind() != std::io::ErrorKind::Interrupted {
                return Err(host_directory_open_error(
                    label,
                    path,
                    &opened_path.join(&name),
                    error,
                ));
            }
        };
        directory = unsafe { File::from_raw_fd(descriptor) };
        opened_path.push(&name);
        let metadata = directory
            .metadata()
            .map_err(|error| host_directory_open_error(label, path, &opened_path, error))?;
        if !metadata.is_dir() {
            return Err(Error::WorkFileParse(format!(
                "{label} host path {} contains a non-directory component {}",
                path.display(),
                opened_path.display()
            )));
        }
        identities.push(HostEntryIdentity {
            volume: metadata.dev(),
            file: unix_file_id(metadata.ino()),
        });
    }

    let c_leaf = CString::new(leaf_name.as_os_str().as_bytes()).map_err(|_| {
        Error::WorkFileParse(format!(
            "{label} host path {} contains a NUL byte",
            path.display()
        ))
    })?;
    let descriptor = loop {
        let descriptor = unsafe {
            libc::openat(
                directory.as_raw_fd(),
                c_leaf.as_ptr(),
                libc::O_RDONLY | libc::O_CLOEXEC | libc::O_NOFOLLOW | libc::O_NONBLOCK,
            )
        };
        if descriptor >= 0 {
            break descriptor;
        }
        let error = std::io::Error::last_os_error();
        if error.kind() != std::io::ErrorKind::Interrupted {
            return Err(host_directory_open_error(
                label,
                path,
                &opened_path.join(&leaf_name),
                error,
            ));
        }
    };
    let file = unsafe { File::from_raw_fd(descriptor) };
    let metadata = file.metadata().map_err(|error| {
        host_directory_open_error(label, path, &opened_path.join(&leaf_name), error)
    })?;
    if !metadata.is_file() {
        return Err(Error::WorkFileParse(format!(
            "{label} host path {} must resolve to a regular file without links or special entries",
            path.display()
        )));
    }
    let metadata = host_file_metadata_snapshot(&file, path, label)?;
    let parent_metadata = host_directory_metadata_snapshot(&directory, path, label)?;
    identities.push(metadata.identity);
    Ok(OpenedHostRegularFile {
        file,
        parent_directory: directory,
        identity: StableRegularFileIdentity::Host(identities),
        metadata,
        parent_metadata,
    })
}

#[cfg(windows)]
fn windows_host_entry_identity(
    file: &std::fs::File,
    path: &Path,
    label: &str,
    directory: bool,
) -> Result<HostEntryIdentity, Error> {
    const FILE_ATTRIBUTE_DIRECTORY: u32 = 0x0000_0010;
    const FILE_ATTRIBUTE_REPARSE_POINT: u32 = 0x0000_0400;
    let information = crate::windows_file::file_information(file)
        .map_err(|error| host_directory_open_error(label, path, path, error))?;
    let attribute_directory = information.attributes & FILE_ATTRIBUTE_DIRECTORY != 0;
    if information.attributes & FILE_ATTRIBUTE_REPARSE_POINT != 0
        || information.delete_pending
        || attribute_directory != directory
        || information.directory != directory
        || (!directory && information.links != 1)
    {
        return Err(Error::WorkFileParse(format!(
            "{label} host path component {} must remain a non-reparse {}",
            path.display(),
            if directory {
                "directory"
            } else {
                "regular file"
            },
        )));
    }
    Ok(HostEntryIdentity {
        volume: information.volume,
        file: information.file,
    })
}

#[cfg(windows)]
fn windows_path_entry_identity(
    entry_path: &Path,
    requested_path: &Path,
    label: &str,
    directory: bool,
) -> Result<HostEntryIdentity, Error> {
    use std::os::windows::fs::OpenOptionsExt;

    const FILE_FLAG_BACKUP_SEMANTICS: u32 = 0x0200_0000;
    const FILE_FLAG_OPEN_REPARSE_POINT: u32 = 0x0020_0000;
    let flags = FILE_FLAG_OPEN_REPARSE_POINT
        | if directory {
            FILE_FLAG_BACKUP_SEMANTICS
        } else {
            0
        };
    let file = std::fs::OpenOptions::new()
        .read(true)
        .custom_flags(flags)
        .open(entry_path)
        .map_err(|error| host_directory_open_error(label, requested_path, entry_path, error))?;
    windows_host_entry_identity(&file, entry_path, label, directory)
}

#[cfg(windows)]
fn open_host_stable_regular_file(
    anchor: &Path,
    path: &Path,
    label: &str,
) -> Result<OpenedHostRegularFile, Error> {
    use std::ffi::OsString;
    use std::fs::OpenOptions;
    use std::os::windows::fs::{MetadataExt, OpenOptionsExt};
    use std::path::Component;

    const FILE_ATTRIBUTE_REPARSE_POINT: u32 = 0x0000_0400;
    const FILE_FLAG_BACKUP_SEMANTICS: u32 = 0x0200_0000;
    const FILE_FLAG_OPEN_REPARSE_POINT: u32 = 0x0020_0000;

    let (mut prefix, traversal) = match directory_traversal(anchor, path) {
        DirectoryTraversal::Anchored(relative) => (anchor.to_path_buf(), relative),
        DirectoryTraversal::Namespace(path) => {
            let namespace_anchor = filesystem_namespace_anchor(&path, label)?;
            let DirectoryTraversal::Anchored(relative) =
                directory_traversal(&namespace_anchor, &path)
            else {
                return Err(Error::WorkFileParse(format!(
                    "{label} host path {} has no safe namespace anchor",
                    path.display()
                )));
            };
            (namespace_anchor, relative)
        }
    };
    let mut directory = OpenOptions::new()
        .read(true)
        .custom_flags(FILE_FLAG_BACKUP_SEMANTICS)
        .open(&prefix)
        .map_err(|error| host_directory_open_error(label, path, &prefix, error))?;
    let anchor_metadata = directory
        .metadata()
        .map_err(|error| host_directory_open_error(label, path, &prefix, error))?;
    if !anchor_metadata.is_dir() {
        return Err(Error::WorkFileParse(format!(
            "{label} namespace anchor {} must resolve to a directory",
            prefix.display()
        )));
    }
    let mut identities = vec![windows_host_entry_identity(
        &directory, &prefix, label, true,
    )?];
    let mut components = Vec::<(OsString, bool)>::new();
    for component in traversal.components() {
        match component {
            Component::Prefix(_) | Component::RootDir => {
                return Err(Error::WorkFileParse(format!(
                    "{label} host path {} contains an unexpected namespace component",
                    path.display()
                )))
            }
            Component::CurDir => {}
            Component::ParentDir => components.push((OsString::from(".."), false)),
            Component::Normal(name) => components.push((name.to_os_string(), true)),
        }
    }
    let Some((leaf_name, leaf_is_normal)) = components.pop() else {
        return Err(Error::WorkFileParse(format!(
            "{label} host path {} has no file-name component",
            path.display()
        )));
    };
    if !leaf_is_normal {
        return Err(Error::WorkFileParse(format!(
            "{label} host path {} must end in a regular file name",
            path.display()
        )));
    }

    for (name, _) in components {
        prefix.push(&name);
        let linked = std::fs::symlink_metadata(&prefix)
            .map_err(|error| host_directory_open_error(label, path, &prefix, error))?;
        if !linked.is_dir() || linked.file_attributes() & FILE_ATTRIBUTE_REPARSE_POINT != 0 {
            return Err(Error::WorkFileParse(format!(
                "{label} host path {} contains a non-directory or reparse component {}",
                path.display(),
                prefix.display()
            )));
        }
        let linked_identity = windows_path_entry_identity(&prefix, path, label, true)?;
        let opened = OpenOptions::new()
            .read(true)
            .custom_flags(FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT)
            .open(&prefix)
            .map_err(|error| host_directory_open_error(label, path, &prefix, error))?;
        let metadata = opened
            .metadata()
            .map_err(|error| host_directory_open_error(label, path, &prefix, error))?;
        let current = std::fs::symlink_metadata(&prefix)
            .map_err(|error| host_directory_open_error(label, path, &prefix, error))?;
        let metadata_identity = windows_host_entry_identity(&opened, &prefix, label, true)?;
        let current_identity = windows_path_entry_identity(&prefix, path, label, true)?;
        if !metadata.is_dir()
            || !current.is_dir()
            || metadata.file_attributes() & FILE_ATTRIBUTE_REPARSE_POINT != 0
            || current.file_attributes() & FILE_ATTRIBUTE_REPARSE_POINT != 0
            || linked_identity != metadata_identity
            || metadata_identity != current_identity
        {
            return Err(Error::WorkFileParse(format!(
                "{label} host path component {} changed identity or is a reparse point",
                prefix.display()
            )));
        }
        identities.push(metadata_identity);
        directory = opened;
    }

    prefix.push(&leaf_name);
    let linked = std::fs::symlink_metadata(&prefix)
        .map_err(|error| host_directory_open_error(label, path, &prefix, error))?;
    if !linked.is_file() || linked.file_attributes() & FILE_ATTRIBUTE_REPARSE_POINT != 0 {
        return Err(Error::WorkFileParse(format!(
            "{label} host path {} ends in a non-file or reparse entry {}",
            path.display(),
            prefix.display()
        )));
    }
    let linked_identity = windows_path_entry_identity(&prefix, path, label, false)?;
    let opened = OpenOptions::new()
        .read(true)
        .custom_flags(FILE_FLAG_OPEN_REPARSE_POINT)
        .open(&prefix)
        .map_err(|error| host_directory_open_error(label, path, &prefix, error))?;
    let metadata = opened
        .metadata()
        .map_err(|error| host_directory_open_error(label, path, &prefix, error))?;
    let current = std::fs::symlink_metadata(&prefix)
        .map_err(|error| host_directory_open_error(label, path, &prefix, error))?;
    let metadata_identity = windows_host_entry_identity(&opened, &prefix, label, false)?;
    let current_identity = windows_path_entry_identity(&prefix, path, label, false)?;
    if !metadata.is_file()
        || !current.is_file()
        || metadata.file_attributes() & FILE_ATTRIBUTE_REPARSE_POINT != 0
        || current.file_attributes() & FILE_ATTRIBUTE_REPARSE_POINT != 0
        || linked_identity != metadata_identity
        || metadata_identity != current_identity
    {
        return Err(Error::WorkFileParse(format!(
            "{label} host path entry {} changed identity or is a reparse point",
            prefix.display()
        )));
    }
    let metadata = host_file_metadata_snapshot(&opened, path, label)?;
    let parent_metadata = host_directory_metadata_snapshot(&directory, path, label)?;
    identities.push(metadata.identity);
    Ok(OpenedHostRegularFile {
        file: opened,
        parent_directory: directory,
        identity: StableRegularFileIdentity::Host(identities),
        metadata,
        parent_metadata,
    })
}

#[cfg(not(any(all(unix, not(target_arch = "wasm32")), windows)))]
fn host_stable_regular_file_identity(
    _anchor: &Path,
    path: &Path,
    label: &str,
) -> Result<StableRegularFileIdentity, Error> {
    Err(Error::WorkFileParse(format!(
        "{label} host path {} cannot be validated against symlink, reparse, and path-replacement races on this platform",
        path.display()
    )))
}

#[cfg(all(unix, not(target_arch = "wasm32")))]
fn host_directory_identity(
    anchor: &Path,
    path: &Path,
    label: &str,
) -> Result<WorkspaceDirectoryIdentity, Error> {
    use std::ffi::{CString, OsStr};
    use std::fs::{File, OpenOptions};
    use std::os::fd::{AsRawFd, FromRawFd};
    use std::os::unix::ffi::OsStrExt;
    use std::os::unix::fs::{MetadataExt, OpenOptionsExt};
    use std::path::Component;

    let (open_path, traversal, no_follow_anchor) = match directory_traversal(anchor, path) {
        DirectoryTraversal::Anchored(relative) => (anchor, relative, false),
        DirectoryTraversal::Namespace(path) => {
            let root = if path.is_absolute() {
                Path::new("/")
            } else {
                Path::new(".")
            };
            (root, path, true)
        }
    };
    let mut flags = libc::O_CLOEXEC | libc::O_DIRECTORY;
    if no_follow_anchor {
        flags |= libc::O_NOFOLLOW;
    }
    let mut directory = OpenOptions::new()
        .read(true)
        .custom_flags(flags)
        .open(open_path)
        .map_err(|error| host_directory_open_error(label, path, open_path, error))?;
    let mut opened_path = open_path.to_path_buf();

    for component in traversal.components() {
        let name = match component {
            Component::Prefix(_) => {
                return Err(Error::WorkFileParse(format!(
                    "{label} host path {} contains an unsupported platform prefix",
                    path.display(),
                )))
            }
            Component::RootDir | Component::CurDir => continue,
            Component::ParentDir => OsStr::new(".."),
            Component::Normal(name) => name,
        };
        let name = CString::new(name.as_bytes()).map_err(|_| {
            Error::WorkFileParse(format!(
                "{label} host path {} contains a NUL byte",
                path.display(),
            ))
        })?;
        let descriptor = loop {
            let descriptor = unsafe {
                libc::openat(
                    directory.as_raw_fd(),
                    name.as_ptr(),
                    libc::O_RDONLY | libc::O_CLOEXEC | libc::O_DIRECTORY | libc::O_NOFOLLOW,
                )
            };
            if descriptor >= 0 {
                break descriptor;
            }
            let error = std::io::Error::last_os_error();
            if error.kind() != std::io::ErrorKind::Interrupted {
                return Err(host_directory_open_error(
                    label,
                    path,
                    &opened_path.join(OsStr::from_bytes(name.as_bytes())),
                    error,
                ));
            }
        };
        directory = unsafe { File::from_raw_fd(descriptor) };
        opened_path.push(OsStr::from_bytes(name.as_bytes()));
    }

    let metadata = directory
        .metadata()
        .map_err(|error| host_directory_open_error(label, path, &opened_path, error))?;
    if !metadata.is_dir() {
        return Err(Error::WorkFileParse(format!(
            "{label} host path {} must resolve to a directory",
            path.display(),
        )));
    }
    Ok(WorkspaceDirectoryIdentity::Host {
        volume: metadata.dev(),
        file: unix_file_id(metadata.ino()),
    })
}

#[cfg(windows)]
fn host_directory_identity(
    anchor: &Path,
    path: &Path,
    label: &str,
) -> Result<WorkspaceDirectoryIdentity, Error> {
    use std::fs::OpenOptions;
    use std::os::windows::fs::{MetadataExt, OpenOptionsExt};
    use std::path::Component;

    const FILE_ATTRIBUTE_REPARSE_POINT: u32 = 0x0000_0400;
    const FILE_FLAG_BACKUP_SEMANTICS: u32 = 0x0200_0000;
    const FILE_FLAG_OPEN_REPARSE_POINT: u32 = 0x0020_0000;

    let (mut prefix, traversal, mut final_identity) = match directory_traversal(anchor, path) {
        DirectoryTraversal::Anchored(relative) => {
            let opened = OpenOptions::new()
                .read(true)
                .custom_flags(FILE_FLAG_BACKUP_SEMANTICS)
                .open(anchor)
                .map_err(|error| host_directory_open_error(label, path, anchor, error))?;
            let metadata = opened
                .metadata()
                .map_err(|error| host_directory_open_error(label, path, anchor, error))?;
            if !metadata.is_dir() {
                return Err(Error::WorkFileParse(format!(
                    "trusted workspace directory {} must resolve to a directory",
                    anchor.display(),
                )));
            }
            let identity = windows_host_entry_identity(&opened, anchor, label, true)?;
            (
                anchor.to_path_buf(),
                relative,
                Some(WorkspaceDirectoryIdentity::Host {
                    volume: identity.volume,
                    file: identity.file,
                }),
            )
        }
        DirectoryTraversal::Namespace(path) => (PathBuf::new(), path, None),
    };
    for component in traversal.components() {
        if matches!(component, Component::CurDir) {
            continue;
        }
        prefix.push(component.as_os_str());
        if matches!(component, Component::Prefix(_)) {
            continue;
        }
        let linked = std::fs::symlink_metadata(&prefix)
            .map_err(|error| host_directory_open_error(label, path, &prefix, error))?;
        if !linked.is_dir() || linked.file_attributes() & FILE_ATTRIBUTE_REPARSE_POINT != 0 {
            return Err(Error::WorkFileParse(format!(
                "{label} host path {} contains a non-directory or reparse component {}",
                path.display(),
                prefix.display(),
            )));
        }
        let linked_identity = windows_path_entry_identity(&prefix, path, label, true)?;
        let opened = OpenOptions::new()
            .read(true)
            .custom_flags(FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT)
            .open(&prefix)
            .map_err(|error| host_directory_open_error(label, path, &prefix, error))?;
        let metadata = opened
            .metadata()
            .map_err(|error| host_directory_open_error(label, path, &prefix, error))?;
        let current = std::fs::symlink_metadata(&prefix)
            .map_err(|error| host_directory_open_error(label, path, &prefix, error))?;
        let metadata_identity = windows_host_entry_identity(&opened, &prefix, label, true)?;
        let current_identity = windows_path_entry_identity(&prefix, path, label, true)?;
        if !metadata.is_dir()
            || !current.is_dir()
            || metadata.file_attributes() & FILE_ATTRIBUTE_REPARSE_POINT != 0
            || current.file_attributes() & FILE_ATTRIBUTE_REPARSE_POINT != 0
            || linked_identity != metadata_identity
            || metadata_identity != current_identity
        {
            return Err(Error::WorkFileParse(format!(
                "{label} host path component {} changed identity or is a reparse point",
                prefix.display(),
            )));
        }
        final_identity = Some(WorkspaceDirectoryIdentity::Host {
            volume: metadata_identity.volume,
            file: metadata_identity.file,
        });
    }
    final_identity.ok_or_else(|| {
        Error::WorkFileParse(format!(
            "cannot determine directory identity for {label} {}",
            path.display(),
        ))
    })
}

#[cfg(not(any(all(unix, not(target_arch = "wasm32")), windows)))]
fn host_directory_identity(
    anchor: &Path,
    path: &Path,
    label: &str,
) -> Result<WorkspaceDirectoryIdentity, Error> {
    use std::path::Component;

    let (mut prefix, traversal) = match directory_traversal(anchor, path) {
        DirectoryTraversal::Anchored(relative) => (anchor.to_path_buf(), relative),
        DirectoryTraversal::Namespace(path) => (PathBuf::new(), path),
    };
    let mut inspected_component = false;
    for component in traversal.components() {
        if matches!(component, Component::CurDir) {
            continue;
        }
        prefix.push(component.as_os_str());
        if matches!(component, Component::Prefix(_) | Component::RootDir) {
            continue;
        }
        inspected_component = true;
        let metadata = std::fs::symlink_metadata(&prefix)
            .map_err(|error| host_directory_open_error(label, path, &prefix, error))?;
        if metadata.file_type().is_symlink() || !metadata.is_dir() {
            return Err(Error::WorkFileParse(format!(
                "{label} host path {} contains a linked or non-directory component {}",
                path.display(),
                prefix.display(),
            )));
        }
    }
    if !inspected_component {
        let metadata = std::fs::metadata(&prefix)
            .map_err(|error| host_directory_open_error(label, path, &prefix, error))?;
        if !metadata.is_dir() {
            return Err(Error::WorkFileParse(format!(
                "{label} host path {} must be a real directory",
                path.display(),
            )));
        }
    }
    let canonical = std::fs::canonicalize(path)
        .map_err(|error| host_directory_open_error(label, path, path, error))?;
    Ok(WorkspaceDirectoryIdentity::HostPath(canonical))
}

fn host_directory_open_error(
    label: &str,
    path: &Path,
    component: &Path,
    error: std::io::Error,
) -> Error {
    source_read_error(
        error.kind(),
        format!(
            "cannot open {label} host path {} safely at component {} without following links or reparse points: {error}",
            path.display(),
            component.display(),
        ),
    )
}

fn exclude_active_module<F: FileSystem>(
    fs: &F,
    mut members: Vec<WorkspaceMember>,
    project_dir: &Path,
    root_module: Option<&ModIdentity>,
) -> Result<Vec<WorkspaceMember>, Error> {
    let project_dir = normalize_fs_path(project_dir);
    let active_module = root_module.map(ModIdentity::as_str);
    let active_generation = match (root_module, members.first()) {
        (Some(_), Some(member)) => {
            let anchor = member.namespace_anchor.clone();
            let identity = workspace_directory_identity(
                fs,
                &anchor,
                &project_dir,
                "active project directory",
            )?;
            Some((anchor, identity))
        }
        _ => None,
    };
    let active_identity = active_generation.as_ref().map(|(_, identity)| identity);
    let mut conflict = None;
    members.retain(|member| {
        let same_directory = member.local_dir == project_dir
            || active_identity.is_some_and(|identity| identity == &member.directory_identity);
        if same_directory {
            if active_module.is_some_and(|active| active != member.module.as_str()) {
                conflict = Some(format!(
                    "active project directory {} declares module {}, while the captured active root identity is {}",
                    project_dir.display(),
                    member.module,
                    active_module.expect("the mismatch requires an active identity"),
                ));
            }
            return false;
        }
        if active_module.is_some_and(|active| active == member.module.as_str()) {
            conflict = Some(format!(
                "workspace member directory {} declares the active root identity {}; the active root at {} is implicit and only that same directory may be omitted from workspace sources",
                member.local_dir.display(),
                member.module,
                project_dir.display(),
            ));
        }
        true
    });

    if let Some((anchor, identity)) = active_generation.as_ref() {
        require_workspace_directory_generation(
            fs,
            anchor,
            &project_dir,
            "active project directory",
            identity,
        )?;
    }
    if let Some(detail) = conflict {
        return Err(Error::WorkspaceValidation(detail));
    }
    Ok(members)
}

fn require_entry_kind<F: FileSystem>(
    fs: &F,
    path: &Path,
    expected: FileSystemEntryKind,
    label: &str,
) -> Result<(), Error> {
    let found = fs.entry_kind(path).map_err(|error| {
        source_read_error(
            error.kind(),
            format!("cannot inspect {label} {}: {error}", path.display()),
        )
    })?;
    if found != expected {
        return Err(Error::WorkFileParse(format!(
            "{label} {} must be {expected:?} without symbolic links; found {found:?}",
            path.display()
        )));
    }
    Ok(())
}

fn read_active_module_in<F: FileSystem>(
    fs: &F,
    project_dir: &Path,
) -> Result<Option<ModIdentity>, Error> {
    let mod_path = normalize_fs_path(&project_dir.join("vo.mod"));
    match fs.entry_kind(&mod_path).map_err(|error| {
        Error::WorkFileParse(format!(
            "cannot inspect active module file {}: {error}",
            mod_path.display()
        ))
    })? {
        FileSystemEntryKind::Missing => Ok(None),
        FileSystemEntryKind::RegularFile => {
            let content = read_stable_regular_text_file(
                fs,
                &mod_path,
                vo_common::vfs::MAX_TEXT_FILE_BYTES,
                "active module file",
            )?;
            let module = ModFile::parse(&content)
                .map_err(|error| {
                    Error::WorkFileParse(format!(
                        "error parsing active module file {}: {error}",
                        mod_path.display()
                    ))
                })?
                .module;
            Ok(Some(module))
        }
        found => Err(Error::WorkFileParse(format!(
            "active module file {} must be RegularFile without symbolic links; found {found:?}",
            mod_path.display()
        ))),
    }
}

fn check_import_covered_by_edges(
    importer: &str,
    import_path: &str,
    importer_module: &ModIdentity,
    allowed_edges: &[ModulePath],
) -> Result<(), Error> {
    if crate::identity::classify_import(import_path)? != crate::identity::ImportClass::External {
        return Err(Error::InvalidImportPath(format!(
            "workspace dependency coverage requires an external import: {import_path}"
        )));
    }
    if let Some(importer_github) = importer_module.as_public() {
        if importer_github.owns_import(import_path).is_some() {
            return Ok(());
        }
    }
    if crate::identity::find_owning_module(import_path, allowed_edges).is_some() {
        return Ok(());
    }

    Err(Error::WorkspaceSourceOutsideGraph {
        importer: importer.to_string(),
        import_path: import_path.to_string(),
    })
}

/// Validate source imports against the exact outgoing edges authorized by the
/// root manifest and the selected lock or workspace graph. Every member scan
/// excludes the other member roots, so nested modules are owned by their
/// nearest `vo.mod` boundary.
pub fn validate_project_external_imports<F: FileSystem>(
    fs: &F,
    project_root: &Path,
    root_mod: &ModFile,
    discovered_candidates: &[WorkspaceMember],
    authorized_sources: &[WorkspaceMember],
    lock_file: Option<&crate::schema::lockfile::LockFile>,
) -> Result<Vec<PathBuf>, Error> {
    let root_edges = root_mod
        .dependencies
        .iter()
        .map(|dependency| dependency.module.clone())
        .collect::<Vec<_>>();
    let normalized_project_root = normalize_fs_path(project_root);
    let excluded_roots = discovered_candidates
        .iter()
        .map(|entry| normalize_fs_path(&entry.local_dir))
        .filter(|directory| directory != &normalized_project_root)
        .collect::<BTreeSet<_>>();
    let mut scanned_entries = 0usize;
    let mut source_files = BTreeSet::new();
    for import_path in scan_external_imports_in_excluding(
        fs,
        project_root,
        &excluded_roots,
        &mut scanned_entries,
        &mut source_files,
    )? {
        check_import_covered_by_edges(
            root_mod.module.as_str(),
            &import_path,
            &root_mod.module,
            &root_edges,
        )?;
    }

    let locked_by_module = lock_file
        .into_iter()
        .flat_map(|lock_file| &lock_file.modules)
        .map(|locked| (locked.path.as_str(), locked))
        .collect::<BTreeMap<_, _>>();
    for source_entry in authorized_sources {
        let allowed_edges = match locked_by_module.get(source_entry.module.as_str()) {
            Some(_) => source_entry
                .mod_file()
                .dependencies
                .iter()
                .map(|dependency| dependency.module.clone())
                .collect::<Vec<_>>(),
            None if lock_file.is_none() => source_entry
                .mod_file()
                .dependencies
                .iter()
                .map(|dependency| dependency.module.clone())
                .collect::<Vec<_>>(),
            None => {
                return Err(Error::DependencyGraph(format!(
                    "workspace member {} is absent from the authoritative lock graph",
                    source_entry.module
                )))
            }
        };
        let excluded_member_roots = discovered_candidates
            .iter()
            .filter(|candidate| candidate.local_dir != source_entry.local_dir)
            .map(|candidate| normalize_fs_path(&candidate.local_dir))
            .chain(
                (normalized_project_root != normalize_fs_path(&source_entry.local_dir))
                    .then(|| normalized_project_root.clone()),
            )
            .collect::<BTreeSet<_>>();
        let imports = scan_external_imports_in_excluding(
            fs,
            &source_entry.local_dir,
            &excluded_member_roots,
            &mut scanned_entries,
            &mut source_files,
        )?;
        let importer = format!("workspace source {}", source_entry.module);
        let importer_module = ModIdentity::Public(source_entry.module.clone());
        for import_path in imports {
            check_import_covered_by_edges(
                &importer,
                &import_path,
                &importer_module,
                &allowed_edges,
            )?;
        }
    }
    Ok(source_files.into_iter().collect())
}

#[cfg(test)]
fn scan_external_imports_in<F: FileSystem>(fs: &F, dir: &Path) -> Result<BTreeSet<String>, Error> {
    let mut scanned_entries = 0usize;
    let mut source_files = BTreeSet::new();
    scan_external_imports_in_with_budget(fs, dir, &mut scanned_entries, &mut source_files)
}

#[cfg(test)]
fn scan_external_imports_in_with_budget<F: FileSystem>(
    fs: &F,
    dir: &Path,
    scanned_entries: &mut usize,
    source_files: &mut BTreeSet<PathBuf>,
) -> Result<BTreeSet<String>, Error> {
    scan_external_imports_in_excluding(fs, dir, &BTreeSet::new(), scanned_entries, source_files)
}

fn scan_external_imports_in_excluding<F: FileSystem>(
    fs: &F,
    dir: &Path,
    excluded_roots: &BTreeSet<PathBuf>,
    scanned_entries: &mut usize,
    source_files: &mut BTreeSet<PathBuf>,
) -> Result<BTreeSet<String>, Error> {
    let mut imports = BTreeSet::new();
    let mut pending = vec![(normalize_fs_path(dir), 0usize)];
    let mut visited = BTreeSet::new();
    const MAX_SCAN_DEPTH: usize = crate::schema::MAX_PORTABLE_PATH_COMPONENTS;
    const MAX_SCAN_ENTRIES: usize = MAX_DIRECTORY_ENTRIES;

    while let Some((dir, depth)) = pending.pop() {
        if !visited.insert(dir.clone()) {
            continue;
        }
        if depth > MAX_SCAN_DEPTH {
            return Err(Error::SourceScan(format!(
                "workspace source scan exceeds the {MAX_SCAN_DEPTH}-directory depth limit at {}",
                dir.display()
            )));
        }
        let mut raw_entries = fs.read_dir(&dir).map_err(Error::Io)?;
        *scanned_entries = scanned_entries
            .checked_add(raw_entries.len())
            .ok_or_else(|| Error::SourceScan("workspace source entry count overflow".into()))?;
        if *scanned_entries > MAX_SCAN_ENTRIES {
            return Err(Error::SourceScan(format!(
                "workspace source scan exceeds the {MAX_SCAN_ENTRIES}-entry limit"
            )));
        }
        sort_fs_paths(&mut raw_entries);
        let mut entries = BTreeSet::new();
        for raw_entry in raw_entries {
            let entry = normalize_fs_path(&raw_entry);
            let entry_parent = normalize_fs_path(entry.parent().unwrap_or(Path::new(".")));
            if entry_parent != dir {
                return Err(Error::SourceScan(format!(
                    "workspace directory {} returned a non-child entry {}",
                    dir.display(),
                    raw_entry.display()
                )));
            }
            if !entries.insert(entry.clone()) {
                return Err(Error::SourceScan(format!(
                    "workspace directory {} returned duplicate entry {}",
                    dir.display(),
                    entry.display()
                )));
            }
        }
        if let Some((alias, canonical)) =
            crate::schema::first_portable_name_alias(&entries, &["vo.mod"])
        {
            return Err(Error::SourceScan(format!(
                "workspace source directory {} contains portable alias {} for canonical protocol file {canonical}",
                dir.display(),
                alias.display(),
            )));
        }
        if let Some(boundary) = entries
            .iter()
            .find(|entry| entry.file_name().and_then(|name| name.to_str()) == Some("vo.mod"))
        {
            let kind = fs.entry_kind(boundary).map_err(Error::Io)?;
            if kind != FileSystemEntryKind::RegularFile {
                return Err(Error::SourceScan(format!(
                    "module boundary {} must be a regular file without links or special entries; found {kind:?}",
                    boundary.display(),
                )));
            }
            if depth > 0 {
                continue;
            }
        }
        for entry in entries {
            match fs.entry_kind(&entry).map_err(Error::Io)? {
                FileSystemEntryKind::Directory => {
                    if excluded_roots.contains(&entry) {
                        continue;
                    }
                    let name = entry
                        .file_name()
                        .and_then(|value| value.to_str())
                        .unwrap_or("");
                    if name.starts_with('.')
                        || name == "vendor"
                        || name == "testdata"
                        || name == "node_modules"
                        || name == "target"
                        || name == "dist"
                    {
                        continue;
                    }
                    pending.push((normalize_fs_path(&entry), depth + 1));
                }
                FileSystemEntryKind::RegularFile => {
                    if entry.extension().is_some_and(|value| value == "vo") {
                        source_files.insert(entry.clone());
                        scan_external_imports_file_in(fs, &entry, &mut imports)?;
                    }
                }
                kind => {
                    return Err(Error::SourceScan(format!(
                        "workspace source entry {} must be a regular file or directory without links; found {kind:?}",
                        entry.display()
                    )));
                }
            }
        }
    }
    Ok(imports)
}

fn scan_external_imports_file_in<F: FileSystem>(
    fs: &F,
    path: &Path,
    imports: &mut BTreeSet<String>,
) -> Result<(), Error> {
    let content = read_stable_regular_text_file(
        fs,
        path,
        vo_common::vfs::MAX_TEXT_FILE_BYTES,
        "workspace source file",
    )?;
    // Import authorization is a pre-analysis guard. Preserve the parser's
    // recovered import list even when another part of the file is malformed;
    // the normal frontend remains responsible for syntax diagnostics. Any
    // source that compiles successfully has an exact recovered import list.
    let (file, _diagnostics, _) = vo_syntax::parse(&content, 0);
    for import in &file.imports {
        let import_path = import.path.value.clone();
        if classify_import(&import_path)? == ImportClass::External {
            imports.insert(import_path);
        }
    }
    Ok(())
}

fn resolve_path(base: &Path, relative: &str) -> PathBuf {
    let p = Path::new(relative);
    if p.is_absolute() {
        normalize_fs_path(p)
    } else {
        normalize_fs_path(&base.join(p))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io;
    static ENV_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

    #[test]
    fn workspace_manifest_aggregate_budget_is_checked_and_transactional() {
        let mut total = 7usize;
        charge_workspace_manifest_bytes(&mut total, 3, 10).unwrap();
        assert_eq!(total, 10);

        let error = charge_workspace_manifest_bytes(&mut total, 1, 10).unwrap_err();
        assert!(error.to_string().contains("aggregate 10-byte"), "{error}");
        assert_eq!(total, 10);

        let mut overflow = usize::MAX;
        let error = charge_workspace_manifest_bytes(&mut overflow, 1, usize::MAX).unwrap_err();
        assert!(error.to_string().contains("byte count overflow"), "{error}");
        assert_eq!(overflow, usize::MAX);
    }

    #[cfg(any(all(unix, not(target_arch = "wasm32")), windows))]
    static STABLE_READ_HOOK_TEST_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

    #[cfg(any(all(unix, not(target_arch = "wasm32")), windows))]
    struct StableReadHookReset {
        _serial: std::sync::MutexGuard<'static, ()>,
    }

    #[cfg(any(all(unix, not(target_arch = "wasm32")), windows))]
    impl Drop for StableReadHookReset {
        fn drop(&mut self) {
            let hook = STABLE_REGULAR_FILE_READ_TEST_HOOK.get_or_init(Default::default);
            *hook.lock().unwrap_or_else(|error| error.into_inner()) = None;
        }
    }

    #[cfg(any(all(unix, not(target_arch = "wasm32")), windows))]
    fn install_stable_read_hook(
        hook: impl FnMut(StableRegularFileReadTestPhase, &Path) + Send + 'static,
    ) -> StableReadHookReset {
        let serial = STABLE_READ_HOOK_TEST_LOCK
            .lock()
            .unwrap_or_else(|error| error.into_inner());
        let slot = STABLE_REGULAR_FILE_READ_TEST_HOOK.get_or_init(Default::default);
        let mut slot = slot.lock().unwrap_or_else(|error| error.into_inner());
        assert!(slot.is_none(), "stable read test hook is already installed");
        *slot = Some(Box::new(hook));
        drop(slot);
        StableReadHookReset { _serial: serial }
    }

    struct VoworkRestore(Option<std::ffi::OsString>);

    impl VoworkRestore {
        fn capture() -> Self {
            Self(std::env::var_os("VOWORK"))
        }
    }

    impl Drop for VoworkRestore {
        fn drop(&mut self) {
            match self.0.take() {
                Some(value) => std::env::set_var("VOWORK", value),
                None => std::env::remove_var("VOWORK"),
            }
        }
    }

    struct NonChildEntryFs;

    struct OversizedDirectoryFs;

    struct FailingDiscoveryFs;

    struct DriftingVirtualFileFs {
        reads: std::sync::atomic::AtomicUsize,
    }

    struct HostAliasFs {
        root: PathBuf,
    }

    #[cfg(any(all(unix, not(target_arch = "wasm32")), windows))]
    struct ReplacingWorkspaceFs {
        host: RealFs,
        root: PathBuf,
        replace_after_read: PathBuf,
        replace_on_target_resolution: usize,
        target_resolutions: std::sync::atomic::AtomicUsize,
        replaced: std::sync::atomic::AtomicBool,
    }

    #[cfg(any(all(unix, not(target_arch = "wasm32")), windows))]
    impl ReplacingWorkspaceFs {
        fn new(
            root: &Path,
            replace_after_read: impl Into<PathBuf>,
            replace_on_target_resolution: usize,
        ) -> Self {
            assert!(replace_on_target_resolution > 0);
            Self {
                host: RealFs::new(root),
                root: root.to_path_buf(),
                replace_after_read: normalize_fs_path(&replace_after_read.into()),
                replace_on_target_resolution,
                target_resolutions: std::sync::atomic::AtomicUsize::new(0),
                replaced: std::sync::atomic::AtomicBool::new(false),
            }
        }

        fn replace_workspace_parent(&self, path: &Path) -> io::Result<()> {
            use std::sync::atomic::Ordering;

            if normalize_fs_path(path) != self.replace_after_read
                || self.replaced.swap(true, Ordering::SeqCst)
            {
                return Ok(());
            }
            std::fs::rename(self.root.join("config"), self.root.join("config-before"))?;
            std::fs::rename(self.root.join("replacement"), self.root.join("config"))
        }

        fn replace_workspace_parent_after_descriptor_read(&self, path: &Path) -> io::Result<()> {
            use std::sync::atomic::Ordering;

            if normalize_fs_path(path) == self.replace_after_read
                && self.target_resolutions.fetch_add(1, Ordering::SeqCst) + 1
                    >= self.replace_on_target_resolution
            {
                self.replace_workspace_parent(path)?;
            }
            Ok(())
        }
    }

    #[cfg(any(all(unix, not(target_arch = "wasm32")), windows))]
    impl FileSystem for ReplacingWorkspaceFs {
        fn read_file(&self, path: &Path) -> io::Result<String> {
            self.host.read_file(path)
        }

        fn read_bytes(&self, path: &Path) -> io::Result<Vec<u8>> {
            self.host.read_bytes(path)
        }

        fn read_bytes_limited(&self, path: &Path, max_bytes: usize) -> io::Result<Vec<u8>> {
            let bytes = self.host.read_bytes_limited(path, max_bytes)?;
            self.replace_workspace_parent(path)?;
            Ok(bytes)
        }

        fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
            self.host.read_dir(path)
        }

        fn exists(&self, path: &Path) -> bool {
            self.host.exists(path)
        }

        fn is_dir(&self, path: &Path) -> bool {
            self.host.is_dir(path)
        }

        fn entry_kind(&self, path: &Path) -> io::Result<FileSystemEntryKind> {
            self.host.entry_kind(path)
        }

        fn root(&self) -> Option<&Path> {
            Some(self.host.root())
        }

        fn resolve_host_path(&self, path: &Path) -> io::Result<Option<PathBuf>> {
            let resolved = self.host.resolve_host_path(path)?;
            self.replace_workspace_parent_after_descriptor_read(path)?;
            Ok(resolved)
        }
    }

    #[cfg(any(all(unix, not(target_arch = "wasm32")), windows))]
    fn create_test_directory_link(target: &Path, link: &Path) -> bool {
        #[cfg(unix)]
        {
            std::os::unix::fs::symlink(target, link).unwrap();
            true
        }
        #[cfg(windows)]
        {
            match std::os::windows::fs::symlink_dir(target, link) {
                Ok(()) => true,
                Err(error) if error.kind() == io::ErrorKind::PermissionDenied => false,
                Err(error) => panic!("cannot create test directory link: {error}"),
            }
        }
    }

    #[cfg(any(all(unix, not(target_arch = "wasm32")), windows))]
    fn create_test_file_link(target: &Path, link: &Path) -> bool {
        #[cfg(unix)]
        {
            std::os::unix::fs::symlink(target, link).unwrap();
            true
        }
        #[cfg(windows)]
        {
            match std::os::windows::fs::symlink_file(target, link) {
                Ok(()) => true,
                Err(error) if error.kind() == io::ErrorKind::PermissionDenied => false,
                Err(error) => panic!("cannot create test file link: {error}"),
            }
        }
    }

    impl HostAliasFs {
        fn translate(&self, path: &Path) -> PathBuf {
            let path = normalize_fs_path(path);
            for alias in ["one", "two"] {
                if let Ok(suffix) = path.strip_prefix(alias) {
                    return normalize_fs_path(&Path::new("target").join(suffix));
                }
            }
            path
        }

        fn host(&self) -> RealFs {
            RealFs::new(&self.root)
        }
    }

    impl FileSystem for HostAliasFs {
        fn read_file(&self, path: &Path) -> io::Result<String> {
            self.host().read_file(&self.translate(path))
        }

        fn read_bytes(&self, path: &Path) -> io::Result<Vec<u8>> {
            self.host().read_bytes(&self.translate(path))
        }

        fn read_bytes_limited(&self, path: &Path, max_bytes: usize) -> io::Result<Vec<u8>> {
            self.host()
                .read_bytes_limited(&self.translate(path), max_bytes)
        }

        fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
            let logical = normalize_fs_path(path);
            let translated = self.translate(path);
            let entries = self.host().read_dir(&translated)?;
            if logical == translated {
                return Ok(entries);
            }
            entries
                .into_iter()
                .map(|entry| {
                    entry
                        .strip_prefix(&translated)
                        .map(|suffix| normalize_fs_path(&logical.join(suffix)))
                        .map_err(|_| {
                            io::Error::new(
                                io::ErrorKind::InvalidData,
                                format!(
                                    "host alias directory {} returned non-child entry {}",
                                    translated.display(),
                                    entry.display(),
                                ),
                            )
                        })
                })
                .collect()
        }

        fn exists(&self, path: &Path) -> bool {
            self.host().exists(&self.translate(path))
        }

        fn is_dir(&self, path: &Path) -> bool {
            self.host().is_dir(&self.translate(path))
        }

        fn entry_kind(&self, path: &Path) -> io::Result<FileSystemEntryKind> {
            self.host().entry_kind(&self.translate(path))
        }

        fn resolve_host_path(&self, path: &Path) -> io::Result<Option<PathBuf>> {
            self.host().resolve_host_path(&self.translate(path))
        }
    }

    impl FileSystem for NonChildEntryFs {
        fn read_file(&self, _path: &Path) -> io::Result<String> {
            Err(io::Error::new(io::ErrorKind::NotFound, "test file"))
        }

        fn read_bytes(&self, _path: &Path) -> io::Result<Vec<u8>> {
            Err(io::Error::new(io::ErrorKind::NotFound, "test file"))
        }

        fn read_dir(&self, _path: &Path) -> io::Result<Vec<PathBuf>> {
            Ok(vec![PathBuf::from("outside/main.vo")])
        }

        fn exists(&self, _path: &Path) -> bool {
            false
        }

        fn is_dir(&self, _path: &Path) -> bool {
            false
        }
    }

    impl FileSystem for FailingDiscoveryFs {
        fn read_file(&self, _path: &Path) -> io::Result<String> {
            unreachable!("workspace discovery must not read source files")
        }

        fn read_bytes(&self, _path: &Path) -> io::Result<Vec<u8>> {
            unreachable!("workspace discovery must not read source files")
        }

        fn read_dir(&self, _path: &Path) -> io::Result<Vec<PathBuf>> {
            unreachable!("workspace discovery must not enumerate directories")
        }

        fn exists(&self, _path: &Path) -> bool {
            false
        }

        fn is_dir(&self, _path: &Path) -> bool {
            false
        }

        fn root(&self) -> Option<&Path> {
            Some(Path::new("."))
        }

        fn resolve_host_path(&self, _path: &Path) -> io::Result<Option<PathBuf>> {
            Err(io::Error::new(
                io::ErrorKind::PermissionDenied,
                "workspace discovery denied",
            ))
        }
    }

    impl FileSystem for OversizedDirectoryFs {
        fn read_file(&self, _path: &Path) -> io::Result<String> {
            Err(io::Error::new(io::ErrorKind::NotFound, "test file"))
        }

        fn read_bytes(&self, _path: &Path) -> io::Result<Vec<u8>> {
            Err(io::Error::new(io::ErrorKind::NotFound, "test file"))
        }

        fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
            Ok(vec![
                path.join("duplicate");
                MAX_DIRECTORY_ENTRIES.checked_add(1).unwrap()
            ])
        }

        fn exists(&self, _path: &Path) -> bool {
            true
        }

        fn is_dir(&self, _path: &Path) -> bool {
            false
        }
    }

    impl FileSystem for DriftingVirtualFileFs {
        fn read_file(&self, path: &Path) -> io::Result<String> {
            String::from_utf8(self.read_bytes(path)?)
                .map_err(|error| io::Error::new(io::ErrorKind::InvalidData, error))
        }

        fn read_bytes(&self, path: &Path) -> io::Result<Vec<u8>> {
            self.read_bytes_limited(path, usize::MAX)
        }

        fn read_bytes_limited(&self, path: &Path, max_bytes: usize) -> io::Result<Vec<u8>> {
            if normalize_fs_path(path) != Path::new("vo.mod") {
                return Err(io::Error::new(io::ErrorKind::NotFound, "test file"));
            }
            let read = self.reads.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
            let bytes = if read == 0 { b"one" } else { b"two" };
            if bytes.len() > max_bytes {
                return Err(io::Error::new(io::ErrorKind::InvalidData, "size limit"));
            }
            Ok(bytes.to_vec())
        }

        fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
            if normalize_fs_path(path) == Path::new(".") {
                Ok(vec![PathBuf::from("vo.mod")])
            } else {
                Err(io::Error::new(io::ErrorKind::NotFound, "test directory"))
            }
        }

        fn exists(&self, path: &Path) -> bool {
            let path = normalize_fs_path(path);
            path == Path::new(".") || path == Path::new("vo.mod")
        }

        fn is_dir(&self, path: &Path) -> bool {
            normalize_fs_path(path) == Path::new(".")
        }

        fn entry_kind(&self, path: &Path) -> io::Result<FileSystemEntryKind> {
            Ok(match normalize_fs_path(path).as_path() {
                path if path == Path::new(".") => FileSystemEntryKind::Directory,
                path if path == Path::new("vo.mod") => FileSystemEntryKind::RegularFile,
                _ => FileSystemEntryKind::Missing,
            })
        }
    }

    #[test]
    fn test_resolve_path_relative() {
        let base = Path::new("/home/user/project");
        let resolved = resolve_path(base, "../vogui");
        assert_eq!(resolved, PathBuf::from("/home/user/vogui"));
    }

    #[test]
    fn test_resolve_path_absolute() {
        let base = Path::new("/home/user/project");
        let resolved = resolve_path(base, "/opt/vogui");
        assert_eq!(resolved, PathBuf::from("/opt/vogui"));
    }

    #[test]
    fn source_scan_rejects_non_child_directory_entries() {
        let error = scan_external_imports_in(&NonChildEntryFs, Path::new("workspace"))
            .expect_err("directory enumeration must remain scoped to the requested parent");
        assert!(error.to_string().contains("returned a non-child entry"));
    }

    #[test]
    fn source_scan_rejects_oversized_directories_before_sorting() {
        let error = scan_external_imports_in(&OversizedDirectoryFs, Path::new("workspace"))
            .expect_err("directory enumeration must obey the shared entry budget");
        assert!(error.to_string().contains("entry limit"), "{error}");
    }

    #[cfg(unix)]
    #[test]
    fn source_scan_rejects_linked_entries_without_following_them() {
        use std::os::unix::fs::symlink;

        let root = tempfile::tempdir().unwrap();
        let outside = tempfile::tempdir().unwrap();
        std::fs::write(
            outside.path().join("outside.vo"),
            "package outside\nimport \"github.com/acme/hidden\"\n",
        )
        .unwrap();
        symlink(outside.path(), root.path().join("linked")).unwrap();

        let error = scan_external_imports_in(&RealFs::new(root.path()), Path::new("."))
            .expect_err("source authorization must reject linked directory entries");
        assert!(error.to_string().contains("without links"), "{error}");
        assert!(error.to_string().contains("Symlink"), "{error}");
    }

    #[cfg(all(unix, not(target_arch = "wasm32")))]
    #[test]
    fn source_scan_rejects_leaf_rebinding_during_the_source_read() {
        let root = tempfile::tempdir().unwrap();
        let target = root.path().join("main.vo");
        let replacement = root.path().join("replacement.vo");
        let parked_original = root.path().join("main.parked");
        let parked_replacement = root.path().join("replacement.parked");
        std::fs::write(&target, "package main\n").unwrap();
        std::fs::write(
            &replacement,
            "package main\nimport \"github.com/acme/transient\"\n",
        )
        .unwrap();
        let hook_target = normalize_host_stable_regular_file_path(&target);
        let replacement = normalize_host_stable_regular_file_path(&replacement);
        let parked_original = normalize_host_stable_regular_file_path(&parked_original);
        let parked_replacement = normalize_host_stable_regular_file_path(&parked_replacement);
        let mut stage = 0u8;
        let _hook = install_stable_read_hook(move |phase, path| {
            if path != hook_target.as_path() {
                return;
            }
            match (stage, phase) {
                (0, StableRegularFileReadTestPhase::BeforeDescriptorRead) => {
                    std::fs::rename(&hook_target, &parked_original).unwrap();
                    std::fs::rename(&replacement, &hook_target).unwrap();
                    stage = 1;
                }
                (1, StableRegularFileReadTestPhase::AfterDescriptorRead) => {
                    std::fs::rename(&hook_target, &parked_replacement).unwrap();
                    std::fs::rename(&parked_original, &hook_target).unwrap();
                    stage = 2;
                }
                _ => {}
            }
        });

        let error = scan_external_imports_in(&RealFs::new(root.path()), Path::new("."))
            .expect_err("source scan must reject a rebound source generation");

        let detail = error.to_string();
        assert!(detail.contains("workspace source file"), "{detail}");
        assert!(detail.contains("mixed workspace generation"), "{detail}");
    }

    #[test]
    fn test_check_import_covered_by_its_own_module() {
        let importer: ModIdentity = ModulePath::parse("github.com/acme/app").unwrap().into();
        assert!(check_import_covered_by_edges(
            "github.com/acme/app",
            "github.com/acme/app/util",
            &importer,
            &[],
        )
        .is_ok());
    }

    #[test]
    fn test_check_import_covered_by_declared_edge() {
        let importer: ModIdentity = ModulePath::parse("github.com/acme/app").unwrap().into();
        let edges = vec![ModulePath::parse("github.com/vo-lang/vogui").unwrap()];
        assert!(check_import_covered_by_edges(
            "github.com/acme/app",
            "github.com/vo-lang/vogui/widget",
            &importer,
            &edges,
        )
        .is_ok());
    }

    #[test]
    fn test_check_import_rejects_an_undeclared_edge() {
        let importer: ModIdentity = ModulePath::parse("github.com/acme/app").unwrap().into();
        assert!(check_import_covered_by_edges(
            "github.com/acme/app",
            "github.com/acme/newdep/pkg",
            &importer,
            &[],
        )
        .is_err());
    }

    #[test]
    fn candidate_discovery_honors_vowork_off() {
        let _guard = ENV_LOCK.lock().unwrap();
        let _restore = VoworkRestore::capture();
        std::env::set_var("VOWORK", "off");

        let mut fs = vo_common::vfs::MemoryFs::new();
        fs.add_file("workspace/vo.work", "format = 1\nmembers = [\".\"]\n");
        fs.add_file(
            "workspace/vo.mod",
            "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        );

        let root: ModIdentity = ModulePath::parse("github.com/acme/app").unwrap().into();
        let result = discover_workspace_candidates_in_with(
            &fs,
            Path::new("workspace"),
            Some(&root),
            &workspace_discovery_from_environment(),
        )
        .unwrap();

        assert!(result.is_empty());
    }

    #[test]
    fn workspace_discovery_environment_supports_auto_disabled_and_explicit() {
        let _guard = ENV_LOCK.lock().unwrap();
        let _restore = VoworkRestore::capture();

        std::env::remove_var("VOWORK");
        assert_eq!(
            workspace_discovery_from_environment(),
            WorkspaceDiscovery::Auto
        );

        std::env::set_var("VOWORK", "off");
        assert_eq!(
            workspace_discovery_from_environment(),
            WorkspaceDiscovery::Disabled
        );

        std::env::set_var("VOWORK", "config/selected.vo.work");
        assert_eq!(
            workspace_discovery_from_environment(),
            WorkspaceDiscovery::Explicit(PathBuf::from("config/selected.vo.work"))
        );
    }

    #[test]
    fn automatic_workspace_discovery_does_not_absorb_memoryfs_root_from_parent_start() {
        let mut fs = vo_common::vfs::MemoryFs::new();
        fs.add_file("vo.work", "format = 1\nmembers = []\n");

        let selected =
            discover_workfile_in_with(&fs, Path::new("../outside"), &WorkspaceDiscovery::Auto)
                .unwrap();

        assert_eq!(selected, None);
    }

    #[test]
    fn automatic_workspace_discovery_does_not_absorb_scoped_realfs_root_from_parent_start() {
        let temp = tempfile::tempdir().unwrap();
        std::fs::write(temp.path().join("vo.work"), "format = 1\nmembers = []\n").unwrap();

        let selected = discover_workfile_in_with(
            &RealFs::new(temp.path()),
            Path::new("../outside"),
            &WorkspaceDiscovery::Auto,
        )
        .unwrap();

        assert_eq!(selected, None);
    }

    #[test]
    fn automatic_workspace_discovery_propagates_start_normalization_failures() {
        let error = discover_workfile_in_with(
            &FailingDiscoveryFs,
            Path::new("../outside"),
            &WorkspaceDiscovery::Auto,
        )
        .expect_err("discovery-start failures must remain observable");

        assert!(error.to_string().contains("workspace discovery denied"));
    }

    #[test]
    fn automatic_workspace_discovery_rejects_child_directory_masking_parent_workfile() {
        let fs = vo_common::vfs::MemoryFs::new()
            .with_file("repo/vo.work", "format = 1\nmembers = []\n")
            .with_dir("repo/child/vo.work");

        let error =
            discover_workfile_in_with(&fs, Path::new("repo/child"), &WorkspaceDiscovery::Auto)
                .expect_err("a child vo.work directory must block parent discovery");
        let detail = error.to_string();
        assert!(detail.contains("repo/child/vo.work"), "{detail}");
        assert!(detail.contains("Directory"), "{detail}");
    }

    #[test]
    fn automatic_workspace_discovery_rejects_portable_workfile_aliases() {
        let fs = vo_common::vfs::MemoryFs::new()
            .with_file("repo/vo.work", "format = 1\nmembers = []\n")
            .with_file("repo/child/VO.WORK", "alias")
            .with_dir("repo/child/grandchild");

        let error = discover_workfile_in_with(
            &fs,
            Path::new("repo/child/grandchild"),
            &WorkspaceDiscovery::Auto,
        )
        .expect_err("a child portable alias must block parent workspace discovery");
        let detail = error.to_string();
        assert!(detail.contains("portable alias"), "{detail}");
        assert!(detail.contains("VO.WORK"), "{detail}");
    }

    #[cfg(any(all(unix, not(target_arch = "wasm32")), windows))]
    #[test]
    fn stable_regular_text_reader_rejects_a_linked_leaf() {
        let temp = tempfile::tempdir().unwrap();
        std::fs::write(
            temp.path().join("real.vo.mod"),
            "format = 1\nmodule = \"local/test\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        if !create_test_file_link(
            Path::new("real.vo.mod"),
            &temp.path().join("selected.vo.mod"),
        ) {
            return;
        }

        let error = read_stable_regular_text_file(
            &RealFs::new(temp.path()),
            Path::new("selected.vo.mod"),
            vo_common::vfs::MAX_TEXT_FILE_BYTES,
            "root module manifest",
        )
        .unwrap_err();

        let detail = error.to_string();
        assert!(detail.contains("root module manifest"), "{detail}");
        assert!(detail.contains("RegularFile"), "{detail}");
        assert!(
            detail.contains("Symlink") || detail.contains("reparse"),
            "{detail}"
        );
    }

    #[cfg(any(all(unix, not(target_arch = "wasm32")), windows))]
    #[test]
    fn stable_regular_text_reader_rejects_a_linked_parent() {
        let temp = tempfile::tempdir().unwrap();
        std::fs::create_dir(temp.path().join("real")).unwrap();
        std::fs::write(
            temp.path().join("real/vo.mod"),
            "format = 1\nmodule = \"local/test\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        if !create_test_directory_link(Path::new("real"), &temp.path().join("linked")) {
            return;
        }

        let error = read_stable_regular_text_file(
            &RealFs::new(temp.path()),
            Path::new("linked/vo.mod"),
            vo_common::vfs::MAX_TEXT_FILE_BYTES,
            "root module manifest",
        )
        .unwrap_err();

        let detail = error.to_string();
        assert!(detail.contains("root module manifest parent"), "{detail}");
        assert!(
            detail.contains("Symlink") || detail.contains("reparse"),
            "{detail}"
        );
    }

    #[test]
    fn stable_regular_bytes_reader_rejects_virtual_content_drift() {
        let fs = DriftingVirtualFileFs {
            reads: std::sync::atomic::AtomicUsize::new(0),
        };

        let error = read_stable_regular_file_bytes(
            &fs,
            Path::new("vo.mod"),
            vo_common::vfs::MAX_TEXT_FILE_BYTES,
            "root module manifest",
        )
        .unwrap_err();

        let detail = error.to_string();
        assert!(detail.contains("root module manifest"), "{detail}");
        assert!(detail.contains("mixed workspace generation"), "{detail}");
    }

    #[test]
    fn stable_regular_bytes_reader_preserves_virtual_absolute_alias_names() {
        let fs = vo_common::vfs::MemoryFs::new()
            .with_bytes("/var/virtual/vo.mod", b"virtual-var".to_vec());

        let bytes = read_stable_regular_file_bytes(
            &fs,
            Path::new("/var/virtual/vo.mod"),
            vo_common::vfs::MAX_TEXT_FILE_BYTES,
            "root module manifest",
        )
        .unwrap();

        assert_eq!(bytes, b"virtual-var");
    }

    #[cfg(any(all(unix, not(target_arch = "wasm32")), windows))]
    #[test]
    fn stable_regular_bytes_reader_rejects_same_size_in_place_content_drift() {
        let temp = tempfile::tempdir().unwrap();
        let target = temp.path().join("vo.mod");
        std::fs::write(&target, b"generation-a").unwrap();
        let hook_target = normalize_host_stable_regular_file_path(&target);
        let mut changed = false;
        let _hook = install_stable_read_hook(move |phase, path| {
            if !changed
                && phase == StableRegularFileReadTestPhase::AfterDescriptorRead
                && path == hook_target.as_path()
            {
                std::fs::write(path, b"generation-b").unwrap();
                changed = true;
            }
        });

        let error = read_stable_regular_file_bytes(
            &RealFs::new("."),
            &target,
            vo_common::vfs::MAX_TEXT_FILE_BYTES,
            "root module manifest",
        )
        .unwrap_err();

        let detail = error.to_string();
        assert!(detail.contains("root module manifest"), "{detail}");
        assert!(detail.contains("mixed workspace generation"), "{detail}");
    }

    #[cfg(all(unix, not(target_arch = "wasm32")))]
    #[test]
    fn stable_regular_bytes_reader_rejects_leaf_aba_while_descriptor_is_open() {
        let temp = tempfile::tempdir().unwrap();
        let target = temp.path().join("vo.mod");
        let replacement = temp.path().join("replacement.vo.mod");
        let parked_original = temp.path().join("original.parked");
        let parked_replacement = temp.path().join("replacement.parked");
        std::fs::write(&target, b"generation-a").unwrap();
        std::fs::write(&replacement, b"generation-b").unwrap();
        let hook_target = normalize_host_stable_regular_file_path(&target);
        let replacement = normalize_host_stable_regular_file_path(&replacement);
        let parked_original = normalize_host_stable_regular_file_path(&parked_original);
        let parked_replacement = normalize_host_stable_regular_file_path(&parked_replacement);
        let mut stage = 0u8;
        let _hook = install_stable_read_hook(move |phase, path| {
            if path != hook_target.as_path() {
                return;
            }
            match (stage, phase) {
                (0, StableRegularFileReadTestPhase::BeforeDescriptorRead) => {
                    std::fs::rename(&hook_target, &parked_original).unwrap();
                    std::fs::rename(&replacement, &hook_target).unwrap();
                    stage = 1;
                }
                (1, StableRegularFileReadTestPhase::AfterDescriptorRead) => {
                    std::fs::rename(&hook_target, &parked_replacement).unwrap();
                    std::fs::rename(&parked_original, &hook_target).unwrap();
                    stage = 2;
                }
                _ => {}
            }
        });

        let error = read_stable_regular_file_bytes(
            &RealFs::new("."),
            &target,
            vo_common::vfs::MAX_TEXT_FILE_BYTES,
            "root module manifest",
        )
        .unwrap_err();

        let detail = error.to_string();
        assert!(detail.contains("root module manifest"), "{detail}");
        assert!(detail.contains("mixed workspace generation"), "{detail}");
        assert_eq!(std::fs::read(&target).unwrap(), b"generation-a");
    }

    #[cfg(all(unix, not(target_arch = "wasm32")))]
    #[test]
    fn stable_regular_bytes_reader_rejects_fifo_without_blocking() {
        use std::ffi::CString;
        use std::os::unix::ffi::OsStrExt;

        let temp = tempfile::tempdir().unwrap();
        let fifo = temp.path().join("manifest.pipe");
        let fifo_name = CString::new(fifo.as_os_str().as_bytes()).unwrap();
        let result = unsafe { libc::mkfifo(fifo_name.as_ptr(), 0o600) };
        assert_eq!(
            result,
            0,
            "cannot create FIFO: {}",
            io::Error::last_os_error()
        );

        let error = read_stable_regular_file_bytes(
            &RealFs::new("."),
            &fifo,
            vo_common::vfs::MAX_TEXT_FILE_BYTES,
            "root module manifest",
        )
        .unwrap_err();

        let detail = error.to_string();
        assert!(detail.contains("root module manifest"), "{detail}");
        assert!(detail.contains("Special"), "{detail}");
    }

    #[cfg(any(all(unix, not(target_arch = "wasm32")), windows))]
    #[test]
    fn explicit_vowork_rejects_a_linked_intermediate_directory_from_namespace_root() {
        let temp = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(temp.path().join("config/real")).unwrap();
        std::fs::write(
            temp.path().join("config/real/selected.vo.work"),
            "format = 1\nmembers = []\n",
        )
        .unwrap();
        if !create_test_directory_link(Path::new("real"), &temp.path().join("config/link")) {
            return;
        }

        let error = discover_workfile_in_with(
            &RealFs::new(temp.path()),
            Path::new("."),
            &WorkspaceDiscovery::Explicit(PathBuf::from("config/link/selected.vo.work")),
        )
        .unwrap_err();

        let detail = error.to_string();
        assert!(detail.contains("workspace file parent"), "{detail}");
        assert!(
            detail.contains("Symlink") || detail.contains("reparse"),
            "{detail}"
        );
    }

    #[cfg(any(all(unix, not(target_arch = "wasm32")), windows))]
    #[test]
    fn explicit_vowork_rejects_a_linked_workfile_leaf() {
        let temp = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(temp.path().join("config")).unwrap();
        std::fs::write(
            temp.path().join("config/real.vo.work"),
            "format = 1\nmembers = []\n",
        )
        .unwrap();
        if !create_test_file_link(
            Path::new("real.vo.work"),
            &temp.path().join("config/selected.vo.work"),
        ) {
            return;
        }

        let error = discover_workfile_in_with(
            &RealFs::new(temp.path()),
            Path::new("."),
            &WorkspaceDiscovery::Explicit(PathBuf::from("config/selected.vo.work")),
        )
        .unwrap_err();

        let detail = error.to_string();
        assert!(detail.contains("RegularFile"), "{detail}");
        assert!(
            detail.contains("Symlink") || detail.contains("reparse"),
            "{detail}"
        );
    }

    #[cfg(any(all(unix, not(target_arch = "wasm32")), windows))]
    #[test]
    fn automatic_vowork_rejects_a_selected_file_below_a_linked_parent() {
        let temp = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(temp.path().join("real/project")).unwrap();
        std::fs::write(
            temp.path().join("real/vo.work"),
            "format = 1\nmembers = []\n",
        )
        .unwrap();
        if !create_test_directory_link(Path::new("real"), &temp.path().join("linked")) {
            return;
        }

        let error = discover_workfile_in_with(
            &RealFs::new(temp.path()),
            Path::new("linked/project"),
            &WorkspaceDiscovery::Auto,
        )
        .unwrap_err();

        let detail = error.to_string();
        assert!(detail.contains("workspace file parent"), "{detail}");
        assert!(
            detail.contains("Symlink") || detail.contains("reparse"),
            "{detail}"
        );
    }

    #[cfg(any(all(unix, not(target_arch = "wasm32")), windows))]
    #[test]
    fn workspace_rejects_parent_path_replacement_while_reading_workfile() {
        let temp = tempfile::tempdir().unwrap();
        for directory in ["config", "replacement"] {
            std::fs::create_dir(temp.path().join(directory)).unwrap();
            std::fs::write(
                temp.path().join(directory).join("selected.vo.work"),
                "format = 1\nmembers = []\n",
            )
            .unwrap();
        }
        let fs = ReplacingWorkspaceFs::new(temp.path(), "config/selected.vo.work", 3);

        let error = load_workspace_members_in_with_provenance(
            &fs,
            Path::new("."),
            &WorkspaceDiscovery::Explicit(PathBuf::from("config/selected.vo.work")),
        )
        .unwrap_err();

        let detail = error.to_string();
        assert!(detail.contains("changed identity"), "{detail}");
        assert!(detail.contains("mixed workspace generation"), "{detail}");
    }

    #[cfg(any(all(unix, not(target_arch = "wasm32")), windows))]
    #[test]
    fn workspace_rejects_workfile_parent_replacement_while_loading_members() {
        let temp = tempfile::tempdir().unwrap();
        let workfile = "format = 1\nmembers = [\"member\"]\n";
        for (directory, module) in [
            ("config", "github.com/acme/original"),
            ("replacement", "github.com/acme/replacement"),
        ] {
            std::fs::create_dir_all(temp.path().join(directory).join("member")).unwrap();
            std::fs::write(
                temp.path().join(directory).join("selected.vo.work"),
                workfile,
            )
            .unwrap();
            std::fs::write(
                temp.path().join(directory).join("member/vo.mod"),
                format!("format = 1\nmodule = \"{module}\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n"),
            )
            .unwrap();
        }
        let fs = ReplacingWorkspaceFs::new(temp.path(), "config/member/vo.mod", 2);

        let error = load_workspace_members_in_with_provenance(
            &fs,
            Path::new("."),
            &WorkspaceDiscovery::Explicit(PathBuf::from("config/selected.vo.work")),
        )
        .unwrap_err();

        let detail = error.to_string();
        assert!(detail.contains("changed identity"), "{detail}");
        assert!(detail.contains("mixed workspace generation"), "{detail}");
    }

    #[cfg(any(all(unix, not(target_arch = "wasm32")), windows))]
    #[test]
    fn workspace_member_rejects_equivalent_directory_rebinding_after_discovery() {
        let temp = tempfile::tempdir().unwrap();
        let workspace = temp.path().join("workspace");
        std::fs::create_dir_all(workspace.join("app")).unwrap();
        std::fs::write(
            workspace.join("vo.work"),
            "format = 1\nmembers = [\"app\", \"lib\"]\n",
        )
        .unwrap();
        std::fs::write(
            workspace.join("app/vo.mod"),
            "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        )
        .unwrap();
        for directory in ["lib", "lib-next"] {
            std::fs::create_dir(workspace.join(directory)).unwrap();
            std::fs::write(
                workspace.join(directory).join("vo.mod"),
                "format = 1\nmodule = \"github.com/acme/lib\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
            )
            .unwrap();
            std::fs::write(
                workspace.join(directory).join("lib.vo"),
                "package lib\n\npub fn Value() int { return 1 }\n",
            )
            .unwrap();
        }

        let fs = RealFs::new(temp.path());
        let root: ModIdentity = ModulePath::parse("github.com/acme/app").unwrap().into();
        let members = discover_workspace_candidates_in_with(
            &fs,
            Path::new("workspace/app"),
            Some(&root),
            &WorkspaceDiscovery::Auto,
        )
        .unwrap();
        assert_eq!(members.len(), 1);
        let discovered_generation = members[0].directory_generation_key();

        std::fs::rename(workspace.join("lib"), workspace.join("lib-before")).unwrap();
        std::fs::rename(workspace.join("lib-next"), workspace.join("lib")).unwrap();

        let error = members[0]
            .validate_directory_generation(&fs)
            .expect_err("source authorization must reject a rebound workspace member");
        let detail = error.to_string();
        assert!(detail.contains("github.com/acme/lib"), "{detail}");
        assert!(detail.contains("changed identity"), "{detail}");
        assert!(detail.contains("mixed workspace generation"), "{detail}");

        let refreshed = discover_workspace_candidates_in_with(
            &fs,
            Path::new("workspace/app"),
            Some(&root),
            &WorkspaceDiscovery::Auto,
        )
        .unwrap();
        assert_eq!(refreshed.len(), 1);
        assert_ne!(
            refreshed[0].directory_generation_key(),
            discovered_generation
        );
        refreshed[0].validate_directory_generation(&fs).unwrap();
    }

    #[test]
    fn selected_workfile_generation_revalidates_exact_content_and_identity() {
        let mut fs = vo_common::vfs::MemoryFs::new();
        fs.add_file("workspace/vo.work", "format = 1\nmembers = [\"app\"]\n");
        fs.add_file(
            "workspace/app/vo.mod",
            "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        );
        let root: ModIdentity = ModulePath::parse("github.com/acme/app").unwrap().into();
        let (generation, members) = discover_workspace_candidates_in_with_generation(
            &fs,
            Path::new("workspace/app"),
            Some(&root),
            &WorkspaceDiscovery::Auto,
        )
        .unwrap();
        assert!(members.is_empty());
        let generation = generation.unwrap();
        generation.validate(&fs).unwrap();
        let original_key = generation.generation_key();

        fs.add_file(
            "workspace/vo.work",
            "format = 1\nmembers = [\"app\"]\n# changed\n",
        );
        let error = generation.validate(&fs).unwrap_err();
        assert!(
            error.to_string().contains("mixed workspace generation"),
            "{error}"
        );

        let (replacement, _) = discover_workspace_candidates_in_with_generation(
            &fs,
            Path::new("workspace/app"),
            Some(&root),
            &WorkspaceDiscovery::Auto,
        )
        .unwrap();
        assert_ne!(replacement.unwrap().generation_key(), original_key);
    }

    #[test]
    fn candidate_discovery_with_disabled_ignores_vo_work() {
        let mut fs = vo_common::vfs::MemoryFs::new();
        fs.add_file("workspace/vo.work", "format = 1\nmembers = [\".\"]\n");
        fs.add_file(
            "workspace/vo.mod",
            "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        );

        let root: ModIdentity = ModulePath::parse("github.com/acme/app").unwrap().into();
        let result = discover_workspace_candidates_in_with(
            &fs,
            Path::new("workspace"),
            Some(&root),
            &WorkspaceDiscovery::Disabled,
        )
        .unwrap();

        assert!(result.is_empty());
    }

    #[test]
    fn candidate_discovery_only_parses_root_identity_for_an_active_workspace() {
        let mut fs = vo_common::vfs::MemoryFs::new();
        fs.add_file("workspace/vo.mod", "invalid\n");
        let candidates = discover_workspace_candidates_in_with(
            &fs,
            Path::new("workspace"),
            None,
            &WorkspaceDiscovery::Auto,
        )
        .unwrap();
        assert!(candidates.is_empty());

        fs.add_file("workspace/vo.work", "format = 1\nmembers = []\n");
        assert!(discover_workspace_candidates_in_with(
            &fs,
            Path::new("workspace"),
            None,
            &WorkspaceDiscovery::Auto,
        )
        .is_err());
    }

    #[cfg(all(unix, not(target_arch = "wasm32")))]
    #[test]
    fn workspace_member_discovery_rejects_vo_mod_leaf_rebinding() {
        let root = tempfile::tempdir().unwrap();
        let member = root.path().join("lib");
        std::fs::create_dir(&member).unwrap();
        std::fs::write(
            root.path().join("vo.work"),
            "format = 1\nmembers = [\"lib\"]\n",
        )
        .unwrap();
        let target = member.join("vo.mod");
        let replacement = member.join("replacement.vo.mod");
        let parked_original = member.join("original.parked");
        let parked_replacement = member.join("replacement.parked");
        std::fs::write(
            &target,
            "format = 1\nmodule = \"github.com/acme/lib\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        )
        .unwrap();
        std::fs::write(
            &replacement,
            "format = 1\nmodule = \"github.com/acme/transient\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        )
        .unwrap();
        let hook_target = normalize_host_stable_regular_file_path(&target);
        let replacement = normalize_host_stable_regular_file_path(&replacement);
        let parked_original = normalize_host_stable_regular_file_path(&parked_original);
        let parked_replacement = normalize_host_stable_regular_file_path(&parked_replacement);
        let mut stage = 0u8;
        let _hook = install_stable_read_hook(move |phase, path| {
            if path != hook_target.as_path() {
                return;
            }
            match (stage, phase) {
                (0, StableRegularFileReadTestPhase::BeforeDescriptorRead) => {
                    std::fs::rename(&hook_target, &parked_original).unwrap();
                    std::fs::rename(&replacement, &hook_target).unwrap();
                    stage = 1;
                }
                (1, StableRegularFileReadTestPhase::AfterDescriptorRead) => {
                    std::fs::rename(&hook_target, &parked_replacement).unwrap();
                    std::fs::rename(&parked_original, &hook_target).unwrap();
                    stage = 2;
                }
                _ => {}
            }
        });

        let error = load_workspace_members_in_with_provenance(
            &RealFs::new(root.path()),
            Path::new("."),
            &WorkspaceDiscovery::Auto,
        )
        .unwrap_err();

        let detail = error.to_string();
        assert!(detail.contains("members[0] vo.mod"), "{detail}");
        assert!(detail.contains("mixed workspace generation"), "{detail}");
    }

    #[test]
    fn workspace_members_derive_identity_and_exclude_the_active_module() {
        let mut fs = vo_common::vfs::MemoryFs::new();
        fs.add_file(
            "workspace/vo.work",
            "format = 1\nmembers = [\"app\", \"lib\"]\n",
        );
        fs.add_file(
            "workspace/app/vo.mod",
            "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        );
        fs.add_file(
            "workspace/lib/vo.mod",
            "format = 1\nmodule = \"github.com/acme/lib\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        );
        let root: ModIdentity = ModulePath::parse("github.com/acme/app").unwrap().into();

        let (selected, members) = load_workspace_members_in_with_provenance(
            &fs,
            Path::new("workspace/app"),
            &WorkspaceDiscovery::Auto,
        )
        .unwrap();
        assert_eq!(selected, Some(PathBuf::from("workspace/vo.work")));
        assert_eq!(members.len(), 2);
        assert_eq!(members[0].module.as_str(), "github.com/acme/app");
        assert_eq!(members[1].module.as_str(), "github.com/acme/lib");

        let sources = discover_workspace_candidates_in_with(
            &fs,
            Path::new("workspace/app"),
            Some(&root),
            &WorkspaceDiscovery::Auto,
        )
        .unwrap();
        assert_eq!(sources.len(), 1);
        assert_eq!(sources[0].module.as_str(), "github.com/acme/lib");
        assert_eq!(sources[0].local_dir, PathBuf::from("workspace/lib"));

        let sources_without_identity = discover_workspace_candidates_in_with(
            &fs,
            Path::new("workspace/app"),
            None,
            &WorkspaceDiscovery::Auto,
        )
        .unwrap();
        assert_eq!(sources_without_identity, sources);
    }

    #[test]
    fn real_workspace_rejects_duplicate_module_identities_including_the_active_root() {
        let temp = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(temp.path().join("app")).unwrap();
        std::fs::create_dir_all(temp.path().join("app-copy")).unwrap();
        std::fs::write(
            temp.path().join("vo.work"),
            "format = 1\nmembers = [\"app\", \"app-copy\"]\n",
        )
        .unwrap();
        let manifest =
            "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n";
        std::fs::write(temp.path().join("app/vo.mod"), manifest).unwrap();
        std::fs::write(temp.path().join("app-copy/vo.mod"), manifest).unwrap();
        let root: ModIdentity = ModulePath::parse("github.com/acme/app").unwrap().into();

        let error = discover_workspace_candidates_in_with(
            &RealFs::new(temp.path()),
            Path::new("app"),
            Some(&root),
            &WorkspaceDiscovery::Auto,
        )
        .unwrap_err();

        assert!(matches!(&error, Error::WorkFileParse(_)));
        let detail = error.to_string();
        assert!(detail.contains("duplicate workspace module"), "{detail}");
        assert!(detail.contains("github.com/acme/app"), "{detail}");
    }

    #[test]
    fn host_alias_of_the_active_directory_is_filtered_by_physical_identity() {
        let temp = tempfile::tempdir().unwrap();
        std::fs::create_dir(temp.path().join("target")).unwrap();
        std::fs::write(
            temp.path().join("vo.work"),
            "format = 1\nmembers = [\"two\"]\n",
        )
        .unwrap();
        std::fs::write(
            temp.path().join("target/vo.mod"),
            "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        )
        .unwrap();
        let fs = HostAliasFs {
            root: temp.path().to_path_buf(),
        };
        let root: ModIdentity = ModulePath::parse("github.com/acme/app").unwrap().into();

        let members = discover_workspace_candidates_in_with(
            &fs,
            Path::new("one"),
            Some(&root),
            &WorkspaceDiscovery::Auto,
        )
        .unwrap();

        assert!(members.is_empty());
    }

    #[test]
    fn nested_active_root_is_a_source_boundary_for_its_parent_member() {
        let mut fs = vo_common::vfs::MemoryFs::new();
        fs.add_file(
            "workspace/vo.work",
            "format = 1\nmembers = [\"parent\", \"parent/app\"]\n",
        );
        fs.add_file(
            "workspace/parent/vo.mod",
            "format = 1\nmodule = \"github.com/acme/parent\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        );
        fs.add_file("workspace/parent/lib.vo", "package parent\n");
        fs.add_file(
            "workspace/parent/app/vo.mod",
            concat!(
                "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\n",
                "vo = \"0.1.0\"\n",
                "[dependencies]\n",
                "\"github.com/acme/parent\" = \"^1.0.0\"\n",
                "\"github.com/acme/root-only\" = \"^1.0.0\"\n",
            ),
        );
        fs.add_file(
            "workspace/parent/app/main.vo",
            "package main\nimport \"github.com/acme/root-only/package\"\n",
        );
        let root_mod = ModFile::parse(
            "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n[dependencies]\n\"github.com/acme/parent\" = \"^1.0.0\"\n\"github.com/acme/root-only\" = \"^1.0.0\"\n",
        )
        .unwrap();
        let (_, members) = load_workspace_members_in_with_provenance(
            &fs,
            Path::new("workspace/parent/app"),
            &WorkspaceDiscovery::Auto,
        )
        .unwrap();

        let inputs = validate_project_external_imports(
            &fs,
            Path::new("workspace/parent/app"),
            &root_mod,
            &members,
            &members,
            None,
        )
        .unwrap();

        assert_eq!(
            inputs,
            [
                PathBuf::from("workspace/parent/app/main.vo"),
                PathBuf::from("workspace/parent/lib.vo"),
            ]
        );
    }

    #[test]
    fn unselected_nested_modules_are_source_boundaries_for_edge_validation() {
        let mut fs = vo_common::vfs::MemoryFs::new();
        fs.add_file(
            "project/vo.mod",
            "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        );
        fs.add_file("project/main.vo", "package main\n");
        fs.add_file(
            "project/nested/vo.mod",
            "format = 1\nmodule = \"github.com/acme/nested\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        );
        fs.add_file(
            "project/nested/main.vo",
            "package nested\nimport \"github.com/acme/hidden/pkg\"\n",
        );
        let root_mod = ModFile::parse(
            "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        )
        .unwrap();

        let inputs =
            validate_project_external_imports(&fs, Path::new("project"), &root_mod, &[], &[], None)
                .unwrap();
        assert_eq!(inputs, [PathBuf::from("project/main.vo")]);

        fs.add_file("project/alias/VO.MOD", "alias");
        let error =
            validate_project_external_imports(&fs, Path::new("project"), &root_mod, &[], &[], None)
                .unwrap_err();
        assert!(error.to_string().contains("portable alias"), "{error}");
        assert!(error.to_string().contains("VO.MOD"), "{error}");
    }

    #[test]
    fn workspace_rejects_duplicate_member_paths_and_module_identities() {
        let mut duplicate_path = vo_common::vfs::MemoryFs::new();
        duplicate_path.add_file(
            "workspace/vo.work",
            "format = 1\nmembers = [\"lib\", \"lib\"]\n",
        );
        duplicate_path.add_file(
            "workspace/lib/vo.mod",
            "format = 1\nmodule = \"github.com/acme/lib\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        );
        let error = load_workspace_members_in_with_provenance(
            &duplicate_path,
            Path::new("workspace"),
            &WorkspaceDiscovery::Auto,
        )
        .unwrap_err();
        assert!(error.to_string().contains("duplicate member path"));

        let mut duplicate_module = vo_common::vfs::MemoryFs::new();
        duplicate_module.add_file(
            "workspace/vo.work",
            "format = 1\nmembers = [\"one\", \"two\"]\n",
        );
        for directory in ["one", "two"] {
            duplicate_module.add_file(
                format!("workspace/{directory}/vo.mod"),
                "format = 1\nmodule = \"github.com/acme/lib\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
            );
        }
        let error = load_workspace_members_in_with_provenance(
            &duplicate_module,
            Path::new("workspace"),
            &WorkspaceDiscovery::Auto,
        )
        .unwrap_err();
        assert!(error.to_string().contains("duplicate workspace module"));
    }

    #[test]
    fn workspace_rejects_distinct_paths_with_the_same_host_directory_identity() {
        let temp = tempfile::tempdir().unwrap();
        std::fs::create_dir(temp.path().join("target")).unwrap();
        std::fs::write(
            temp.path().join("vo.work"),
            "format = 1\nmembers = [\"one\", \"two\"]\n",
        )
        .unwrap();
        std::fs::write(
            temp.path().join("target/vo.mod"),
            "format = 1\nmodule = \"github.com/acme/lib\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        )
        .unwrap();
        let fs = HostAliasFs {
            root: temp.path().to_path_buf(),
        };

        let error = load_workspace_members_in_with_provenance(
            &fs,
            Path::new("."),
            &WorkspaceDiscovery::Auto,
        )
        .unwrap_err();
        assert!(error.to_string().contains("same real directory"), "{error}");
    }

    #[cfg(any(unix, windows))]
    #[test]
    fn workspace_rejects_a_symbolic_link_in_the_middle_of_a_member_path() {
        let temp = tempfile::tempdir().unwrap();
        let workspace = temp.path().join("workspace");
        std::fs::create_dir_all(workspace.join("real/member")).unwrap();
        std::fs::write(
            workspace.join("vo.work"),
            "format = 1\nmembers = [\"bridge/member\"]\n",
        )
        .unwrap();
        std::fs::write(
            workspace.join("real/member/vo.mod"),
            "format = 1\nmodule = \"github.com/acme/lib\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        )
        .unwrap();
        #[cfg(unix)]
        std::os::unix::fs::symlink("real", workspace.join("bridge")).unwrap();
        #[cfg(windows)]
        if let Err(error) = std::os::windows::fs::symlink_dir("real", workspace.join("bridge")) {
            if error.kind() == io::ErrorKind::PermissionDenied {
                return;
            }
            panic!("cannot create test directory link: {error}");
        }

        let fs = RealFs::new(temp.path());
        let error = load_workspace_members_in_with_provenance(
            &fs,
            Path::new("workspace"),
            &WorkspaceDiscovery::Auto,
        )
        .unwrap_err();
        let detail = error.to_string();
        assert!(detail.contains("component"), "{detail}");
        assert!(detail.contains("Symlink"), "{detail}");
    }

    #[test]
    fn workspace_member_rejects_parent_components_before_filesystem_access() {
        let temp = tempfile::tempdir().unwrap();
        std::fs::write(
            temp.path().join("vo.work"),
            "format = 1\nmembers = [\"../outside\"]\n",
        )
        .unwrap();
        let fs = RealFs::new(temp.path());

        let error = load_workspace_members_in_with_provenance(
            &fs,
            Path::new("."),
            &WorkspaceDiscovery::Auto,
        )
        .unwrap_err();
        assert!(error.to_string().contains("parent components"), "{error}");
    }

    #[test]
    fn workspace_rejects_old_use_tables_and_non_directory_members() {
        let mut old_wire = vo_common::vfs::MemoryFs::new();
        old_wire.add_file(
            "workspace/vo.work",
            "version = 1\n[[use]]\npath = \"lib\"\n",
        );
        assert!(load_workspace_members_in_with_provenance(
            &old_wire,
            Path::new("workspace"),
            &WorkspaceDiscovery::Auto,
        )
        .is_err());

        let mut regular_file = vo_common::vfs::MemoryFs::new();
        regular_file.add_file("workspace/vo.work", "format = 1\nmembers = [\"lib\"]\n");
        regular_file.add_file("workspace/lib", "not a directory");
        let error = load_workspace_members_in_with_provenance(
            &regular_file,
            Path::new("workspace"),
            &WorkspaceDiscovery::Auto,
        )
        .unwrap_err();
        assert!(error.to_string().contains("must be Directory"));
    }

    #[test]
    fn workspace_requires_member_manifests_and_accepts_local_modules() {
        let mut missing_manifest = vo_common::vfs::MemoryFs::new();
        missing_manifest.add_file(
            "workspace/vo.work",
            "format = 1\nmembers = [\"app\", \"lib\"]\n",
        );
        missing_manifest.add_file(
            "workspace/app/vo.mod",
            "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        );
        missing_manifest.add_file("workspace/lib/placeholder", "");
        let error = load_workspace_members_in_with_provenance(
            &missing_manifest,
            Path::new("workspace/app"),
            &WorkspaceDiscovery::Auto,
        )
        .unwrap_err();
        assert!(error.to_string().contains("vo.mod"), "{error}");

        let mut ephemeral = vo_common::vfs::MemoryFs::new();
        ephemeral.add_file(
            "workspace/vo.work",
            "format = 1\nmembers = [\"app\", \"scratch\"]\n",
        );
        ephemeral.add_file(
            "workspace/app/vo.mod",
            "format = 1\nmodule = \"github.com/acme/app\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        );
        ephemeral.add_file(
            "workspace/scratch/vo.mod",
            "format = 1\nmodule = \"local/scratch\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
        );
        let (_, members) = load_workspace_members_in_with_provenance(
            &ephemeral,
            Path::new("workspace/app"),
            &WorkspaceDiscovery::Auto,
        )
        .unwrap();
        assert_eq!(members.len(), 2);
        assert_eq!(members[1].module.as_str(), "local/scratch");
    }
}
