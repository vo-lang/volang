//! Browser VFS adapter for compiler and module-system filesystem contracts.

use std::io;
use std::path::{Component, Path, PathBuf};

use vo_common::vfs::{
    sort_fs_paths, FileSystem, FileSystemEntryKind, MAX_DIRECTORY_ENTRIES, MAX_TEXT_FILE_BYTES,
};
use vo_module::async_install::InstallSurface;

const MAX_VFS_PATH_DEPTH: usize = 256;

/// A `FileSystem` rooted at a lexically confined directory in the browser VFS.
///
/// Relative paths may descend below `root` and may use `..` only while they
/// remain below that scope. Absolute paths are accepted solely by the global
/// (`root = ""`) host surface used by Studio project loading and installation.
/// Guest OS calls use the separately confined resolver in `runtime-wasm` and
/// never receive this host capability.
#[derive(Clone)]
pub struct WasmVfs {
    root_parts: Option<Vec<String>>,
}

impl WasmVfs {
    pub fn new(root: impl Into<PathBuf>) -> Self {
        let requested = root.into();
        let root_parts = scoped_root_parts(&requested);
        Self { root_parts }
    }

    fn full_path(&self, path: &Path) -> io::Result<String> {
        let root_parts = self.root_parts.as_ref().ok_or_else(|| {
            io::Error::new(io::ErrorKind::InvalidInput, "invalid browser VFS root")
        })?;
        let absolute = path.is_absolute();
        if absolute && !root_parts.is_empty() {
            return Err(io::Error::new(
                io::ErrorKind::PermissionDenied,
                "absolute path escapes the browser VFS root",
            ));
        }

        let mut parts = if absolute {
            Vec::new()
        } else {
            root_parts.clone()
        };
        let floor = parts.len();
        for component in path.components() {
            match component {
                Component::CurDir | Component::RootDir => {}
                Component::ParentDir => {
                    if parts.len() == floor {
                        return Err(io::Error::new(
                            io::ErrorKind::PermissionDenied,
                            "path escapes the browser VFS root",
                        ));
                    }
                    parts.pop();
                }
                Component::Normal(part) => {
                    let part = part.to_str().ok_or_else(|| {
                        io::Error::new(
                            io::ErrorKind::InvalidData,
                            "browser VFS paths must be valid UTF-8",
                        )
                    })?;
                    if part.is_empty() || part.contains('\0') {
                        return Err(io::Error::new(
                            io::ErrorKind::InvalidInput,
                            "browser VFS path contains invalid data",
                        ));
                    }
                    parts.push(part.to_string());
                    if parts.len() > MAX_VFS_PATH_DEPTH {
                        return Err(io::Error::new(
                            io::ErrorKind::InvalidInput,
                            "browser VFS path is too deep",
                        ));
                    }
                }
                Component::Prefix(_) => {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidInput,
                        "browser VFS does not accept platform path prefixes",
                    ));
                }
            }
        }

        if parts.is_empty() {
            Ok("/".to_string())
        } else {
            Ok(format!("/{}", parts.join("/")))
        }
    }

    fn full_install_path(&self, path: &Path) -> io::Result<String> {
        vo_module::schema::portable_relative_path_from_path(path).map_err(|error| {
            io::Error::new(
                io::ErrorKind::InvalidInput,
                format!(
                    "invalid browser VFS install path {}: {error}",
                    path.display()
                ),
            )
        })?;
        self.full_path(path)
    }
}

fn scoped_root_parts(root: &Path) -> Option<Vec<String>> {
    let mut parts = Vec::new();
    for component in root.components() {
        match component {
            Component::CurDir | Component::RootDir => {}
            Component::ParentDir => {
                parts.pop()?;
            }
            Component::Normal(part) => {
                let part = part.to_str()?;
                if part.is_empty() || part.contains('\0') {
                    return None;
                }
                parts.push(part.to_string());
                if parts.len() > MAX_VFS_PATH_DEPTH {
                    return None;
                }
            }
            Component::Prefix(_) => return None,
        }
    }
    Some(parts)
}

fn vfs_io_error(message: String) -> io::Error {
    let kind = match message.as_str() {
        "file does not exist" => io::ErrorKind::NotFound,
        "file already exists" => io::ErrorKind::AlreadyExists,
        "permission denied" => io::ErrorKind::PermissionDenied,
        "invalid argument" => io::ErrorKind::InvalidInput,
        "operation timed out" | "i/o timeout" => io::ErrorKind::TimedOut,
        "not a directory" => io::ErrorKind::NotADirectory,
        "is a directory" => io::ErrorKind::IsADirectory,
        "file too large"
        | "directory contains too many entries"
        | "invalid browser VFS host response" => io::ErrorKind::InvalidData,
        _ if message.starts_with("invalid argument:") => io::ErrorKind::InvalidInput,
        _ => io::ErrorKind::Other,
    };
    io::Error::new(kind, message)
}

fn checked_vfs_read_size(size: i64, is_dir: bool, max_bytes: usize) -> io::Result<usize> {
    if is_dir {
        return Err(io::Error::new(
            io::ErrorKind::IsADirectory,
            "browser VFS path is a directory",
        ));
    }
    let size = usize::try_from(size).map_err(|_| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            "browser VFS reported an invalid file size",
        )
    })?;
    if size > max_bytes {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!("browser VFS file size {size} exceeds the {max_bytes}-byte limit"),
        ));
    }
    Ok(size)
}

impl FileSystem for WasmVfs {
    fn read_file(&self, path: &Path) -> io::Result<String> {
        let data = self.read_bytes_limited(path, MAX_TEXT_FILE_BYTES)?;
        String::from_utf8(data)
            .map_err(|error| io::Error::new(io::ErrorKind::InvalidData, error.utf8_error()))
    }

    fn read_bytes(&self, path: &Path) -> io::Result<Vec<u8>> {
        self.read_bytes_limited(path, vo_web_runtime_wasm::vfs::MAX_VFS_FILE_BYTES)
    }

    fn read_bytes_limited(&self, path: &Path, max_bytes: usize) -> io::Result<Vec<u8>> {
        let full = self.full_path(path)?;
        let (_name, size, _mode, _mtime, is_dir, stat_error) =
            vo_web_runtime_wasm::vfs::stat(&full);
        if let Some(error) = stat_error {
            return Err(vfs_io_error(error));
        }
        checked_vfs_read_size(size, is_dir, max_bytes)?;
        let (data, error) = vo_web_runtime_wasm::vfs::read_file_limited(&full, max_bytes);
        if let Some(error) = error {
            return Err(vfs_io_error(error));
        }
        if data.len() > max_bytes {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "browser VFS file size {} exceeds the {max_bytes}-byte limit",
                    data.len()
                ),
            ));
        }
        Ok(data)
    }

    fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
        let full = self.full_path(path)?;
        let (entries, error) = vo_web_runtime_wasm::vfs::read_dir(&full);
        if let Some(error) = error {
            return Err(vfs_io_error(error));
        }
        if entries.len() > MAX_DIRECTORY_ENTRIES {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("directory contains more than {MAX_DIRECTORY_ENTRIES} entries"),
            ));
        }
        let mut entries: Vec<_> = entries
            .into_iter()
            .map(|(name, _is_dir, _mode)| path.join(name))
            .collect();
        sort_fs_paths(&mut entries);
        Ok(entries)
    }

    fn exists(&self, path: &Path) -> bool {
        let Ok(full) = self.full_path(path) else {
            return false;
        };
        vo_web_runtime_wasm::vfs::exists(&full).unwrap_or(false)
    }

    fn is_dir(&self, path: &Path) -> bool {
        let Ok(full) = self.full_path(path) else {
            return false;
        };
        let (_name, _size, _mode, _mtime, is_dir, error) = vo_web_runtime_wasm::vfs::stat(&full);
        error.is_none() && is_dir
    }

    fn entry_kind(&self, path: &Path) -> io::Result<FileSystemEntryKind> {
        let full = self.full_path(path)?;
        let (_name, _size, _mode, _mtime, is_dir, error) = vo_web_runtime_wasm::vfs::stat(&full);
        match error {
            None if is_dir => Ok(FileSystemEntryKind::Directory),
            None => Ok(FileSystemEntryKind::RegularFile),
            Some(error) => {
                let error = vfs_io_error(error);
                if error.kind() == io::ErrorKind::NotFound {
                    Ok(FileSystemEntryKind::Missing)
                } else {
                    Err(error)
                }
            }
        }
    }
}

impl InstallSurface for WasmVfs {
    fn remove_tree(&self, path: &Path) -> vo_module::Result<()> {
        let full = self.full_install_path(path).map_err(vo_module::Error::Io)?;
        if let Some(error) = vo_web_runtime_wasm::vfs::remove_all(&full) {
            let error = vfs_io_error(error);
            if error.kind() != io::ErrorKind::NotFound {
                return Err(vo_module::Error::Io(error));
            }
        }
        Ok(())
    }

    fn mkdir_all(&self, path: &Path) -> vo_module::Result<()> {
        let full = self.full_install_path(path).map_err(vo_module::Error::Io)?;
        if let Some(error) = vo_web_runtime_wasm::vfs::mkdir_all(&full, 0o755) {
            return Err(vo_module::Error::Io(vfs_io_error(error)));
        }
        Ok(())
    }

    fn create_dir(&self, path: &Path) -> vo_module::Result<()> {
        let full = self.full_install_path(path).map_err(vo_module::Error::Io)?;
        if let Some(error) = vo_web_runtime_wasm::vfs::mkdir(&full, 0o755) {
            return Err(vo_module::Error::Io(vfs_io_error(error)));
        }
        Ok(())
    }

    fn write_bytes(&self, path: &Path, bytes: &[u8]) -> vo_module::Result<()> {
        let full = self.full_install_path(path).map_err(vo_module::Error::Io)?;
        if let Some(error) = vo_web_runtime_wasm::vfs::write_file(&full, bytes, 0o644) {
            return Err(vo_module::Error::Io(vfs_io_error(error)));
        }
        Ok(())
    }

    fn publish_noreplace(&self, from: &Path, to: &Path) -> vo_module::Result<()> {
        let from = self.full_install_path(from).map_err(vo_module::Error::Io)?;
        let to = self.full_install_path(to).map_err(vo_module::Error::Io)?;
        if let Some(error) = vo_web_runtime_wasm::vfs::rename_noreplace(&from, &to) {
            return Err(vo_module::Error::Io(vfs_io_error(error)));
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::io::ErrorKind;
    use std::path::Path;

    use super::{checked_vfs_read_size, WasmVfs};

    #[test]
    fn scoped_paths_cannot_replace_or_escape_the_configured_root() {
        let vfs = WasmVfs::new("/.vo/mod");
        assert_eq!(
            vfs.full_path(Path::new("github.com/vo-lang/pkg/pkg.vo"))
                .unwrap(),
            "/.vo/mod/github.com/vo-lang/pkg/pkg.vo"
        );
        assert_eq!(
            vfs.full_path(Path::new("../escape")).unwrap_err().kind(),
            ErrorKind::PermissionDenied
        );
        assert_eq!(
            vfs.full_path(Path::new("/absolute")).unwrap_err().kind(),
            ErrorKind::PermissionDenied
        );
    }

    #[test]
    fn privileged_host_surface_accepts_absolute_paths_but_rejects_parent_escape() {
        let vfs = WasmVfs::new("");
        assert_eq!(
            vfs.full_path(Path::new("/tmp/cache/file")).unwrap(),
            "/tmp/cache/file"
        );
        assert_eq!(
            vfs.full_path(Path::new("../escape")).unwrap_err().kind(),
            ErrorKind::PermissionDenied
        );
        assert!(WasmVfs::new("../invalid")
            .full_path(Path::new("file"))
            .is_err());
    }

    #[test]
    fn install_mutations_require_portable_relative_paths() {
        let vfs = WasmVfs::new("/.vo/mod");
        assert_eq!(
            vfs.full_install_path(Path::new("github.com@acme@pkg/0.1.0"))
                .unwrap(),
            "/.vo/mod/github.com@acme@pkg/0.1.0"
        );
        assert!(vfs.full_install_path(Path::new("")).is_err());
        assert!(vfs.full_install_path(Path::new("../escape")).is_err());
        assert!(vfs.full_install_path(Path::new("/absolute")).is_err());
        assert!(vfs.full_install_path(Path::new("source\\main.vo")).is_err());
    }

    #[test]
    fn limited_read_preflight_rejects_type_size_and_sign_errors() {
        assert_eq!(checked_vfs_read_size(4, false, 4).unwrap(), 4);
        assert_eq!(
            checked_vfs_read_size(0, true, 4).unwrap_err().kind(),
            ErrorKind::IsADirectory
        );
        assert_eq!(
            checked_vfs_read_size(-1, false, 4).unwrap_err().kind(),
            ErrorKind::InvalidData
        );
        assert_eq!(
            checked_vfs_read_size(5, false, 4).unwrap_err().kind(),
            ErrorKind::InvalidData
        );
    }
}
