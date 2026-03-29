//! WasmVfs — FileSystem implementation backed by the JS VirtualFS.
//!
//! This allows the Vo compiler's `ModSource` (and other resolvers) to read
//! third-party modules directly from the browser's VFS, exactly like native
//! code reads from `~/.vo/mod/` via `RealFs`.

use std::io;
use std::path::{Path, PathBuf};

use vo_common::vfs::FileSystem;

/// A `FileSystem` that delegates to the JS VirtualFS via `vo_web_runtime_wasm::vfs`.
///
/// All paths are resolved under an optional `root` prefix inside the VFS.
/// For example, with `root = "/.vo/mod"`, reading `"github.com/vo-lang/vox/vox.vo"`
/// maps to the VFS path `"/.vo/mod/github.com/vo-lang/vox/vox.vo"`.
#[derive(Clone)]
pub struct WasmVfs {
    root: PathBuf,
}

impl WasmVfs {
    /// Create a WasmVfs rooted at the given VFS directory.
    pub fn new(root: impl Into<PathBuf>) -> Self {
        Self { root: root.into() }
    }

    fn full_path(&self, path: &Path) -> String {
        let joined = self.root.join(path);
        format!("/{}", joined.display())
    }
}

impl FileSystem for WasmVfs {
    fn read_file(&self, path: &Path) -> io::Result<String> {
        let full = self.full_path(path);
        let (data, err) = vo_web_runtime_wasm::vfs::read_file(&full);
        if let Some(e) = err {
            return Err(io::Error::new(io::ErrorKind::NotFound, e));
        }
        String::from_utf8(data).map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))
    }

    fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
        let full = self.full_path(path);
        let (entries, err) = vo_web_runtime_wasm::vfs::read_dir(&full);
        if let Some(e) = err {
            return Err(io::Error::new(io::ErrorKind::NotFound, e));
        }
        Ok(entries
            .into_iter()
            .map(|(name, _is_dir, _mode)| path.join(name))
            .collect())
    }

    fn exists(&self, path: &Path) -> bool {
        let full = self.full_path(path);
        let (_name, _size, _mode, _mtime, _is_dir, err) = vo_web_runtime_wasm::vfs::stat(&full);
        err.is_none()
    }

    fn is_dir(&self, path: &Path) -> bool {
        let full = self.full_path(path);
        let (_name, _size, _mode, _mtime, is_dir, err) = vo_web_runtime_wasm::vfs::stat(&full);
        err.is_none() && is_dir
    }
}
