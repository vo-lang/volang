//! Embedded standard library.

use rust_embed::RustEmbed;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use vo_common::vfs::{FileSystem, RealFs};
use vo_module::{PackageResolverMixed, StdSource, LocalSource, ModSource};
use std::io;

#[derive(RustEmbed)]
#[folder = "../../stdlib/"]
#[prefix = ""]
struct StdlibAssets;

#[derive(Debug, Clone)]
pub struct EmbeddedStdlib {
    files: HashMap<PathBuf, String>,
}

impl Default for EmbeddedStdlib {
    fn default() -> Self {
        Self::new()
    }
}

impl EmbeddedStdlib {
    pub fn new() -> Self {
        let mut files = HashMap::new();
        for file_path in StdlibAssets::iter() {
            if let Some(content) = StdlibAssets::get(&file_path) {
                if let Ok(s) = std::str::from_utf8(content.data.as_ref()) {
                    files.insert(PathBuf::from(file_path.to_string()), s.to_string());
                }
            }
        }
        Self { files }
    }
}

impl FileSystem for EmbeddedStdlib {
    fn read_file(&self, path: &Path) -> io::Result<String> {
        self.files.get(path)
            .cloned()
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, format!("file not found: {:?}", path)))
    }
    
    fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
        let mut entries = Vec::new();
        let is_root = path == Path::new(".") || path == Path::new("") || path.as_os_str().is_empty();
        
        for file_path in self.files.keys() {
            if is_root {
                if let Some(first_component) = file_path.components().next() {
                    let entry = PathBuf::from(first_component.as_os_str());
                    if !entries.contains(&entry) {
                        entries.push(entry);
                    }
                }
            } else {
                let path_str = format!("{}/", path.to_string_lossy());
                let file_str = file_path.to_string_lossy();
                if file_str.starts_with(&path_str) {
                    let rest = &file_str[path_str.len()..];
                    if !rest.is_empty() {
                        let entry = if let Some(idx) = rest.find('/') {
                            path.join(&rest[..idx])
                        } else {
                            path.join(rest)
                        };
                        if !entries.contains(&entry) {
                            entries.push(entry);
                        }
                    }
                }
            }
        }
        Ok(entries)
    }
    
    fn exists(&self, path: &Path) -> bool {
        if self.files.contains_key(path) {
            return true;
        }
        if path == Path::new(".") || path == Path::new("") {
            return !self.files.is_empty();
        }
        let path_str = path.to_string_lossy();
        self.files.keys().any(|p| {
            let p_str = p.to_string_lossy();
            p_str.starts_with(&*path_str) && 
            p_str.len() > path_str.len() &&
            p_str.chars().nth(path_str.len()) == Some('/')
        })
    }
    
    fn is_dir(&self, path: &Path) -> bool {
        !self.files.contains_key(path) && self.exists(path)
    }
}

impl vo_common_core::SourceProvider for EmbeddedStdlib {
    fn read_source(&self, path: &str) -> Option<String> {
        self.read_file(Path::new(path)).ok()
    }
}

pub fn create_resolver(local_root: &Path) -> PackageResolverMixed<EmbeddedStdlib, RealFs, RealFs> {
    let mod_root = dirs::home_dir()
        .map(|h| h.join(".vo/mod"))
        .unwrap_or_else(|| local_root.join(".vo/mod"));
    
    PackageResolverMixed {
        std: StdSource::with_fs(EmbeddedStdlib::new()),
        local: LocalSource::with_fs(RealFs::new(local_root)),
        r#mod: ModSource::with_fs(RealFs::new(mod_root)),
    }
}
