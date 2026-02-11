//! Extension loader for dynamic loading of native extensions.
//!
//! Extensions are discovered from `vo.ext.toml` manifests and loaded
//! at runtime via dlopen. Uses `ExtensionTable` at the dylib
//! boundary for stable cross-compilation compatibility.

use std::collections::HashMap;
use std::path::Path;

use libloading::{Library, Symbol};

use crate::ffi::{ExternEntry, ExternFnPtr, ExtensionTable};

// Re-export from vo-module
pub use vo_module::{ExtensionManifest, discover_extensions};

/// ABI version — must match vo-ext's ABI_VERSION.
pub const ABI_VERSION: u32 = 2;

/// Error type for extension loading.
#[derive(Debug)]
pub enum ExtError {
    /// Failed to load library.
    LoadFailed(String),
    /// Missing entry point function.
    MissingEntryPoint,
    /// ABI version mismatch.
    VersionMismatch { expected: u32, found: u32 },
    /// Manifest parse error.
    ManifestError(String),
    /// IO error.
    Io(std::io::Error),
}

impl std::fmt::Display for ExtError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExtError::LoadFailed(msg) => write!(f, "failed to load extension: {}", msg),
            ExtError::MissingEntryPoint => write!(f, "extension missing vo_ext_get_entries"),
            ExtError::VersionMismatch { expected, found } => {
                write!(f, "ABI version mismatch: expected {}, found {}", expected, found)
            }
            ExtError::ManifestError(msg) => write!(f, "manifest error: {}", msg),
            ExtError::Io(e) => write!(f, "IO error: {}", e),
        }
    }
}

impl std::error::Error for ExtError {}

impl From<std::io::Error> for ExtError {
    fn from(e: std::io::Error) -> Self {
        ExtError::Io(e)
    }
}

/// A loaded extension entry.
struct LoadedEntry {
    func: ExternFnPtr,
}

/// A loaded extension.
struct LoadedExtension {
    /// Keep library alive.
    _lib: Library,
    /// Name of the extension.
    name: String,
    /// All extern functions.
    entries: Vec<LoadedEntry>,
}

/// Extension loader and registry.
pub struct ExtensionLoader {
    /// Loaded extensions.
    loaded: Vec<LoadedExtension>,
    /// Cache: function name -> (ext_idx, entry_idx).
    cache: HashMap<String, (usize, usize)>,
}

impl ExtensionLoader {
    /// Create a new extension loader.
    pub fn new() -> Self {
        Self {
            loaded: Vec::new(),
            cache: HashMap::new(),
        }
    }

    /// Load an extension from a dynamic library path.
    pub fn load(&mut self, path: &Path, name: &str) -> Result<(), ExtError> {
        // Canonicalize path to resolve .. and symlinks (needed for QEMU compatibility)
        let canonical_path = path.canonicalize()
            .map_err(|e| ExtError::LoadFailed(format!("{}: {}", path.display(), e)))?;
        
        // Use RTLD_GLOBAL so symbols are visible to other extensions
        #[cfg(unix)]
        let lib = unsafe {
            let flags = libloading::os::unix::RTLD_NOW | libloading::os::unix::RTLD_GLOBAL;
            libloading::os::unix::Library::open(Some(&canonical_path), flags)
                .map(|l| Library::from(l))
                .map_err(|e| ExtError::LoadFailed(e.to_string()))?
        };
        #[cfg(not(unix))]
        let lib = unsafe {
            Library::new(path).map_err(|e| ExtError::LoadFailed(e.to_string()))?
        };

        let get_entries: Symbol<extern "C" fn() -> ExtensionTable> = unsafe {
            lib.get(b"vo_ext_get_entries")
                .map_err(|_| ExtError::MissingEntryPoint)?
        };

        let table = get_entries();

        if table.version != ABI_VERSION {
            return Err(ExtError::VersionMismatch {
                expected: ABI_VERSION,
                found: table.version,
            });
        }

        let c_entries: &[ExternEntry] = unsafe {
            std::slice::from_raw_parts(table.entries, table.entry_count as usize)
        };

        let ext_idx = self.loaded.len();
        let mut entries = Vec::with_capacity(c_entries.len());

        for (i, c_entry) in c_entries.iter().enumerate() {
            self.cache.insert(c_entry.name().to_string(), (ext_idx, i));
            entries.push(LoadedEntry {
                func: c_entry.func,
            });
        }

        self.loaded.push(LoadedExtension {
            _lib: lib,
            name: name.to_string(),
            entries,
        });

        Ok(())
    }

    /// Lookup an extension function by name.
    pub fn lookup(&self, name: &str) -> Option<ExternFnPtr> {
        let (ext_idx, entry_idx) = self.cache.get(name)?;
        Some(self.loaded[*ext_idx].entries[*entry_idx].func)
    }

    /// Get list of loaded extension names.
    pub fn loaded_extensions(&self) -> Vec<&str> {
        self.loaded.iter().map(|e| e.name.as_str()).collect()
    }
}

impl Default for ExtensionLoader {
    fn default() -> Self {
        Self::new()
    }
}
