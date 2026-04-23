//! Extension loader for dynamic loading of native extensions.
//!
//! Extensions are discovered from `vo.mod` extension metadata and loaded
//! at runtime via dlopen. Uses `ExtensionTable` at the dylib boundary.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use libloading::{Library, Symbol};

use crate::ffi::{ExtensionTable, ExternEntry, ExternFnPtr, EXTENSION_ABI_VERSION};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NativeExtensionSpec {
    pub name: String,
    pub native_path: PathBuf,
    pub manifest_path: PathBuf,
}

impl NativeExtensionSpec {
    pub fn new(name: impl Into<String>, native_path: PathBuf, manifest_path: PathBuf) -> Self {
        Self {
            name: name.into(),
            native_path,
            manifest_path,
        }
    }
}

/// ABI version — must match vo-ext's ABI_VERSION.
pub const ABI_VERSION: u32 = EXTENSION_ABI_VERSION;

/// Error type for extension loading.
#[derive(Debug)]
pub enum ExtError {
    /// Failed to load library.
    LoadFailed(String),
    /// Missing entry point function.
    MissingEntryPoint,
    /// Failed to lookup an exported symbol from a loaded extension library.
    SymbolLookupFailed {
        extension: String,
        symbol: String,
        message: String,
    },
    /// ABI version mismatch.
    VersionMismatch { expected: u32, found: u32 },
    /// IO error.
    Io(std::io::Error),
}

impl std::fmt::Display for ExtError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExtError::LoadFailed(msg) => write!(f, "failed to load extension: {}", msg),
            ExtError::MissingEntryPoint => write!(f, "extension missing vo_ext_get_entries"),
            ExtError::SymbolLookupFailed {
                extension,
                symbol,
                message,
            } => {
                write!(
                    f,
                    "failed to lookup symbol '{}' from extension '{}': {}",
                    symbol, extension, message
                )
            }
            ExtError::VersionMismatch { expected, found } => {
                write!(
                    f,
                    "ABI version mismatch: expected {}, found {}",
                    expected, found
                )
            }
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
    specs: Vec<NativeExtensionSpec>,
}

impl ExtensionLoader {
    /// Create a new extension loader.
    pub fn new() -> Self {
        Self {
            loaded: Vec::new(),
            cache: HashMap::new(),
            specs: Vec::new(),
        }
    }

    pub fn from_specs(specs: &[NativeExtensionSpec]) -> Result<Self, ExtError> {
        let mut loader = Self::new();
        for spec in specs {
            loader.load_spec(spec)?;
        }
        Ok(loader)
    }

    fn load_spec(&mut self, spec: &NativeExtensionSpec) -> Result<(), ExtError> {
        self.load_impl(&spec.native_path, &spec.name, spec.manifest_path.clone())
    }

    /// Load an extension from a dynamic library path.
    pub fn load(&mut self, path: &Path, name: &str) -> Result<(), ExtError> {
        self.load_impl(path, name, path.to_path_buf())
    }

    fn load_impl(
        &mut self,
        path: &Path,
        name: &str,
        manifest_path: PathBuf,
    ) -> Result<(), ExtError> {
        // Canonicalize path to resolve .. and symlinks (needed for QEMU compatibility)
        let canonical_path = path
            .canonicalize()
            .map_err(|e| ExtError::LoadFailed(format!("{}: {}", path.display(), e)))?;

        // Use RTLD_LOCAL to keep dylib symbols private and avoid linkme EXTERN_TABLE conflicts
        // with the host binary. Extensions are standalone and do not share symbols.
        #[cfg(unix)]
        let lib = unsafe {
            let flags = libloading::os::unix::RTLD_NOW | libloading::os::unix::RTLD_LOCAL;
            libloading::os::unix::Library::open(Some(&canonical_path), flags)
                .map(Library::from)
                .map_err(|e| ExtError::LoadFailed(e.to_string()))?
        };
        #[cfg(not(unix))]
        let lib = unsafe { Library::new(path).map_err(|e| ExtError::LoadFailed(e.to_string()))? };

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

        let c_entries: &[ExternEntry] =
            unsafe { std::slice::from_raw_parts(table.entries, table.entry_count as usize) };

        let ext_idx = self.loaded.len();
        let mut entries = Vec::with_capacity(c_entries.len());

        for (i, c_entry) in c_entries.iter().enumerate() {
            self.cache.insert(c_entry.name().to_string(), (ext_idx, i));
            entries.push(LoadedEntry { func: c_entry.func });
        }

        self.loaded.push(LoadedExtension {
            _lib: lib,
            name: name.to_string(),
            entries,
        });
        self.specs.push(NativeExtensionSpec::new(
            name,
            canonical_path,
            manifest_path,
        ));

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

    pub fn specs(&self) -> &[NativeExtensionSpec] {
        &self.specs
    }

    /// Get a loaded extension library by manifest name.
    pub fn library(&self, name: &str) -> Option<&Library> {
        self.loaded
            .iter()
            .find(|ext| ext.name == name)
            .map(|ext| &ext._lib)
    }

    /// Lookup an arbitrary exported symbol from a loaded extension library.
    ///
    /// # Safety
    ///
    /// The caller must ensure `T` matches the actual symbol type exported by the library.
    pub unsafe fn symbol<T>(
        &self,
        extension: &str,
        symbol: &[u8],
    ) -> Result<Option<Symbol<'_, T>>, ExtError> {
        let Some(lib) = self.library(extension) else {
            return Ok(None);
        };
        match lib.get::<T>(symbol) {
            Ok(sym) => Ok(Some(sym)),
            Err(err) => Err(ExtError::SymbolLookupFailed {
                extension: extension.to_string(),
                symbol: String::from_utf8_lossy(symbol)
                    .trim_end_matches('\0')
                    .to_string(),
                message: err.to_string(),
            }),
        }
    }

    /// Inject a host bridge pointer into all loaded extension dylibs.
    ///
    /// Each dylib that exports `vo_ext_set_host_bridge` will receive the
    /// raw `*const HostBridge` pointer.  Dylibs without the symbol are
    /// silently skipped (they don't need a host bridge).
    ///
    /// # Safety
    ///
    /// `ptr` must come from `vo_ext::host::encode_bridge_ptr` and the
    /// bridge must remain alive until [`clear_bridge_all`] is called.
    pub unsafe fn broadcast_bridge(&self, ptr: usize) {
        type SetBridgeFn = unsafe extern "C" fn(usize);
        for ext in &self.loaded {
            if let Ok(sym) = ext._lib.get::<SetBridgeFn>(b"vo_ext_set_host_bridge") {
                sym(ptr);
            }
        }
    }

    /// Clear the host bridge reference from all loaded extension dylibs.
    ///
    /// Must be called before dropping the bridge to avoid dangling pointers.
    pub fn clear_bridge_all(&self) {
        type ClearBridgeFn = extern "C" fn();
        for ext in &self.loaded {
            if let Ok(sym) = unsafe { ext._lib.get::<ClearBridgeFn>(b"vo_ext_clear_host_bridge") } {
                sym();
            }
        }
    }
}

impl Default for ExtensionLoader {
    fn default() -> Self {
        Self::new()
    }
}
