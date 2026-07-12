//! Extension loader for dynamic loading of native extensions.
//!
//! Extensions are discovered from `vo.mod` extension metadata and loaded
//! at runtime via dlopen. Uses `ExtensionTable` at the dylib boundary.

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use libloading::{Library, Symbol};

use crate::bytecode::ExternEffects;
use crate::ffi::{
    ExtensionTable, ExternEntry, ExternFnPtr, EXTENSION_ABI_FINGERPRINT, EXTENSION_ABI_VERSION,
};

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
pub const ABI_FINGERPRINT: u64 = EXTENSION_ABI_FINGERPRINT;

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
    /// Missing ABI fingerprint entry point.
    MissingAbiFingerprint,
    /// ABI fingerprint mismatch.
    FingerprintMismatch { expected: u64, found: u64 },
    /// Invalid extension entry table.
    InvalidEntryTable { extension: String, message: String },
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
            ExtError::MissingAbiFingerprint => {
                write!(f, "extension missing vo_ext_get_abi_fingerprint")
            }
            ExtError::FingerprintMismatch { expected, found } => {
                write!(
                    f,
                    "ABI fingerprint mismatch: expected {:#x}, found {:#x}",
                    expected, found
                )
            }
            ExtError::InvalidEntryTable { extension, message } => {
                write!(
                    f,
                    "invalid entry table for extension '{}': {}",
                    extension, message
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

fn invalid_entry_table(extension: &str, message: impl Into<String>) -> ExtError {
    ExtError::InvalidEntryTable {
        extension: extension.to_string(),
        message: message.into(),
    }
}

fn validate_extension_table_pointer(
    extension: &str,
    table: &ExtensionTable,
) -> Result<(), ExtError> {
    if table.entry_count > 0 && table.entries.is_null() {
        return Err(invalid_entry_table(
            extension,
            "non-empty extension table has a null entries pointer",
        ));
    }
    Ok(())
}

fn validate_extension_abi_version(found: u32) -> Result<(), ExtError> {
    if found != ABI_VERSION {
        return Err(ExtError::VersionMismatch {
            expected: ABI_VERSION,
            found,
        });
    }
    Ok(())
}

fn validate_extension_abi_fingerprint(found: u64) -> Result<(), ExtError> {
    if found != ABI_FINGERPRINT {
        return Err(ExtError::FingerprintMismatch {
            expected: ABI_FINGERPRINT,
            found,
        });
    }
    Ok(())
}

fn extern_entry_name<'a>(
    extension: &str,
    index: usize,
    entry: &'a ExternEntry,
) -> Result<&'a str, ExtError> {
    if entry.name_len == 0 {
        return Err(invalid_entry_table(
            extension,
            format!("entry {} has an empty extern name", index),
        ));
    }
    if entry.name_ptr.is_null() {
        return Err(invalid_entry_table(
            extension,
            format!("entry {} has a null extern name pointer", index),
        ));
    }
    let bytes = unsafe { std::slice::from_raw_parts(entry.name_ptr, entry.name_len as usize) };
    std::str::from_utf8(bytes).map_err(|err| {
        invalid_entry_table(
            extension,
            format!("entry {} extern name is not UTF-8: {}", index, err),
        )
    })
}

fn validate_extension_entries<'a>(
    extension: &str,
    entries: impl IntoIterator<Item = (usize, &'a ExternEntry)>,
    mut is_already_registered: impl FnMut(&str) -> bool,
) -> Result<(), ExtError> {
    let mut seen = HashSet::new();
    for (index, entry) in entries {
        let extern_name = extern_entry_name(extension, index, entry)?;
        if !seen.insert(extern_name) {
            return Err(invalid_entry_table(
                extension,
                format!("duplicate extern name '{}'", extern_name),
            ));
        }
        if is_already_registered(extern_name) {
            return Err(invalid_entry_table(
                extension,
                format!("extern name '{}' is already registered", extern_name),
            ));
        }
        if ExternEffects::from_bits(entry.effects_bits).is_none() {
            return Err(invalid_entry_table(
                extension,
                format!(
                    "entry {} extern '{}' has invalid effects bits 0x{:x}",
                    index, extern_name, entry.effects_bits
                ),
            ));
        }
    }
    Ok(())
}

fn extension_extern_prefix(extension: &str) -> String {
    let mut prefix = String::with_capacity(extension.len() + 1);
    for ch in extension.chars() {
        if ch.is_ascii_alphanumeric() {
            prefix.push(ch.to_ascii_lowercase());
        } else {
            prefix.push('_');
        }
    }
    prefix.push('_');
    prefix
}

fn extension_exports_entry(extension: &str, extern_name: &str) -> bool {
    extern_name.starts_with(&extension_extern_prefix(extension))
}

/// A loaded extension entry.
#[derive(Clone, Copy)]
pub struct LoadedEntry {
    pub func: ExternFnPtr,
    pub effects: ExternEffects,
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
        if self.has_loaded_spec(name, &canonical_path) {
            return Ok(());
        }

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
        validate_extension_abi_version(table.version)?;

        let get_abi_fingerprint: Symbol<extern "C" fn() -> u64> = unsafe {
            lib.get(b"vo_ext_get_abi_fingerprint")
                .map_err(|_| ExtError::MissingAbiFingerprint)?
        };
        let found_fingerprint = get_abi_fingerprint();
        validate_extension_abi_fingerprint(found_fingerprint)?;

        validate_extension_table_pointer(name, &table)?;
        let c_entries: &[ExternEntry] = if table.entry_count == 0 {
            &[]
        } else {
            unsafe { std::slice::from_raw_parts(table.entries, table.entry_count as usize) }
        };
        let mut owned_entries = Vec::new();
        for (index, entry) in c_entries.iter().enumerate() {
            let extern_name = extern_entry_name(name, index, entry)?;
            if extension_exports_entry(name, extern_name) {
                owned_entries.push((index, entry));
            }
        }
        validate_extension_entries(name, owned_entries.iter().copied(), |extern_name| {
            self.cache.contains_key(extern_name)
        })?;

        let ext_idx = self.loaded.len();
        let mut entries = Vec::with_capacity(owned_entries.len());

        for (entry_idx, (table_idx, c_entry)) in owned_entries.iter().enumerate() {
            let extern_name = extern_entry_name(name, *table_idx, c_entry)?;
            self.cache
                .insert(extern_name.to_string(), (ext_idx, entry_idx));
            entries.push(LoadedEntry {
                func: c_entry.func,
                effects: c_entry.effects().unwrap_or(ExternEffects::UNKNOWN_CONTROL),
            });
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
    pub fn lookup(&self, name: &str) -> Option<LoadedEntry> {
        let (ext_idx, entry_idx) = self.cache.get(name)?;
        Some(self.loaded[*ext_idx].entries[*entry_idx])
    }

    /// Get list of loaded extension names.
    pub fn loaded_extensions(&self) -> Vec<&str> {
        self.loaded.iter().map(|e| e.name.as_str()).collect()
    }

    pub fn specs(&self) -> &[NativeExtensionSpec] {
        &self.specs
    }

    fn has_loaded_spec(&self, name: &str, native_path: &Path) -> bool {
        self.specs
            .iter()
            .any(|loaded| loaded.name == name && loaded.native_path == native_path)
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

#[cfg(test)]
mod tests {
    use super::*;

    extern "C" fn dummy_ext(_ctx: *mut crate::ffi::ExternCallContext<'_>) -> u32 {
        crate::ffi::ext_abi::RESULT_OK
    }

    fn entry(name: &'static [u8]) -> ExternEntry {
        ExternEntry {
            name_ptr: name.as_ptr(),
            name_len: name.len() as u32,
            func: dummy_ext,
            effects_bits: ExternEffects::NONE.bits(),
        }
    }

    #[test]
    fn extension_table_allows_empty_table_with_null_entries() {
        let table = ExtensionTable {
            version: ABI_VERSION,
            entry_count: 0,
            entries: std::ptr::null(),
        };

        validate_extension_table_pointer("empty", &table).expect("empty table is valid");
    }

    #[test]
    fn extension_table_rejects_non_empty_null_entries() {
        let table = ExtensionTable {
            version: ABI_VERSION,
            entry_count: 1,
            entries: std::ptr::null(),
        };

        let err = validate_extension_table_pointer("bad", &table).expect_err("null entries");
        assert!(err.to_string().contains("null entries pointer"));
    }

    #[test]
    fn extension_table_rejects_invalid_effect_bits() {
        let mut entries = [entry(b"pkg_bad_effects")];
        entries[0].effects_bits = ExternEffects::ALLOWED_BITS << 1;
        let err = validate_extension_entries("bad", entries.iter().enumerate(), |_| false)
            .expect_err("invalid effects");
        assert!(err.to_string().contains("invalid effects bits"));
    }

    #[test]
    fn extension_abi_contract_rejects_version_mismatch() {
        let found = ABI_VERSION.wrapping_add(1);

        let err = validate_extension_abi_version(found).expect_err("version mismatch");
        assert!(matches!(
            err,
            ExtError::VersionMismatch {
                expected: ABI_VERSION,
                found: actual
            } if actual == found
        ));

        validate_extension_abi_version(ABI_VERSION).expect("matching ABI version");
    }

    #[test]
    fn extension_abi_contract_rejects_fingerprint_mismatch() {
        let found = ABI_FINGERPRINT ^ 0x1;

        let err = validate_extension_abi_fingerprint(found).expect_err("fingerprint mismatch");
        assert!(matches!(
            err,
            ExtError::FingerprintMismatch {
                expected: ABI_FINGERPRINT,
                found: actual
            } if actual == found
        ));

        validate_extension_abi_fingerprint(ABI_FINGERPRINT).expect("matching ABI fingerprint");
    }

    #[test]
    fn extension_entries_reject_duplicate_extern_names() {
        let entries = [entry(b"dup"), entry(b"dup")];

        let err = validate_extension_entries("dups", entries.iter().enumerate(), |_| false)
            .expect_err("duplicate name");
        assert!(err.to_string().contains("duplicate extern name 'dup'"));
    }

    #[test]
    fn extension_entries_reject_already_registered_extern_names() {
        let entries = [entry(b"existing")];

        let err = validate_extension_entries("dups", entries.iter().enumerate(), |name| {
            name == "existing"
        })
        .expect_err("already registered name");
        assert!(err.to_string().contains("already registered"));
    }

    #[test]
    fn extension_entries_accept_unique_extern_names() {
        let entries = [entry(b"one"), entry(b"two")];

        validate_extension_entries("ok", entries.iter().enumerate(), |_| false)
            .expect("unique names");
    }

    #[test]
    fn extension_entry_ownership_uses_manifest_prefix() {
        assert!(extension_exports_entry("voplay", "voplay_renderInit"));
        assert!(extension_exports_entry("vo-play", "vo_play_renderInit"));
        assert!(extension_exports_entry("vogui", "vogui_navigate"));
        assert!(!extension_exports_entry("voplay", "vogui_navigate"));
    }

    #[test]
    fn load_impl_skips_exact_duplicate_specs() {
        let mut loader = ExtensionLoader::new();
        let native_path = PathBuf::from("/tmp/libsame.so");
        loader.specs.push(NativeExtensionSpec::new(
            "same",
            native_path.clone(),
            PathBuf::from("/tmp/same/vo.mod"),
        ));

        assert!(loader.has_loaded_spec("same", &native_path));
        assert!(!loader.has_loaded_spec("same", Path::new("/tmp/other.so")));
        assert!(!loader.has_loaded_spec("other", &native_path));
    }
}
