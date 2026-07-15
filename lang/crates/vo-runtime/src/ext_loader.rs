//! Extension loader for dynamic loading of native extensions.
//!
//! Extensions are discovered from `vo.mod` extension metadata and loaded
//! at runtime via dlopen. Uses `ExtensionTable` at the dylib boundary.

use std::any::Any;
use std::borrow::Borrow;
use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use libloading::{Library, Symbol};

use crate::bytecode::ExternEffects;
use crate::ffi::{
    ExtensionTable, ExternEntry, ExternFnPtr, EXTENSION_ABI_FINGERPRINT, EXTENSION_ABI_VERSION,
};
use vo_common_core::extern_key::{
    decode_extern_name, deepest_owning_module, validate_canonical_module_owner,
    CanonicalModuleOwnerError, MAX_EXTERN_NAME_BYTES,
};

#[derive(Clone)]
pub struct NativeExtensionSpec {
    /// Human-facing provider/artifact identity from `[extension].name`.
    pub name: String,
    /// Exact canonical module path that owns every package exported by this
    /// provider. Language-level symbol lookup never derives identity from
    /// `name`.
    pub module_owner: String,
    pub native_path: PathBuf,
    pub manifest_path: PathBuf,
    /// Opaque resources whose lifetime must cover every future load of this
    /// specification, including loads performed by island VMs.
    ///
    /// This is lifecycle state rather than provider identity, so equality and
    /// persistent compile-cache serialization intentionally ignore it. A
    /// deserialized specification must attach a fresh resource before it is
    /// exposed to callers.
    lifetime_resources: Vec<Arc<dyn Any + Send + Sync>>,
}

impl std::fmt::Debug for NativeExtensionSpec {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NativeExtensionSpec")
            .field("name", &self.name)
            .field("module_owner", &self.module_owner)
            .field("native_path", &self.native_path)
            .field("manifest_path", &self.manifest_path)
            .field(
                "retained_lifetime_resources",
                &self.lifetime_resources.len(),
            )
            .finish()
    }
}

impl PartialEq for NativeExtensionSpec {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
            && self.module_owner == other.module_owner
            && self.native_path == other.native_path
            && self.manifest_path == other.manifest_path
    }
}

impl Eq for NativeExtensionSpec {}

impl NativeExtensionSpec {
    /// Construct a specification without performing I/O.
    ///
    /// [`ExtensionLoader`] validates `module_owner` before opening the native
    /// path. Call [`Self::try_new`] when eager validation is useful.
    pub fn new(
        name: impl Into<String>,
        module_owner: impl Into<String>,
        native_path: PathBuf,
        manifest_path: PathBuf,
    ) -> Self {
        Self {
            name: name.into(),
            module_owner: module_owner.into(),
            native_path,
            manifest_path,
            lifetime_resources: Vec::new(),
        }
    }

    pub fn try_new(
        name: impl Into<String>,
        module_owner: impl Into<String>,
        native_path: PathBuf,
        manifest_path: PathBuf,
    ) -> Result<Self, CanonicalModuleOwnerError> {
        let module_owner = module_owner.into();
        validate_canonical_module_owner(&module_owner)?;
        Ok(Self::new(name, module_owner, native_path, manifest_path))
    }

    /// Retain an opaque resource for as long as this specification, any clone,
    /// or any loader created from it remains alive.
    pub fn retain_lifetime_resource<T>(&mut self, resource: Arc<T>)
    where
        T: Any + Send + Sync,
    {
        self.retain_erased_lifetime_resource(resource);
    }

    fn retain_erased_lifetime_resource(&mut self, resource: Arc<dyn Any + Send + Sync>) {
        if self
            .lifetime_resources
            .iter()
            .any(|existing| Arc::ptr_eq(existing, &resource))
        {
            return;
        }
        self.lifetime_resources.push(resource);
    }
}

/// ABI version — must match vo-ext's ABI_VERSION.
pub const ABI_VERSION: u32 = EXTENSION_ABI_VERSION;
pub const ABI_FINGERPRINT: u64 = EXTENSION_ABI_FINGERPRINT;
const MAX_EXTENSION_ENTRIES: u32 = 65_536;

/// Error type for extension loading.
#[derive(Debug)]
pub enum ExtError {
    /// Failed to load library.
    LoadFailed(String),
    /// Missing entry point function.
    MissingEntryPoint,
    /// Missing ABI version entry point.
    MissingAbiVersion,
    /// Failed to lookup an exported symbol from a loaded extension library.
    SymbolLookupFailed {
        module_owner: String,
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
            ExtError::MissingAbiVersion => {
                write!(f, "extension missing vo_ext_get_abi_version")
            }
            ExtError::SymbolLookupFailed {
                module_owner,
                symbol,
                message,
            } => {
                write!(
                    f,
                    "failed to lookup symbol '{}' from module owner '{}': {}",
                    symbol, module_owner, message
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
    if table.entry_count > MAX_EXTENSION_ENTRIES {
        return Err(invalid_entry_table(
            extension,
            format!(
                "entry count {} exceeds limit {}",
                table.entry_count, MAX_EXTENSION_ENTRIES
            ),
        ));
    }
    if table.entry_count > 0 && table.entries.is_null() {
        return Err(invalid_entry_table(
            extension,
            "non-empty extension table has a null entries pointer",
        ));
    }
    if table.entry_count > 0 && !table.entries.is_aligned() {
        return Err(invalid_entry_table(
            extension,
            "extension entries pointer is misaligned",
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
    if entry.name_len as usize > MAX_EXTERN_NAME_BYTES {
        return Err(invalid_entry_table(
            extension,
            format!(
                "entry {} extern name length {} exceeds limit {}",
                index, entry.name_len, MAX_EXTERN_NAME_BYTES
            ),
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

fn extern_entry_module_owner<'a>(
    extension: &str,
    index: usize,
    entry: &'a ExternEntry,
) -> Result<&'a str, ExtError> {
    let owner_len = entry.module_owner_len as usize;
    if owner_len == 0 {
        return Err(invalid_entry_table(
            extension,
            format!("entry {index} has an empty module owner"),
        ));
    }
    if owner_len > vo_common_core::extern_key::MAX_CANONICAL_MODULE_OWNER_BYTES {
        return Err(invalid_entry_table(
            extension,
            format!(
                "entry {index} module owner is {owner_len} bytes, exceeding the {}-byte limit",
                vo_common_core::extern_key::MAX_CANONICAL_MODULE_OWNER_BYTES,
            ),
        ));
    }
    if entry.module_owner_ptr.is_null() {
        return Err(invalid_entry_table(
            extension,
            format!("entry {index} has a null module owner pointer"),
        ));
    }
    // SAFETY: the raw table contract promises readable immutable bytes for
    // every non-null bounded range. UTF-8 and canonical spelling are checked
    // before the borrowed owner is used.
    let bytes = unsafe { std::slice::from_raw_parts(entry.module_owner_ptr, owner_len) };
    std::str::from_utf8(bytes).map_err(|error| {
        invalid_entry_table(
            extension,
            format!("entry {index} module owner is not UTF-8: {error}"),
        )
    })
}

#[derive(Debug)]
struct ValidatedExtensionEntry {
    name: String,
    func: ExternFnPtr,
    effects: ExternEffects,
}

/// Validate and snapshot the complete table before the library can be
/// registered. Ownership is exact and every raw entry participates.
fn validate_extension_entries<'a>(
    extension: &str,
    module_owner: &str,
    entries: impl IntoIterator<Item = (usize, &'a ExternEntry)>,
) -> Result<Vec<ValidatedExtensionEntry>, ExtError> {
    validate_canonical_module_owner(module_owner).map_err(|error| {
        invalid_entry_table(
            extension,
            format!("invalid canonical module owner '{module_owner}': {error}"),
        )
    })?;
    let mut seen = BTreeSet::new();
    let mut validated = Vec::new();
    for (index, entry) in entries {
        let extern_name = extern_entry_name(extension, index, entry)?;
        let entry_owner = extern_entry_module_owner(extension, index, entry)?;
        validate_canonical_module_owner(entry_owner).map_err(|error| {
            invalid_entry_table(
                extension,
                format!("entry {index} module owner '{entry_owner}' is invalid: {error}"),
            )
        })?;
        if entry_owner != module_owner {
            return Err(invalid_entry_table(
                extension,
                format!(
                    "entry {index} declares module owner '{entry_owner}', expected exact owner '{module_owner}'"
                ),
            ));
        }
        let key = decode_extern_name(extern_name).map_err(|error| {
            invalid_entry_table(
                extension,
                format!(
                    "entry {} extern name '{}' is not canonical: {}",
                    index, extern_name, error
                ),
            )
        })?;
        vo_common_core::extern_key::validate_canonical_extern_identity(key).map_err(|error| {
            invalid_entry_table(
                extension,
                format!(
                    "entry {index} extern '{extern_name}' has invalid language identity: {error}"
                ),
            )
        })?;
        if !key.is_owned_by_module(module_owner) {
            return Err(invalid_entry_table(
                extension,
                format!(
                    "entry {} extern '{}' belongs to package '{}', outside module owner '{}'",
                    index,
                    extern_name,
                    key.package(),
                    module_owner,
                ),
            ));
        }
        if !seen.insert(extern_name) {
            return Err(invalid_entry_table(
                extension,
                format!("duplicate extern name '{}'", extern_name),
            ));
        }
        let func = entry.func.ok_or_else(|| {
            invalid_entry_table(
                extension,
                format!(
                    "entry {} extern '{}' has a null function pointer",
                    index, extern_name
                ),
            )
        })?;
        let effects = ExternEffects::from_bits(entry.effects_bits).ok_or_else(|| {
            invalid_entry_table(
                extension,
                format!(
                    "entry {} extern '{}' has invalid effects bits 0x{:x}",
                    index, extern_name, entry.effects_bits
                ),
            )
        })?;
        validated.push(ValidatedExtensionEntry {
            name: extern_name.to_owned(),
            func,
            effects,
        });
    }
    Ok(validated)
}

fn extension_entry_is_active_for_owner<T>(
    name: &str,
    entry_owner: &str,
    owners: &BTreeSet<T>,
) -> bool
where
    T: Borrow<str> + Ord,
{
    decode_extern_name(name)
        .is_ok_and(|key| deepest_owning_module(key, owners) == Some(entry_owner))
}

/// A loaded extension entry.
#[derive(Clone, Copy)]
pub struct LoadedEntry {
    pub func: ExternFnPtr,
    pub effects: ExternEffects,
}

struct LoadedProviderEntry {
    name: String,
    entry: LoadedEntry,
}

/// A loaded extension.
struct LoadedExtension {
    /// Keep library alive.
    _lib: Library,
    /// Name of the extension.
    name: String,
    /// Canonical language namespace owned by this provider.
    module_owner: String,
    /// All extern functions.
    entries: Vec<LoadedProviderEntry>,
    /// Exact name index for owner-qualified catalog lookup.
    entry_index: BTreeMap<String, usize>,
}

/// Extension loader and registry.
pub struct ExtensionLoader {
    /// Loaded extensions.
    loaded: Vec<LoadedExtension>,
    /// Cache: function name -> (ext_idx, entry_idx).
    cache: BTreeMap<String, (usize, usize)>,
    /// Deduplicated canonical owner index for bounded deepest-owner lookup.
    owner_index: BTreeSet<String>,
    /// Canonical native library path -> committed specification index.
    ///
    /// An empty entry table still declares a module owner, so table contents
    /// cannot be used to detect one artifact being rebound to another owner.
    native_path_index: BTreeMap<PathBuf, usize>,
    specs: Vec<NativeExtensionSpec>,
}

impl ExtensionLoader {
    /// Create a new extension loader.
    pub fn new() -> Self {
        Self {
            loaded: Vec::new(),
            cache: BTreeMap::new(),
            owner_index: BTreeSet::new(),
            native_path_index: BTreeMap::new(),
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
        self.load_impl(spec.clone())
    }

    /// Load an extension from a dynamic library path.
    pub fn load(&mut self, path: &Path, name: &str, module_owner: &str) -> Result<(), ExtError> {
        self.load_impl(NativeExtensionSpec::new(
            name,
            module_owner,
            path.to_path_buf(),
            path.to_path_buf(),
        ))
    }

    fn load_impl(&mut self, mut spec: NativeExtensionSpec) -> Result<(), ExtError> {
        let path = spec.native_path.as_path();
        let name = spec.name.as_str();
        let module_owner = spec.module_owner.as_str();
        validate_canonical_module_owner(module_owner).map_err(|error| {
            invalid_entry_table(
                name,
                format!("invalid canonical module owner '{module_owner}': {error}"),
            )
        })?;
        // Canonicalize path to resolve .. and symlinks (needed for QEMU compatibility)
        let canonical_path = path
            .canonicalize()
            .map_err(|e| ExtError::LoadFailed(format!("{}: {}", path.display(), e)))?;
        if let Some(spec_index) = self.loaded_spec_index(name, module_owner, &canonical_path) {
            for resource in spec.lifetime_resources.drain(..) {
                self.specs[spec_index].retain_erased_lifetime_resource(resource);
            }
            return Ok(());
        }
        if let Some(existing) = self.loaded_spec_for_native_path(&canonical_path) {
            return Err(invalid_entry_table(
                name,
                format!(
                    "native extension path '{}' is already bound to module owner '{}' by extension '{}'; one canonical native artifact cannot be rebound to module owner '{module_owner}'",
                    canonical_path.display(),
                    existing.module_owner,
                    existing.name,
                ),
            ));
        }
        if let Some(existing) = self.loaded_spec_for_owner(module_owner) {
            return Err(invalid_entry_table(
                name,
                format!(
                    "module owner '{module_owner}' is already provided by extension '{}' at {}; one canonical module owner cannot be split across native providers",
                    existing.name,
                    existing.native_path.display(),
                ),
            ));
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
        let lib = unsafe {
            Library::new(&canonical_path).map_err(|e| ExtError::LoadFailed(e.to_string()))?
        };

        let get_abi_version: Symbol<extern "C" fn() -> u32> = unsafe {
            lib.get(b"vo_ext_get_abi_version")
                .map_err(|_| ExtError::MissingAbiVersion)?
        };
        validate_extension_abi_version(get_abi_version())?;

        let get_abi_fingerprint: Symbol<extern "C" fn() -> u64> = unsafe {
            lib.get(b"vo_ext_get_abi_fingerprint")
                .map_err(|_| ExtError::MissingAbiFingerprint)?
        };
        let found_fingerprint = get_abi_fingerprint();
        validate_extension_abi_fingerprint(found_fingerprint)?;

        let get_entries: Symbol<extern "C" fn() -> ExtensionTable> = unsafe {
            lib.get(b"vo_ext_get_entries")
                .map_err(|_| ExtError::MissingEntryPoint)?
        };
        let table = get_entries();
        validate_extension_abi_version(table.version)?;

        validate_extension_table_pointer(name, &table)?;
        let c_entries: &[ExternEntry] = if table.entry_count == 0 {
            &[]
        } else {
            unsafe { std::slice::from_raw_parts(table.entries, table.entry_count as usize) }
        };
        let validated_entries =
            validate_extension_entries(name, module_owner, c_entries.iter().enumerate())?;

        let mut entries = Vec::with_capacity(validated_entries.len());
        let mut entry_index = BTreeMap::new();

        for (entry_idx, entry) in validated_entries.iter().enumerate() {
            entry_index.insert(entry.name.clone(), entry_idx);
            entries.push(LoadedProviderEntry {
                name: entry.name.clone(),
                entry: LoadedEntry {
                    func: entry.func,
                    effects: entry.effects,
                },
            });
        }

        let loaded_extension = LoadedExtension {
            _lib: lib,
            name: name.to_string(),
            module_owner: module_owner.to_string(),
            entries,
            entry_index,
        };
        spec.native_path = canonical_path;
        self.record_loaded_spec(spec)?;
        self.loaded.push(loaded_extension);
        self.rebuild_active_cache();

        Ok(())
    }

    /// Rebuild the active extern catalog from the complete loaded owner set.
    ///
    /// Package ownership is selected before function lookup. An entry from a
    /// parent provider is therefore removed whenever a deeper loaded module
    /// owns its package, even when the deeper provider omits that function.
    /// Every loaded library and its full table remain alive so a later catalog
    /// rebuild cannot leave dangling function pointers.
    fn rebuild_active_cache(&mut self) {
        self.cache.clear();
        for (extension_index, extension) in self.loaded.iter().enumerate() {
            for (entry_index, provider_entry) in extension.entries.iter().enumerate() {
                if extension_entry_is_active_for_owner(
                    &provider_entry.name,
                    &extension.module_owner,
                    &self.owner_index,
                ) {
                    self.cache
                        .insert(provider_entry.name.clone(), (extension_index, entry_index));
                }
            }
        }
    }

    /// Lookup an extension function by name.
    pub fn lookup(&self, name: &str) -> Option<LoadedEntry> {
        let (ext_idx, entry_idx) = self.cache.get(name)?;
        Some(self.loaded[*ext_idx].entries[*entry_idx].entry)
    }

    /// Lookup an entry together with the exact canonical module owner proven
    /// while loading its native table.
    pub fn lookup_with_module_owner(&self, name: &str) -> Option<(&str, LoadedEntry)> {
        let (ext_idx, entry_idx) = self.cache.get(name)?;
        let extension = self.loaded.get(*ext_idx)?;
        Some((
            extension.module_owner.as_str(),
            extension.entries.get(*entry_idx)?.entry,
        ))
    }

    /// Lookup one provider entry inside an exact canonical module owner.
    ///
    /// This owner-qualified view is used while merging native and linkme
    /// catalogs after the deepest owner has already been selected from their
    /// union.
    pub fn lookup_in_module_owner(&self, module_owner: &str, name: &str) -> Option<LoadedEntry> {
        let extension = self
            .loaded
            .iter()
            .find(|extension| extension.module_owner == module_owner)?;
        let entry_index = *extension.entry_index.get(name)?;
        Some(extension.entries.get(entry_index)?.entry)
    }

    /// Iterate canonical module owners and their human-facing provider names.
    pub fn loaded_providers(&self) -> impl Iterator<Item = (&str, &str)> {
        self.loaded
            .iter()
            .map(|extension| (extension.module_owner.as_str(), extension.name.as_str()))
    }

    pub fn specs(&self) -> &[NativeExtensionSpec] {
        &self.specs
    }

    #[cfg(test)]
    fn has_loaded_spec(&self, name: &str, module_owner: &str, native_path: &Path) -> bool {
        self.loaded_spec_index(name, module_owner, native_path)
            .is_some()
    }

    fn loaded_spec_index(
        &self,
        name: &str,
        module_owner: &str,
        native_path: &Path,
    ) -> Option<usize> {
        self.specs.iter().position(|loaded| {
            loaded.name == name
                && loaded.module_owner == module_owner
                && loaded.native_path == native_path
        })
    }

    fn loaded_spec_for_owner(&self, module_owner: &str) -> Option<&NativeExtensionSpec> {
        self.specs
            .iter()
            .find(|loaded| loaded.module_owner == module_owner)
    }

    fn loaded_spec_for_native_path(&self, native_path: &Path) -> Option<&NativeExtensionSpec> {
        let spec_index = *self.native_path_index.get(native_path)?;
        self.specs.get(spec_index)
    }

    fn record_loaded_spec(&mut self, spec: NativeExtensionSpec) -> Result<(), ExtError> {
        if let Some(existing) = self.loaded_spec_for_native_path(&spec.native_path) {
            return Err(invalid_entry_table(
                &spec.name,
                format!(
                    "native extension path '{}' is already bound to module owner '{}' by extension '{}'",
                    spec.native_path.display(),
                    existing.module_owner,
                    existing.name,
                ),
            ));
        }
        if let Some(existing) = self.loaded_spec_for_owner(&spec.module_owner) {
            return Err(invalid_entry_table(
                &spec.name,
                format!(
                    "module owner '{}' is already provided by extension '{}' at {}",
                    spec.module_owner,
                    existing.name,
                    existing.native_path.display(),
                ),
            ));
        }
        let spec_index = self.specs.len();
        let native_path = spec.native_path.clone();
        let module_owner = spec.module_owner.clone();
        self.specs.push(spec);
        self.native_path_index.insert(native_path, spec_index);
        self.owner_index.insert(module_owner);
        Ok(())
    }

    /// Get a loaded extension library by canonical module owner.
    pub fn library_by_module_owner(&self, module_owner: &str) -> Option<&Library> {
        self.loaded
            .iter()
            .find(|extension| extension.module_owner == module_owner)
            .map(|ext| &ext._lib)
    }

    /// Lookup an arbitrary exported symbol from a loaded extension library.
    ///
    /// # Safety
    ///
    /// The caller must ensure `T` matches the actual symbol type exported by the library.
    pub unsafe fn symbol_by_module_owner<T>(
        &self,
        module_owner: &str,
        symbol: &[u8],
    ) -> Result<Option<Symbol<'_, T>>, ExtError> {
        let Some(lib) = self.library_by_module_owner(module_owner) else {
            return Ok(None);
        };
        match lib.get::<T>(symbol) {
            Ok(sym) => Ok(Some(sym)),
            Err(err) => Err(ExtError::SymbolLookupFailed {
                module_owner: module_owner.to_string(),
                symbol: String::from_utf8_lossy(symbol)
                    .trim_end_matches('\0')
                    .to_string(),
                message: err.to_string(),
            }),
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
    use std::sync::atomic::{AtomicUsize, Ordering};
    use vo_common_core::extern_key::ExternKeyRef;

    const OWNER: &str = "github.com/acme/demo";

    extern "C" fn dummy_ext(_ctx: *mut crate::ffi::ExtAbiContextV9) -> u32 {
        crate::ffi::ext_abi::RESULT_OK
    }

    fn extern_name(package: &str, function: &str) -> String {
        ExternKeyRef::new(package, function).encode().unwrap()
    }

    fn entry(name: &[u8]) -> ExternEntry {
        entry_with_owner(name, OWNER.as_bytes())
    }

    fn entry_with_owner(name: &[u8], module_owner: &[u8]) -> ExternEntry {
        ExternEntry {
            name_ptr: name.as_ptr(),
            name_len: name.len() as u32,
            module_owner_ptr: module_owner.as_ptr(),
            module_owner_len: module_owner.len() as u32,
            func: Some(dummy_ext),
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
    fn extension_table_rejects_entry_count_over_limit() {
        let table = ExtensionTable {
            version: ABI_VERSION,
            entry_count: MAX_EXTENSION_ENTRIES + 1,
            entries: core::ptr::NonNull::<ExternEntry>::dangling().as_ptr(),
        };

        let err = validate_extension_table_pointer("oversized", &table)
            .expect_err("oversized entry table must be rejected before slicing");
        assert!(err.to_string().contains("entry count"));
        assert!(err.to_string().contains("exceeds limit"));
    }

    #[test]
    fn extension_table_rejects_misaligned_entries_pointer() {
        let mut storage = [0_u8; core::mem::size_of::<ExternEntry>() + 8];
        let misaligned = (0..storage.len())
            .map(|offset| unsafe { storage.as_mut_ptr().add(offset) })
            .find(|ptr| !(*ptr as usize).is_multiple_of(core::mem::align_of::<ExternEntry>()))
            .expect("storage contains a misaligned address")
            .cast::<ExternEntry>();
        let table = ExtensionTable {
            version: ABI_VERSION,
            entry_count: 1,
            entries: misaligned,
        };

        let err = validate_extension_table_pointer("misaligned", &table)
            .expect_err("misaligned entries pointer must be rejected before slicing");
        assert!(err.to_string().contains("misaligned"));
    }

    #[test]
    fn extension_entry_rejects_name_length_over_limit_before_reading_pointer() {
        let mut oversized = entry(b"placeholder");
        oversized.name_len = MAX_EXTERN_NAME_BYTES as u32 + 1;
        oversized.name_ptr = core::ptr::NonNull::<u8>::dangling().as_ptr();

        let err = extern_entry_name("oversized", 0, &oversized)
            .expect_err("oversized name must be rejected before pointer access");
        assert!(err.to_string().contains("name length"));
        assert!(err.to_string().contains("exceeds limit"));
    }

    #[test]
    fn extension_entry_rejects_owner_length_over_limit_before_reading_pointer() {
        let mut oversized = entry(b"placeholder");
        oversized.module_owner_len =
            vo_common_core::extern_key::MAX_CANONICAL_MODULE_OWNER_BYTES as u32 + 1;
        oversized.module_owner_ptr = core::ptr::NonNull::<u8>::dangling().as_ptr();

        let err = extern_entry_module_owner("oversized", 0, &oversized)
            .expect_err("oversized owner must be rejected before pointer access");
        assert!(err.to_string().contains("module owner"));
        assert!(err.to_string().contains("exceeding"));
    }

    #[test]
    fn extension_entries_require_exact_canonical_owner_metadata() {
        let name = extern_name(&format!("{OWNER}/render"), "Run");
        let foreign_owner = b"github.com/acme/other";
        let entries = [entry_with_owner(name.as_bytes(), foreign_owner)];
        let error = validate_extension_entries("owner", OWNER, entries.iter().enumerate())
            .expect_err("entry owner must exactly match the loader spec owner");
        assert!(
            error.to_string().contains("expected exact owner"),
            "{error}"
        );

        let invalid_owner = b"local/demo";
        let entries = [entry_with_owner(name.as_bytes(), invalid_owner)];
        let error = validate_extension_entries("owner", OWNER, entries.iter().enumerate())
            .expect_err("entry owner must be canonical");
        assert!(error.to_string().contains("module owner"), "{error}");
        assert!(error.to_string().contains("invalid"), "{error}");
    }

    #[test]
    fn extension_entry_owner_rejects_traversal_and_accepts_portable_unicode_descendants() {
        let traversal = extern_name(&format!("{OWNER}/../evil"), "Run");
        let entries = [entry(traversal.as_bytes())];
        let error = validate_extension_entries("traversal", OWNER, entries.iter().enumerate())
            .expect_err("owner prefix cannot authorize traversal");
        assert!(
            error.to_string().contains("invalid language identity"),
            "{error}"
        );

        let unicode = extern_name(&format!("{OWNER}/图形/é"), "方法");
        let entries = [entry(unicode.as_bytes())];
        validate_extension_entries("unicode", OWNER, entries.iter().enumerate())
            .expect("portable Unicode descendants belong to their exact owner");
    }

    #[test]
    fn extension_table_rejects_invalid_effect_bits() {
        let name = extern_name(OWNER, "BadEffects");
        let mut entries = [entry(name.as_bytes())];
        entries[0].effects_bits = ExternEffects::ALLOWED_BITS << 1;
        let err = validate_extension_entries("bad", OWNER, entries.iter().enumerate())
            .expect_err("invalid effects");
        assert!(err.to_string().contains("invalid effects bits"));
    }

    #[test]
    fn extension_table_rejects_null_function_pointer_as_data() {
        let name = extern_name(OWNER, "NullFunction");
        let mut entries = [entry(name.as_bytes())];
        entries[0].func = None;

        let err = validate_extension_entries("bad", OWNER, entries.iter().enumerate())
            .expect_err("null function pointer");
        assert!(err.to_string().contains("null function pointer"));
    }

    #[test]
    fn extension_table_rejects_legacy_and_malformed_names() {
        for name in ["demo_Run", "vo1:01:x:1:F", "vo1:1:x:1:Fx"] {
            let entries = [entry(name.as_bytes())];
            let error = validate_extension_entries("demo", OWNER, entries.iter().enumerate())
                .expect_err("non-canonical name must invalidate the complete table");
            assert!(error.to_string().contains("not canonical"), "{name}");
        }

        let decomposed_package = format!("{OWNER}/e\u{301}");
        for (package, function) in [
            (decomposed_package.as_str(), "Run"),
            (OWNER, "for"),
            (OWNER, "_"),
            (OWNER, "bad-name"),
            (OWNER, "\u{301}Bad"),
        ] {
            let name = extern_name(package, function);
            let entries = [entry(name.as_bytes())];
            let error = validate_extension_entries("demo", OWNER, entries.iter().enumerate())
                .expect_err("semantic-invalid name must invalidate the complete table");
            assert!(
                error.to_string().contains("invalid language identity"),
                "{error}"
            );
        }
    }

    #[test]
    fn extension_table_rejects_every_foreign_owner_entry() {
        let local = extern_name(OWNER, "Local");
        let foreign = extern_name("github.com/other/provider", "Foreign");
        let entries = [entry(local.as_bytes()), entry(foreign.as_bytes())];
        let error = validate_extension_entries("demo", OWNER, entries.iter().enumerate())
            .expect_err("one foreign entry must invalidate the complete table");
        assert!(error.to_string().contains("outside module owner"));
    }

    #[test]
    fn extension_table_rejects_noncanonical_module_owners() {
        let name = extern_name("github.com/acme/demo", "Run");
        let entries = [entry(name.as_bytes())];
        for owner in [
            "",
            "local/demo",
            "github.com/acme//demo",
            "github.com/acme/demo/",
            "github.com/acme/demo/v1",
        ] {
            let error = validate_extension_entries("demo", owner, entries.iter().enumerate())
                .expect_err("invalid owner must reject the complete table");
            assert!(
                error.to_string().contains("invalid canonical module owner"),
                "{owner:?}: {error}"
            );
            assert!(NativeExtensionSpec::try_new(
                "demo",
                owner,
                PathBuf::from("libdemo.so"),
                PathBuf::from("vo.mod")
            )
            .is_err());
        }
    }

    #[test]
    fn spec_constructor_defers_invalid_owner_to_fallible_load_boundary() {
        let spec = NativeExtensionSpec::new(
            "invalid-owner",
            "local/demo",
            PathBuf::from("missing-native-extension"),
            PathBuf::from("vo.mod"),
        );
        let mut loader = ExtensionLoader::new();

        let error = loader
            .load_spec(&spec)
            .expect_err("loading must reject an invalid owner without constructor panic");
        let message = error.to_string();
        assert!(
            message.contains("invalid canonical module owner"),
            "{message}"
        );
        assert!(loader.specs().is_empty());
    }

    #[test]
    fn specification_clones_retain_opaque_lifetime_resource() {
        struct DropProbe(Arc<AtomicUsize>);

        impl Drop for DropProbe {
            fn drop(&mut self) {
                self.0.fetch_add(1, Ordering::SeqCst);
            }
        }

        let drops = Arc::new(AtomicUsize::new(0));
        let root = std::env::temp_dir().join(format!(
            "vo-ext-lifetime-resource-{}-{:?}",
            std::process::id(),
            std::thread::current().id(),
        ));
        let _ = std::fs::remove_dir_all(&root);
        std::fs::create_dir_all(&root).unwrap();
        let native_path = root.join("libleased.so");
        std::fs::write(&native_path, b"duplicate load returns before opening").unwrap();
        let native_path = native_path.canonicalize().unwrap();
        let mut spec =
            NativeExtensionSpec::new("leased", OWNER, native_path.clone(), root.join("vo.mod"));
        let resource = Arc::new(DropProbe(Arc::clone(&drops)));
        spec.retain_lifetime_resource(Arc::clone(&resource));
        spec.retain_lifetime_resource(Arc::clone(&resource));
        assert_eq!(spec.lifetime_resources.len(), 1);
        drop(resource);
        let identity_only =
            NativeExtensionSpec::new("leased", OWNER, native_path, root.join("vo.mod"));
        assert_eq!(spec, identity_only, "a lease is lifecycle-only state");

        let mut loader = ExtensionLoader::new();
        loader
            .record_loaded_spec(identity_only)
            .expect("record identity without a lease");
        let retained_clone = spec.clone();
        drop(spec);
        assert_eq!(drops.load(Ordering::SeqCst), 0);
        loader
            .load_spec(&retained_clone)
            .expect("an idempotent load must merge its lifetime resources");
        loader
            .load_spec(&retained_clone)
            .expect("repeated idempotent loads must remain bounded");
        assert_eq!(loader.specs()[0].lifetime_resources.len(), 1);
        drop(retained_clone);
        assert_eq!(drops.load(Ordering::SeqCst), 0);
        drop(loader);
        assert_eq!(drops.load(Ordering::SeqCst), 1);
        std::fs::remove_dir_all(root).unwrap();
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
        let name = extern_name(OWNER, "Duplicate");
        let entries = [entry(name.as_bytes()), entry(name.as_bytes())];

        let err = validate_extension_entries("dups", OWNER, entries.iter().enumerate())
            .expect_err("duplicate name");
        assert!(err.to_string().contains("duplicate extern name"));
    }

    #[test]
    fn native_owner_routing_is_order_independent_and_never_falls_back() {
        let parent = "github.com/acme/mono";
        let child = "github.com/acme/mono/graphics";
        let name = extern_name("github.com/acme/mono/graphics/render", "Missing");

        for owners in [
            BTreeSet::from([parent, child]),
            BTreeSet::from([child, parent]),
        ] {
            assert!(
                !extension_entry_is_active_for_owner(&name, parent, &owners),
                "parent entry must be shadowed once the nested owner is loaded"
            );
            assert!(extension_entry_is_active_for_owner(&name, child, &owners));
        }

        assert!(extension_entry_is_active_for_owner(
            &name,
            parent,
            &BTreeSet::from([parent])
        ));
        // With only a parent entry in the complete catalog and both owners
        // loaded, no active route exists. Function lookup cannot fall back.
        let parent_entries = [(parent, name.as_str())];
        let nested_owners = BTreeSet::from([parent, child]);
        let active = parent_entries
            .iter()
            .filter(|(owner, name)| {
                extension_entry_is_active_for_owner(name, owner, &nested_owners)
            })
            .count();
        assert_eq!(active, 0);
    }

    #[test]
    fn extension_entries_accept_unique_extern_names() {
        let one = extern_name(OWNER, "One");
        let two = extern_name(&format!("{OWNER}/unicode"), "方法");
        let entries = [entry(one.as_bytes()), entry(two.as_bytes())];

        let validated = validate_extension_entries("ok", OWNER, entries.iter().enumerate())
            .expect("unique names");
        assert_eq!(
            validated
                .iter()
                .map(|entry| entry.name.as_str())
                .collect::<Vec<_>>(),
            [one.as_str(), two.as_str()]
        );
    }

    #[test]
    fn extension_entry_ownership_uses_exact_module_boundaries() {
        assert!(vo_common_core::extern_key::ExternKeyRef::new(OWNER, "F").is_owned_by_module(OWNER));
        assert!(
            vo_common_core::extern_key::ExternKeyRef::new(&format!("{OWNER}/render"), "F")
                .is_owned_by_module(OWNER)
        );
        assert!(
            !vo_common_core::extern_key::ExternKeyRef::new("github.com/acme/demolition", "F")
                .is_owned_by_module(OWNER)
        );
        assert!(
            !vo_common_core::extern_key::ExternKeyRef::new("github.com/acme/demo2", "F")
                .is_owned_by_module(OWNER)
        );
    }

    #[test]
    fn canonical_names_separate_legacy_path_and_tuple_collisions() {
        let names = [
            extern_name(&format!("{OWNER}/a/b"), "F"),
            extern_name(&format!("{OWNER}/a_b"), "F"),
            extern_name(&format!("{OWNER}/a"), "b_C"),
            extern_name(&format!("{OWNER}/a_b"), "C"),
        ];
        let entries = names
            .iter()
            .map(|name| entry(name.as_bytes()))
            .collect::<Vec<_>>();
        let validated = validate_extension_entries("demo", OWNER, entries.iter().enumerate())
            .expect("injective names remain distinct");
        assert_eq!(validated.len(), names.len());
    }

    #[test]
    fn load_impl_skips_exact_duplicate_specs() {
        let root = std::env::temp_dir().join(format!(
            "vo-ext-exact-duplicate-{}-{:?}",
            std::process::id(),
            std::thread::current().id()
        ));
        std::fs::create_dir_all(&root).unwrap();
        let path = root.join("libsame.so");
        std::fs::write(
            &path,
            b"not a library; an exact duplicate must return before loading",
        )
        .unwrap();
        let native_path = path.canonicalize().unwrap();
        let mut loader = ExtensionLoader::new();
        loader
            .record_loaded_spec(NativeExtensionSpec::new(
                "same",
                OWNER,
                native_path.clone(),
                root.join("vo.mod"),
            ))
            .expect("record the initial provider");

        loader
            .load(&path, "same", OWNER)
            .expect("same name, owner, and canonical path are idempotent");
        assert_eq!(loader.specs.len(), 1);
        assert!(loader.has_loaded_spec("same", OWNER, &native_path));
        assert!(!loader.has_loaded_spec("same", OWNER, Path::new("/tmp/other.so")));
        assert!(!loader.has_loaded_spec("same", "github.com/acme/other", &native_path));
        assert!(!loader.has_loaded_spec("other", OWNER, &native_path));

        std::fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn empty_table_provider_path_cannot_be_rebound_to_another_owner() {
        let root = std::env::temp_dir().join(format!(
            "vo-ext-empty-path-owner-{}-{:?}",
            std::process::id(),
            std::thread::current().id()
        ));
        std::fs::create_dir_all(&root).unwrap();
        let path = root.join("libempty.so");
        std::fs::write(
            &path,
            b"the path conflict must be detected before opening this placeholder",
        )
        .unwrap();
        let canonical_path = path.canonicalize().unwrap();
        let mut loader = ExtensionLoader::new();

        // A successfully validated zero-entry table commits the same spec and
        // path ownership metadata as a non-empty table.
        loader
            .record_loaded_spec(NativeExtensionSpec::new(
                "empty",
                OWNER,
                canonical_path.clone(),
                root.join("vo.mod"),
            ))
            .expect("record the zero-entry provider");

        let other_owner = "github.com/acme/other";
        let error = loader
            .load(&path, "empty-rebound", other_owner)
            .expect_err("one canonical native path cannot acquire a second owner");
        let message = error.to_string();
        assert!(
            message.contains("already bound to module owner"),
            "{message}"
        );
        assert!(message.contains(OWNER), "{message}");
        assert!(message.contains(other_owner), "{message}");
        assert_eq!(loader.specs.len(), 1);
        assert_eq!(
            loader
                .loaded_spec_for_native_path(&canonical_path)
                .map(|spec| spec.module_owner.as_str()),
            Some(OWNER)
        );

        std::fs::remove_dir_all(root).unwrap();
    }

    #[cfg(unix)]
    #[test]
    fn symlink_alias_cannot_rebind_canonical_native_path() {
        use std::os::unix::fs::symlink;

        let root = std::env::temp_dir().join(format!(
            "vo-ext-symlink-path-owner-{}-{:?}",
            std::process::id(),
            std::thread::current().id()
        ));
        std::fs::create_dir_all(&root).unwrap();
        let target = root.join("libtarget.so");
        let alias = root.join("libalias.so");
        std::fs::write(
            &target,
            b"the canonical path conflict must be detected before opening",
        )
        .unwrap();
        symlink(&target, &alias).unwrap();
        let canonical_path = target.canonicalize().unwrap();
        assert_eq!(alias.canonicalize().unwrap(), canonical_path);

        let mut loader = ExtensionLoader::new();
        loader
            .record_loaded_spec(NativeExtensionSpec::new(
                "target",
                OWNER,
                canonical_path,
                root.join("vo.mod"),
            ))
            .expect("record the symlink target provider");

        let error = loader
            .load(&alias, "alias", "github.com/acme/alias")
            .expect_err("a symlink alias cannot bypass native path ownership");
        let message = error.to_string();
        assert!(
            message.contains("already bound to module owner"),
            "{message}"
        );
        assert_eq!(loader.specs.len(), 1);

        std::fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn one_module_owner_cannot_be_split_across_native_providers() {
        let root = std::env::temp_dir().join(format!(
            "vo-ext-owner-conflict-{}-{:?}",
            std::process::id(),
            std::thread::current().id()
        ));
        std::fs::create_dir_all(&root).unwrap();
        let primary_path = root.join("libprimary.so");
        let secondary_path = root.join("libsecondary.so");
        std::fs::write(&primary_path, b"primary").unwrap();
        std::fs::write(&secondary_path, b"secondary").unwrap();

        let mut loader = ExtensionLoader::new();
        loader
            .record_loaded_spec(NativeExtensionSpec::new(
                "primary",
                OWNER,
                primary_path.canonicalize().unwrap(),
                root.join("primary.vo.mod"),
            ))
            .expect("record the primary owner provider");

        let error = loader
            .load(&secondary_path, "secondary", OWNER)
            .expect_err("a second provider for one owner must fail before loading its bytes");
        assert!(error
            .to_string()
            .contains("cannot be split across native providers"));
        assert!(!loader.has_loaded_spec(
            "secondary",
            OWNER,
            &secondary_path.canonicalize().unwrap()
        ));

        std::fs::remove_dir_all(root).unwrap();
    }
}
