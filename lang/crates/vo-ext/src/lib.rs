//! SDK for creating Vo native extensions.
//!
//! This crate provides the API for implementing Vo functions in Rust.

#![cfg_attr(not(feature = "native"), no_std)]
//!
//! # Platform Support
//!
//! - **Native** (default): Extensions are compiled as dynamic libraries (.so/.dylib/.dll)
//!   and loaded by the Vo runtime at startup via dlopen.
//! - **WASM static link**: `#[vo_fn]` generates a typed provider entry that the
//!   alloc-only embedding runtime registers at initialization. The `wasm`
//!   feature keeps both this SDK and its target dependency closure `no_std`.
//! - **Browser WASM v3 artifact**: the final artifact supplies the protocol's
//!   bytes-ABI wrapper. `#[vo_fn]` publishes exact export-key metadata and does
//!   not synthesize that serialization boundary.
//!
//! # Example
//!
//! ```ignore
//! use vo_ext::prelude::*;
//!
//! #[vo_fn("mylib/math", "FastAdd")]
//! fn fast_add(a: i64, b: i64) -> i64 {
//!     a + b
//! }
//!
//! // Required: export the extension table
//! vo_ext::export_extensions!();
//! ```
//!
//! With `module = "github.com/example/mylib"` declared in `../vo.mod`, the
//! extension crate identifies that authoritative file and declares the runtime
//! used by generated ABI code. The macro resolves `mylib/math` to the complete
//! package `github.com/example/mylib/math` before encoding the extern key:
//!
//! ```toml
//! [dependencies]
//! vo-ext = "=0.1.1"
//! vo-runtime = "=0.1.1"
//!
//! [package.metadata.vo]
//! vomod = "../vo.mod"
//! ```
//!
//! `export_extensions!` reads the same authoritative metadata and emits one
//! independent module-owner declaration. The owner therefore remains reserved
//! when the static extern table is empty. Invoke it exactly once in the final
//! extension crate; duplicate invocations fail at compile time.
//!
//! # Browser WASM v3 wrapper
//!
//! A standalone browser artifact pairs the typed implementation above with an
//! artifact-owned bytes-ABI wrapper. `#[vo_wasm_export]` validates the exact C
//! signature and source declaration, injects the collision-free export name,
//! and tracks the owning metadata and Vo source files for incremental builds:
//!
//! ```ignore
//! #[vo_wasm_export("mylib/math", "FastAdd")]
//! pub extern "C" fn fast_add_v3(
//!     input_ptr: u32,
//!     input_len: u32,
//!     out_len_ptr: u32,
//! ) -> u32 {
//!     browser_v3::dispatch_fast_add(input_ptr, input_len, out_len_ptr)
//! }
//!
//! vo_ext::export_wasm_extension_protocol!();
//! ```
//!
//! A wasm-bindgen artifact uses the synchronous bytes wrapper. The macro emits
//! the same exact key as `js_name`; wasm-bindgen exposes it as a JavaScript
//! function accepting and returning `Uint8Array`:
//!
//! ```ignore
//! #[vo_wasm_bindgen_export("mylib/math", "FastAdd")]
//! pub fn fast_add_bindgen(input: &[u8]) -> Vec<u8> {
//!     browser_v3::dispatch_fast_add_bytes(input)
//! }
//! ```
//!
//! The standalone artifact additionally exports the protocol allocator and
//! deallocator. Both forms own decoding inputs plus encoding the tagged output
//! stream. The browser host looks up only the exact key derived from the full
//! canonical extern name.
//!
//! Incremental dependency markers cover `Cargo.toml`, the configured `vo.mod`,
//! selected workspace metadata, and every Vo source file consulted during macro
//! expansion. They also observe `VOWORK` and `VO_FFI_SOURCE_FINGERPRINT`.
//! Supported builds run through `vo build .`; the engine hashes Rust and Vo
//! source membership and bytes, requires a checked-in `Cargo.lock`, invokes
//! Cargo with `--locked`, and supplies the source fingerprint. A caller that
//! invokes Cargo directly must change `VO_FFI_SOURCE_FINGERPRINT` whenever
//! relevant source membership changes, or perform a clean rebuild.
//!
//! # Extension Structure
//!
//! A typical extension project:
//!
//! ```text
//! myext/
//! ├── vo.mod               # Module and extension metadata
//! ├── math/
//! │   └── math.vo          # Vo interface (extern declarations)
//! └── native/
//!     ├── Cargo.toml
//!     └── src/
//!         └── lib.rs       # Native implementation
//! ```

#[cfg(feature = "native")]
pub mod host;

pub use vo_common_core::extern_key::{
    wasm_extension_export_key, WASM_EXTENSION_EXPORT_PREFIX, WASM_EXTENSION_PROTOCOL_VERSION,
};
#[doc(hidden)]
pub use vo_ffi_macro::__vo_extension_module_owner;
pub use vo_ffi_macro::{vo_extension_entry, vo_fn, vo_wasm_bindgen_export, vo_wasm_export};
pub use vo_runtime::ffi::{
    // Extension ABI types (available on all platforms)
    ext_abi,
    ExternCallContext,
    ExternFiberInputs,
    ExternFn,
    ExternFnPtr,
    ExternInvoke,
    ExternRegistry,
    ExternResult,
    ExternWorld,
    // Type-safe slot wrapper for interface types
    InterfaceSlot,
    StdlibEntry,
    VoArray,
    VoArrayCursor,
    VoBytes,
    VoClosure,
    VoElem,
    // Pointer and closure accessors
    VoPtr,
    // Container accessors
    VoSlice,
    VoSliceCursor,
    VoString,
    VoStringElem,
    VoWritableElem,
};
// Extension ABI types (native only - require dylib boundary support)
#[cfg(feature = "native")]
pub use vo_runtime::ffi::{ExtensionTable, ExternEntry, ExternModuleOwnerEntry};
pub use vo_runtime::gc::GcRef;

// Native platform: re-export linkme registration through the public SDK so
// downstream crates do not need the implementation dependency directly.
#[cfg(feature = "native")]
pub use vo_runtime::ffi::{EXTERN_MODULE_OWNER_TABLE, EXTERN_TABLE};
#[cfg(feature = "native")]
pub use vo_runtime::{__linkme, distributed_slice};

/// ABI version for extension compatibility checking.
/// Must match `vo_runtime::ext_loader::ABI_VERSION`.
pub const ABI_VERSION: u32 = vo_runtime::ffi::EXTENSION_ABI_VERSION;
pub const ABI_FINGERPRINT: u64 = vo_runtime::ffi::EXTENSION_ABI_FINGERPRINT;

/// Export the browser extension protocol epoch from one final WASM artifact.
///
/// Call this exactly once in the root crate that produces the `.wasm` file.
/// Dependency extension crates must not call it because a final artifact has
/// one protocol identity. Browser hosts read the function from the raw
/// `WebAssembly.Instance.exports` returned by standalone or wasm-bindgen
/// initialization, so the extension crate needs no direct wasm-bindgen
/// dependency for this export.
///
/// ```ignore
/// #[cfg(target_arch = "wasm32")]
/// vo_ext::export_wasm_extension_protocol!();
/// ```
#[macro_export]
macro_rules! export_wasm_extension_protocol {
    () => {
        #[cfg(target_arch = "wasm32")]
        #[no_mangle]
        pub extern "C" fn vo_ext_protocol_version() -> u32 {
            $crate::WASM_EXTENSION_PROTOCOL_VERSION
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __vo_declare_extension_module_owner {
    () => {
        #[doc(hidden)]
        const __VO_EXTENSION_MODULE_OWNER_DECLARATION_ONCE: () = ();

        #[doc(hidden)]
        const __VO_EXTENSION_MODULE_OWNER: &str = $crate::__vo_extension_module_owner!();

        #[cfg(not(target_arch = "wasm32"))]
        #[$crate::distributed_slice($crate::EXTERN_MODULE_OWNER_TABLE)]
        #[linkme(crate = $crate::__linkme)]
        #[doc(hidden)]
        static __VO_EXTENSION_MODULE_OWNER_ENTRY: $crate::ExternModuleOwnerEntry =
            $crate::ExternModuleOwnerEntry {
                module_owner_ptr: __VO_EXTENSION_MODULE_OWNER.as_ptr(),
                module_owner_len: __VO_EXTENSION_MODULE_OWNER.len() as u32,
            };
    };
}

/// Export the extension entry point.
///
/// # Native Platform (default)
/// Call with no arguments. Generates `vo_ext_get_entries` function for dlopen.
/// The function returns `ExtensionTable` containing all registered
/// extension functions. Invoke this macro exactly once in the final extension
/// crate; the generated module-owner declaration intentionally uses a fixed
/// sentinel so duplicate invocations fail at compile time. Both forms require
/// `[package.metadata.vo].vomod`, including an empty extension with no
/// `#[vo_fn]` entries, because owner identity comes from the authoritative
/// module manifest rather than from function entries.
/// ```ignore
/// vo_ext::export_extensions!();
/// ```
///
/// # Explicit Entries
/// Call with a list of extension-entry constant expressions to export only
/// those entries. [`vo_extension_entry!`](crate::vo_extension_entry) provides
/// the stable public spelling and resolves to the exact constant generated by
/// `#[vo_fn]`. This is useful when a native extension links another extension
/// crate and must avoid exporting the dependency's linkme entries. Every
/// expression appears exactly once in the selected target's static table.
/// ```ignore
/// vo_ext::export_extensions!(
///     vo_ext::vo_extension_entry!("gui", "emitRender"),
///     vo_ext::vo_extension_entry!(navigation, "gui", "navigate"),
/// );
/// ```
#[macro_export]
macro_rules! export_extensions {
    // Native: no arguments, auto-collect from linkme table
    () => {
        $crate::__vo_declare_extension_module_owner!();

        #[no_mangle]
        pub extern "C" fn vo_ext_get_abi_version() -> u32 {
            $crate::ABI_VERSION
        }

        #[no_mangle]
        pub extern "C" fn vo_ext_get_entries() -> $crate::ExtensionTable {
            $crate::ExtensionTable {
                version: $crate::ABI_VERSION,
                entry_count: $crate::EXTERN_TABLE.len() as u32,
                entries: $crate::EXTERN_TABLE.as_ptr(),
            }
        }

        #[no_mangle]
        pub extern "C" fn vo_ext_get_abi_fingerprint() -> u64 {
            $crate::ABI_FINGERPRINT
        }
    };

    // Explicit entries: native exports exactly this table; wasm exposes the
    // StdlibEntry list so callers can implement static registration.
    ($($entry:expr),+ $(,)?) => {
        $crate::__vo_declare_extension_module_owner!();

        /// All extern entries for this extension.
        #[cfg(not(target_arch = "wasm32"))]
        pub static VO_EXT_ENTRIES: &[$crate::ExternEntry] = &[$($entry),*];

        /// All extern entries for this extension.
        #[cfg(target_arch = "wasm32")]
        pub static VO_EXT_ENTRIES: &[$crate::StdlibEntry] = &[$($entry),*];

        #[cfg(not(target_arch = "wasm32"))]
        #[no_mangle]
        pub extern "C" fn vo_ext_get_abi_version() -> u32 {
            $crate::ABI_VERSION
        }

        #[cfg(not(target_arch = "wasm32"))]
        #[no_mangle]
        pub extern "C" fn vo_ext_get_entries() -> $crate::ExtensionTable {
            $crate::ExtensionTable {
                version: $crate::ABI_VERSION,
                entry_count: VO_EXT_ENTRIES.len() as u32,
                entries: VO_EXT_ENTRIES.as_ptr(),
            }
        }

        #[cfg(not(target_arch = "wasm32"))]
        #[no_mangle]
        pub extern "C" fn vo_ext_get_abi_fingerprint() -> u64 {
            $crate::ABI_FINGERPRINT
        }
    };
}

/// Prelude module for convenient imports.
pub mod prelude {
    pub use crate::export_extensions;
    pub use crate::export_wasm_extension_protocol;
    pub use crate::vo_extension_entry;
    pub use crate::vo_fn;
    pub use crate::vo_wasm_bindgen_export;
    pub use crate::vo_wasm_export;
    pub use crate::wasm_extension_export_key;
    pub use crate::ExternCallContext;
    pub use crate::ExternResult;
    pub use crate::GcRef;
    pub use crate::InterfaceSlot;
    // Extension ABI types
    pub use crate::{ext_abi, ExternFnPtr};
    // Container accessors
    pub use crate::{VoArray, VoArrayCursor, VoBytes, VoString};
    pub use crate::{VoElem, VoStringElem, VoWritableElem};
    pub use crate::{VoSlice, VoSliceCursor};
    // Pointer and closure accessors
    pub use crate::{VoClosure, VoPtr};
}

#[cfg(test)]
mod tests {
    #[test]
    fn extension_sdk_exports_runtime_abi_v9() {
        assert_eq!(super::ABI_VERSION, 9);
    }

    #[test]
    fn browser_protocol_epoch_is_independent_and_explicit() {
        assert_eq!(super::WASM_EXTENSION_PROTOCOL_VERSION, 3);
        assert_eq!(super::WASM_EXTENSION_EXPORT_PREFIX, "__vo_ext_");
        assert_eq!(
            super::wasm_extension_export_key("vo1:1:x:1:F").unwrap(),
            "__vo_ext_766f313a313a783a313a46"
        );
        let source = include_str!("lib.rs");
        assert!(source.contains("export_wasm_extension_protocol"));
        assert!(source.contains("pub extern \"C\" fn vo_ext_protocol_version()"));
    }
}
