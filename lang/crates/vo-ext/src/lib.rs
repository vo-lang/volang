//! SDK for creating Vo native extensions.
//!
//! This crate provides the API for implementing Vo functions in Rust.
//!
//! # Platform Support
//!
//! - **Native** (default): Extensions are compiled as dynamic libraries (.so/.dylib/.dll)
//!   and loaded by the Vo runtime at startup via dlopen.
//! - **WASM**: Extensions are statically linked and registered at initialization.
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
//! # Extension Structure
//!
//! A typical extension project:
//!
//! ```text
//! myext/
//! ├── vo.ext.toml          # Extension manifest
//! ├── math/
//! │   └── math.vo          # Vo interface (extern declarations)
//! └── native/
//!     ├── Cargo.toml
//!     └── src/
//!         └── lib.rs       # Native implementation
//! ```

#[cfg(feature = "native")]
pub mod host;

pub use vo_ffi_macro::vo_fn;
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
    VoMap,
    VoMapCursor,
    // Pointer and closure accessors
    VoPtr,
    // Container accessors
    VoSlice,
    VoSliceCursor,
    VoString,
    VoStringElem,
};
// Extension ABI types (native only - require dylib boundary support)
#[cfg(feature = "native")]
pub use vo_runtime::ffi::{ExtensionTable, ExternEntry};
pub use vo_runtime::gc::GcRef;

// Native platform: re-export linkme types for auto-registration
#[cfg(feature = "native")]
pub use vo_runtime::distributed_slice;
#[cfg(feature = "native")]
pub use vo_runtime::ffi::EXTERN_TABLE;

/// ABI version for extension compatibility checking.
/// Must match `vo_runtime::ext_loader::ABI_VERSION`.
pub const ABI_VERSION: u32 = 2;

/// Export the extension entry point.
///
/// # Native Platform (default)
/// Call with no arguments. Generates `vo_ext_get_entries` function for dlopen.
/// The function returns `ExtensionTable` containing all registered
/// extension functions.
/// ```ignore
/// vo_ext::export_extensions!();
/// ```
///
/// # WASM Platform
/// Call with list of StdlibEntry constants. Generates `vo_ext_register` function.
/// ```ignore
/// vo_ext::export_extensions!(__EXT_gui_emitRender, __EXT_gui_navigate);
/// ```
#[macro_export]
macro_rules! export_extensions {
    // Native: no arguments, auto-collect from linkme table
    () => {
        #[no_mangle]
        pub extern "C" fn vo_ext_get_entries() -> $crate::ExtensionTable {
            $crate::ExtensionTable {
                version: $crate::ABI_VERSION,
                entry_count: $crate::EXTERN_TABLE.len() as u32,
                entries: $crate::EXTERN_TABLE.as_ptr(),
            }
        }

        /// Receive a host bridge pointer from the extension loader.
        #[no_mangle]
        pub unsafe extern "C" fn vo_ext_set_host_bridge(ptr: usize) {
            $crate::host::receive_host_bridge(ptr);
        }

        /// Clear the host bridge reference before the host drops it.
        #[no_mangle]
        pub extern "C" fn vo_ext_clear_host_bridge() {
            $crate::host::receive_clear_host_bridge();
        }
    };

    // WASM: explicit list of entries (caller must implement registration)
    ($($entry:ident),+ $(,)?) => {
        /// All extern entries for this extension.
        pub static VO_EXT_ENTRIES: &[$crate::StdlibEntry] = &[$($entry),*];
    };
}

/// Prelude module for convenient imports.
pub mod prelude {
    pub use crate::export_extensions;
    pub use crate::vo_fn;
    pub use crate::ExternCallContext;
    pub use crate::ExternResult;
    pub use crate::GcRef;
    pub use crate::InterfaceSlot;
    // Extension ABI types
    pub use crate::{ext_abi, ExternFnPtr};
    // Container accessors
    pub use crate::{VoArray, VoArrayCursor, VoBytes, VoString};
    pub use crate::{VoElem, VoStringElem};
    pub use crate::{VoMap, VoMapCursor, VoSlice, VoSliceCursor};
    // Pointer and closure accessors
    pub use crate::{VoClosure, VoPtr};
}
