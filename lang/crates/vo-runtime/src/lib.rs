#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(not(feature = "std"))]
extern crate alloc;

extern crate self as vo_runtime;

/// Canonical extern-name encoder for hand-written provider tables.
pub use vo_ffi_macro::vo_extern_name;

/// Cooperative instruction budget shared by interpreter and native tiers.
///
/// The value keeps native and browser-hosted interpreter work preemptible while
/// avoiding repeated host scheduling for ordinary frame-sized workloads.
pub const EXECUTION_TIMESLICE_INSTRUCTIONS: u32 = 16_384;

pub mod output;
pub mod slot;

// Core runtime (no_std compatible)
pub mod gc;
pub mod gc_types;
pub mod island;
pub mod island_msg;
pub mod island_transport;
pub mod itab;
pub mod objects;
pub mod pack;
#[cfg(test)]
pub(crate) mod test_support;

// FFI core types (no_std compatible), registration requires std
pub mod ffi;

// Builtins (no_std compatible, linkme registration is std-only)
pub mod builtins;

// Std-only modules
#[cfg(feature = "std")]
pub mod ext_loader;
#[cfg(feature = "gc-debug")]
pub mod gc_debug;
pub mod host_services;
#[cfg(feature = "std")]
pub mod io;
#[cfg(feature = "std")]
pub mod jit_api;

// Re-exports from vo-common-core (no_std compatible)
pub use vo_common_core::bytecode::{
    Constant, ExternDef, ExternEffects, ExternJitRoute, FunctionDef, GlobalDef, InterfaceMeta,
    Itab, Module, ParamShape, ProviderTrust, RegisteredExternSource, ResolvedExtern,
    ResolvedExternAbi, ResolvedExternTable, ResolvedExternTableError, ReturnShape, StructMeta,
};
pub use vo_common_core::instruction::{Instruction, Opcode};
pub use vo_common_core::runtime_type::{ChanDir, InterfaceMethod, RuntimeType, StructField};
pub use vo_common_core::serialize;
pub use vo_common_core::symbol::Symbol;
#[cfg(feature = "std")]
pub use vo_common_core::symbol::SymbolInterner;
pub use vo_common_core::types::{
    MetaId, SlotType, ValueKind, ValueMeta, ValueRttid, INVALID_META_ID, META_ID_MASK,
};

// Re-export modules for downstream crates
pub use vo_common_core::bytecode;
pub use vo_common_core::instruction;
pub use vo_common_core::symbol;
pub use vo_common_core::types as core_types;

// Re-export InterfaceSlot from objects::interface (the canonical location)
pub use objects::interface::InterfaceSlot;

// Re-exports from ffi (core types always available)
pub use ffi::{
    ExternCallContext, ExternContractError, ExternEffectManifestEntry, ExternFn, ExternRegistry,
    ExternResult, SentinelErrorCache, WasmExtensionBridgeEntry, WasmExtensionOwner,
};
// Re-exports from ffi (std only - linkme registration and extension ABI types)
#[cfg(feature = "std")]
pub use ffi::{
    ExtensionTable, ExternEntry, ExternFnPtr, ExternModuleOwnerEntry, EXTERN_MODULE_OWNER_TABLE,
    EXTERN_TABLE,
};
#[cfg(feature = "std")]
#[doc(hidden)]
pub use linkme as __linkme;
#[cfg(feature = "std")]
pub use linkme::distributed_slice;

#[cfg(test)]
mod source_contract_tests {
    #[test]
    fn source_contract_tests_do_not_use_textual_cfg_test_truncation_062() {
        vo_source_contract::assert_no_textual_cfg_test_splits(env!("CARGO_MANIFEST_DIR"));
    }
}
