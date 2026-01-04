#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(not(feature = "std"))]
extern crate alloc;

// Runtime types
pub mod types;

// Runtime (std feature)
#[cfg(feature = "std")]
pub mod gc;
#[cfg(feature = "std")]
pub mod gc_types;
#[cfg(feature = "std")]
pub mod objects;
#[cfg(feature = "std")]
pub mod ffi;
#[cfg(feature = "std")]
pub mod builtins;
#[cfg(feature = "std")]
pub mod stdlib;
#[cfg(feature = "std")]
pub mod jit_api;
#[cfg(feature = "gc-debug")]
pub mod gc_debug;

// Re-exports from vo-common-core (no_std compatible)
pub use vo_common_core::types::{MetaId, SlotType, ValueKind, ValueMeta, META_ID_MASK, INVALID_META_ID};
pub use vo_common_core::symbol::Symbol;
#[cfg(feature = "std")]
pub use vo_common_core::symbol::SymbolInterner;
pub use vo_common_core::runtime_type::{RuntimeType, ChanDir, StructField, InterfaceMethod};
pub use vo_common_core::instruction::{Instruction, Opcode};
pub use vo_common_core::bytecode::{Module, FunctionDef, Constant, ExternDef, GlobalDef, StructMeta, InterfaceMeta, Itab};
pub use vo_common_core::serialize;

// Re-export modules for downstream crates
pub use vo_common_core::bytecode;
pub use vo_common_core::instruction;
pub use vo_common_core::types as core_types;
pub use vo_common_core::symbol;

// Re-exports from ffi (std only)
#[cfg(feature = "std")]
pub use ffi::{
    ExternCall, ExternCallContext, ExternFn, ExternFnWithContext, ExternRegistry, ExternResult,
    ExternEntry, ExternEntryWithContext, EXTERN_TABLE, EXTERN_TABLE_WITH_CONTEXT,
    lookup_extern, lookup_extern_with_context,
};
#[cfg(feature = "std")]
pub use linkme::distributed_slice;
