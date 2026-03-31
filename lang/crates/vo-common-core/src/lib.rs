//! # vo-common-core
//!
//! Core types for Vo that are `no_std` compatible.
//!
//! This crate provides foundational types used by the VM runtime:
//! - `ValueKind` - Runtime type classification
//! - `symbol` - Symbol type (no_std) and SymbolInterner (std feature)
//! - `runtime_type` - Runtime type representation for type identity
//! - `instruction` - Bytecode instruction format and opcodes
//! - `bytecode` - Module and function definitions

#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(not(feature = "std"))]
extern crate alloc;

pub mod bytecode;
pub mod debug_info;
pub mod instruction;
pub mod log_record;
pub mod runtime_type;
pub mod serialize;
pub mod source_provider;
pub mod symbol;
pub mod types;

pub use bytecode::{
    Constant, ExternDef, FunctionDef, GlobalDef, InterfaceMeta, Itab, Module, StructMeta,
    TransferType,
};
pub use debug_info::{DebugInfo, DebugLoc, FuncDebugInfo, SourceLoc};
pub use instruction::{Instruction, Opcode};
pub use log_record::LogRecordCore;
pub use runtime_type::{ChanDir, InterfaceMethod, RuntimeType, StructField};
pub use source_provider::{NoSource, SourceProvider};
pub use symbol::Symbol;
#[cfg(feature = "std")]
pub use symbol::SymbolInterner;
pub use types::{
    elem_flags, MetaId, SlotType, ValueKind, ValueMeta, ELEM_FLAG_BYTES_MASK, ELEM_FLAG_FLOAT32,
    ELEM_FLAG_FLOAT_BIT, ELEM_FLAG_INT16, ELEM_FLAG_INT32, ELEM_FLAG_INT8, ELEM_FLAG_SIGN_BIT,
};
