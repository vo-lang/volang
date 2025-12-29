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

pub mod types;
pub mod symbol;
pub mod runtime_type;
pub mod instruction;
pub mod bytecode;
pub mod serialize;

pub use types::{
    ValueKind, ValueMeta, SlotType, MetaId, elem_flags,
    ELEM_FLAG_SIGN_BIT, ELEM_FLAG_FLOAT_BIT, ELEM_FLAG_BYTES_MASK,
    ELEM_FLAG_INT8, ELEM_FLAG_INT16, ELEM_FLAG_INT32, ELEM_FLAG_FLOAT32,
};
pub use symbol::Symbol;
#[cfg(feature = "std")]
pub use symbol::SymbolInterner;
pub use runtime_type::{RuntimeType, ChanDir, StructField, InterfaceMethod};
pub use instruction::{Instruction, Opcode};
pub use bytecode::{Module, FunctionDef, Constant, ExternDef, GlobalDef, StructMeta, InterfaceMeta, Itab};
