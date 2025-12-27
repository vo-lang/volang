//! # vo-common-core
//!
//! Core types for Vo that are `no_std` compatible.
//!
//! This crate provides foundational types used by the VM runtime:
//! - `ValueKind` - Runtime type classification
//! - `utf8` - UTF-8 decoding utilities
//! - `symbol` - Symbol type (no_std) and SymbolInterner (std feature)
//! - `runtime_type` - Runtime type representation for type identity

#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(not(feature = "std"))]
extern crate alloc;

pub mod types;
pub mod utf8;
pub mod symbol;
pub mod runtime_type;

pub use types::{ValueKind, ValueMeta, SlotType, MetaId};
pub use symbol::Symbol;
#[cfg(feature = "std")]
pub use symbol::SymbolInterner;
pub use runtime_type::{RuntimeType, ChanDir, StructField, InterfaceMethod};
