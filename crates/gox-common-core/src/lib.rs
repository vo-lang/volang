//! # gox-common-core
//!
//! Core types for GoX that are `no_std` compatible.
//!
//! This crate provides foundational types used by the VM runtime:
//! - `ValueKind` - Runtime type classification
//! - `utf8` - UTF-8 decoding utilities

#![cfg_attr(not(feature = "std"), no_std)]

mod types;
pub mod utf8;

pub use types::ValueKind;
