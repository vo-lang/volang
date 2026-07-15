//! Type checking implementation.
//!
//! This module contains the type checker that validates Vo source code
//! and produces type information for code generation.

mod assignment;
mod builtin;
mod call;
pub(crate) mod checker;
mod conversion;
mod decl;
mod dyn_protocol;
pub mod errors;
pub(crate) mod escape;
mod expr;
pub(crate) mod format;
pub(crate) mod go_island;
mod initorder;
mod interface;
mod label;
pub(crate) mod resolver;
mod returns;
pub mod sendable;
mod stmt;
pub mod type_info;
mod typexpr;
mod util;

/// Every runtime length is represented by the language's fixed-width `int`.
/// Target backends may impose an additional, narrower address-width limit.
const MAX_LANGUAGE_LEN: u64 = i64::MAX as u64;

/// Maximum recursive semantic dependency depth accepted by the checker.
///
/// Source syntax already has a matching structural bound. Separate package
/// declarations can still form much deeper dependency or interface-embedding
/// chains, so those recursive checker paths need their own deterministic
/// target limit.
const MAX_TYPE_CHECK_DEPTH: usize = 128;

/// Maximum number of concrete type-check diagnostics retained per package.
/// One final diagnostic records that later findings were suppressed.
const MAX_TYPE_CHECK_DIAGNOSTICS: usize = 256;

pub use checker::Checker;
pub use errors::TypeError;
pub use resolver::DeclInfo;
pub use type_info::{Initializer, TypeAndValue, TypeInfo};
