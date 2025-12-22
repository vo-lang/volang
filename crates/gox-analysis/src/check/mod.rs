//! Type checking implementation.
//!
//! This module contains the type checker that validates GoX source code
//! and produces type information for code generation.

mod assignment;
mod builtin;
mod call;
pub(crate) mod checker;
mod conversion;
mod decl;
pub mod errors;
mod expr;
pub(crate) mod format;
mod initorder;
mod interface;
mod label;
pub(crate) mod resolver;
mod returns;
mod stmt;
mod type_info;
mod typexpr;
mod util;

pub use checker::Checker;
pub use errors::TypeError;
pub use resolver::DeclInfo;
pub use type_info::{Initializer, TypeAndValue, TypeInfo};
